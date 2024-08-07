# Match rainbowR buddies and send emails

# packages ----------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(readxl)
library(blastula)
library(glue)

# get form data -----------------------------------------------------------

# Will need to update URL for each round
url <- "https://docs.google.com/spreadsheets/d/1eT_3yFdPrYOSE7a_zrtSkzWMYHGZT32AsiVToJmWY1g/edit?resourcekey#gid=1925000112"
buddy_form <- read_sheet(url)

View(buddy_form)

buddy_df <- buddy_form |>
  select(first_name = `First name`,
         last_name = `Last name`, 
         pronouns = `Pronouns`,
         email = `Email`,
         about = starts_with("About"),
         interests = contains("interest"),
         times = contains("mutually"))

View(buddy_df)

# message is buddy_df has an odd or even number of rows
if (nrow(buddy_df) %% 2 == 1) {
  message("buddy_df has an odd number of rows")
} else {
  message("buddy_df has an even number of rows")
}


# deal with odd number of sign-ups ----------------------------------------
# I can have 0, 1 or 2 buddies, depending on number of sign-ups and my capacity to take part

Ella_Kaye_row <- buddy_df |> 
  filter(first_name == "Ella" & last_name == "Kaye") |> 
  nrow()

# Check Ella Kaye is in buddy_df
if (!Ella_Kaye_row) {
  message("Ella Kaye is not in buddy_df")
}

# If buddy_df has an odd number of rows and Ella Kaye is in buddy_df, stop
if ((nrow(buddy_df) %% 2 == 1) && Ella_Kaye_row) {
  stop("Either filter out Ella Kaye or add a second Ella Kaye entry from a different email.")
}

# If filtering Ella Kaye out, uncomment the code below:
# if ((nrow(buddy_df) %% 2 == 1) && Ella_Kaye_row) {
#  buddy_df <- buddy_df |>
#    filter(!(first_name == "Ella" & last_name == "Kaye"))
# }

# If buddy_df has an odd number of rows and Ella Kaye is not in buddy_df, stop
if ((nrow(buddy_df) %% 2 == 1) && !Ella_Kaye_row) {
  stop("Add Ella Kaye via the Google Form then rerun script from top.")
}

# match buddies -----------------------------------------------------------

# write a function make_buddy_pairs that takes buddy_df and seed with default seed as an argument
make_buddy_pairs <- function(buddy_df, seed = 1) {
  
  # set the seed to seed
  set.seed(seed)
  
  # create a vector pairs which repeats each of the numbers from 1
  # to half the number in buddy_df and shuffles them (assume even number)
  pairs <- sample(rep(1:(nrow(buddy_df)/2), 2))
  
  # add pairs to buddy_df
  buddy_df <- buddy_df |>
    mutate(pair = pairs) |> 
    arrange(pair)
  
  # from buddy_df create a tibble buddy_pairs with half the rows of buddy_df 
  # with a column called pair with the numbers 1 to half the number of rows in buddy_df
  # a column called buddy1 with the first email associated with pair 
  # and a column called buddy2 with the second email associated with pair
  buddy_pairs <- buddy_df |>
    group_by(pair) |>
    summarise(buddy1 = email[1],
              buddy2 = email[2])
  
  # in buddy_pairs, create first_buddy which is the lesser of buddy1 and buddy2
  # and create second_buddy which is the greater of buddy1 and buddy2
  # then select first_buddy and second_buddy
  # this will make it easier to compare buddy_pairs with previous_buddy_pairs later
  buddy_pairs <- buddy_pairs |>
    mutate(first_buddy = pmin(buddy1, buddy2),
           second_buddy = pmax(buddy1, buddy2)) |>
    select(first_buddy, second_buddy)
  
  # return buddy_pairs and buddy_df
  return(list(buddy_pairs = buddy_pairs,
              buddy_df = buddy_df))
}

# a function make_buddies that takes buddy_df, avoid = NULL and seed = 1 as arguments
# avoid is a tibble of email pairs to avoid
# it is primarily intended to be used to avoid pairing two entries for the same person (esp. Ella Kaye)
# it can also be used if participants specifically request to avoid a pairing
make_buddies <- function(buddy_df, avoid = NULL, seed = 1) {
  
  # sanity check if buddy_df has an odd number of rows,
  # N.B. this shouldn't happen, if the script above has run properly
  if(nrow(buddy_df) %% 2 == 1) {
    stop("buddy_df must have an even number of rows")
  }
  
  # call make_buddy_pairs with buddy_df and seed
  # and save buddy_pairs as buddy_pairs and buddy_df as buddy_df
  buddies <- make_buddy_pairs(buddy_df, seed)
  buddy_pairs <- buddies$buddy_pairs
  buddy_df <- buddies$buddy_df
  
  # if previous_buddy_pairs.csv exists, read it into previous_buddy_pairs
  # if not, create previous_buddy_pairs as an empty tibble with col_names first_buddy and second_buddy
  if(file.exists("previous_buddy_pairs.csv")) {
    previous_buddy_pairs <- read_csv("previous_buddy_pairs.csv")
  } else {
    previous_buddy_pairs <- tibble(first_buddy = character(),
                                   second_buddy = character())
  }
  
  # if !is.null(avoid), add avoid to previous_buddy_pairs
  if(!is.null(avoid)) {
    avoid_pairs <- previous_buddy_pairs |>
      bind_rows(avoid)
  } else {
    avoid_pairs <- previous_buddy_pairs
  }
  
  # while any of the rows in buddy_pairs are in avoid_pairs
  # increment seed by 1 and run make_buddy_pairs again, updating buddy_pairs and buddy_df
  while(any(apply(buddy_pairs, 1, function(x) paste(x, collapse = " ")) %in% apply(avoid_pairs, 1, function(x) paste(x, collapse = " ")))) {
    seed <- seed + 1
    updated_buddies <- make_buddy_pairs(buddy_df, seed)
    buddy_pairs <- updated_buddies$buddy_pairs
    buddy_df <- updated_buddies$buddy_df
  }
    
  # create updated_buddy_pairs by binding previous_buddy_pairs and buddy_pairs
  updated_buddy_pairs <- bind_rows(previous_buddy_pairs, buddy_pairs)
  # write updated_buddy_pairs to previous_buddy_pairs.csv
  write_csv(updated_buddy_pairs, "previous_buddy_pairs.csv")
  
  # write buddy_pairs to YYYY-MM_buddy_pairs.csv
  # where YYYY is the current year and MM is the current month
  write_csv(buddy_pairs, paste0(format(Sys.Date(), "%Y-%m"), "_buddy_pairs.csv"))
  
  # write buddy_df to YYYY-MM_buddy_df.csv
  # where YYYY is the current year and MM is the current month
  write_csv(buddy_df, paste0(format(Sys.Date(), "%Y-%m"), "_buddy_df.csv"))
  
  # return a list with buddy_pairs, buddy_df, and seed
  return(list(buddy_pairs = buddy_pairs,
              buddy_df = buddy_df,
              final_seed = seed))
}

# read in avoid.csv (N.B. in .gitignore)
avoid <- read_csv("avoid.csv")

buddies <- make_buddies(buddy_df, avoid = avoid, seed = 1)
buddy_df <- buddies$buddy_df
buddy_pairs <- buddies$buddy_pairs
buddy_pairs


# prepare data for emailing -----------------------------------------------

buddies_for_email <- buddy_df |>
  group_by(pair) |>
  summarise(first_name1 = first_name[1],
            first_name2 = first_name[2],
            last_name1 = last_name[1],
            last_name2 = last_name[2],
            pronouns1 = pronouns[1],
            pronouns2 = pronouns[2],
            email1 = email[1],
            email2 = email[2],
            about1 = about[1],
            about2 = about[2],
            interests1 = interests[1],
            interests2 = interests[2],
            times1 = times[1],
            times2 = times[2])

View(buddies_for_email)

write_csv(buddies_for_email, "buddies_for_email.csv") 

# send emails -------------------------------------------------------------

# test
buddies_for_email_test <- read_csv("buddies_email_test.csv")
View(buddies_for_email_test)

# Adapted from example in https://thecoatlessprofessor.com/programming/r/sending-an-email-from-r-with-blastula-to-groups-of-students/

buddies_email_template = function(buddies) {
  
  # Construct the e-mail for the buddies.
  buddies |> 
    glue_data(
      "Hello {first_name1} {last_name1} and {first_name2} {last_name2},\n\n\n\n",
      "You are now rainbowR 🌈👯 buddies! \n\n\n\n",
      "Thank you both for signing up! We hope you enjoy connecting with each other. 
      For a reminder about what the buddies scheme is and how it works,
      see the blog post at <https://rainbowr.netlify.app/introducing-buddies>.
      \n\n\n\n",
      "**About {first_name1}**\n\nPronouns: {pronouns1}.\n\n\n\n{about1} \n\n\n\n",
      "**About {first_name2}**\n\nPronouns: {pronouns2}.\n\n\n\n{about2} \n\n\n\n",
      "**{first_name1}'s interests**\n\n{interests1} \n\n\n\n",
      "**{first_name2}'s interests**\n\n{interests2} \n\n\n\n",
      "**{first_name1}'s time/timezone considerations**\n\n{times1} \n\n\n\n",
      "**{first_name2}'s time/timezone considerations**\n\n{times2} \n\n\n\n",
      "You can contact each other at [{email1}](mailto:{email1}) and [{email2}](mailto:{email2}).\n\n\n\n",
      "Over to you! \n\n\n\n"
    )  |> 
    md()  |> 
    compose_email()
}

# test
for (i in seq_len(nrow(buddies_for_email_test))) {
  # Retrieve current buddies
  buddy_pair <- buddies_for_email_test[i, ] 
  
  # get email addresses
  to <- c(buddy_pair$email1, buddy_pair$email2)
  
  # Construct the e-mail using our custom template.
  email_contents <- buddies_email_template(buddy_pair)
  
  # Send e-mail
  email_contents %>%
    smtp_send(
      from = "hello@rainbowr.org",
      to = to,
      subject = "Your new rainbowR buddy!",
      credentials = creds_key(id = "rainbowr")
    )
}

# create and send the emails for real!
for (i in seq_len(nrow(buddies_for_email))) {
  # Retrieve current buddies
  buddy_pair <- buddies_for_email[i, ] 
  
  # get email addresses
  to <- c(buddy_pair$email1, buddy_pair$email2)
  
  # Construct the e-mail using our custom template.
  email_contents <- buddies_email_template(buddy_pair)
  
  # Send e-mail
  email_contents %>%
    smtp_send(
      from = "hello@rainbowr.org",
      to = to,
      subject = "Your new rainbowR buddy!",
      credentials = creds_key(id = "rainbowr")
    )
}
