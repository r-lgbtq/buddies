library(tidyverse)
library(googlesheets4)

url <- "https://docs.google.com/spreadsheets/d/1y74Ti54UA5nxtgMt8S-MyalOiqJHWSUOztlrr4ZUKls"
buddy_form <- read_sheet(url)

buddy_df <- buddy_form |>
  select(first_name = `First name`,
         last_name = `Last name`, 
         email = `e-mail`,
         about = starts_with("About"),
         interests = contains("interest"))

# create a tibble called extra_buddy with first_name = rainbow, last_name = R, email = rlgbtq@gmail.com, about = rainbow, interests = unicorns
extra_buddy <- tibble(first_name = "rainbow",
                last_name = "R",
                email = "rlgbtq@gmail.com",
                about = "rainbow",
                interests = "unicorns")

# write a function make_buddy_pairs that takes buddy_df and seed with default seed as an argument
make_buddy_pairs <- function(buddy_df, seed = 1) {
  
  # set the seed to seed
  set.seed(seed)
  
  # create a vector pairs which repeats each of the numbers from 1
  # to half the number in buddy_df and shuffles them (assume even number)
  pairs <- sample(rep(1:(nrow(buddy_df)/2), 2))
  
  # add pairs to buddy_df
  buddy_df <- buddy_df |>
    mutate(pair = pairs)
  
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
  buddy_pairs <- buddy_pairs |>
    mutate(first_buddy = pmin(buddy1, buddy2),
           second_buddy = pmax(buddy1, buddy2)) |>
    select(first_buddy, second_buddy)
  
  # return buddy_pairs and buddy_df
  return(list(buddy_pairs = buddy_pairs,
              buddy_df = buddy_df))
}

# a function make_buddies that takes buddy_df, extra_buddy with default extra_buddy and seed = 1 as arguments
make_buddies <- function(buddy_df, extra_buddy = extra_buddy, seed = 1) {
  
  # if buddy_df has an odd number of rows, add extra_buddy to buddy_df
  if(nrow(buddy_df) %% 2 == 1) {
    buddy_df <- bind_rows(buddy_df, extra_buddy)
  }
  
  # call make_buddy_pairs with buddy_df and seed
  # and save buddy_pairs as buddy_pairs and buddy_df as buddy_df
  buddies <- make_buddy_pairs(buddy_df, seed)
  buddy_pairs <- buddies$buddy_pairs
  buddy_df <- buddies$buddy_df
  
  # if previous_buddy_pairs.csv exists, read it into previous_buddy_pairs
  if(file.exists("previous_buddy_pairs.csv")) {
    previous_buddy_pairs <- read_csv("previous_buddy_pairs.csv")
    
    # while any of the rows in buddy_pairs are in previous_buddy_pairs
    # increment seed by 1 and run make_buddy_pairs again, updating buddy_pairs and buddy_df
    while(any(apply(buddy_pairs, 1, function(x) paste(x, collapse = " ")) %in% apply(previous_buddy_pairs, 1, function(x) paste(x, collapse = " ")))) {
      seed <- seed + 1
      updated_buddies <- make_buddy_pairs(buddy_df, seed)
      buddy_pairs <- updated_buddies$buddy_pairs
      buddy_df <- updated_buddies$buddy_df
    }
    
    # created updated_buddy_pairs by binding previous_buddy_pairs and buddy_pairs
    updated_buddy_pairs <- bind_rows(previous_buddy_pairs, buddy_pairs)
    # write updated_buddy_pairs to previous_buddy_pairs.csv
    write_csv(updated_buddy_pairs, "previous_buddy_pairs.csv")
  } else {
    # if previous_buddy_pairs.csv does not exist, write buddy_pairs to previous_buddy_pairs.csv
    write_csv(buddy_pairs, "previous_buddy_pairs.csv")
  }
  
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

buddies <- make_buddies(buddy_df, extra_buddy, 1)$buddy_df

buddies


