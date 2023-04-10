# tab_helper(): returns a data frame adding sex counts and percentage columns
# df (tibble): data frame to be transformed
# count (vector): y-axis values (quantitative)
# x_options (vector): x-axis values (categorical)
# cols (string): primary filter variable
# cols2 (string = NULL): secondary filter variable
# response_code (integer): mapped value to cols2
tab_helper <- function(df, count, x_options, cols, col2 = NULL, response_code) {
  start <- x_options[1]
  end <- x_options[length(x_options)]
  total_male <- sum(df$SEX == 1)
  total_female <- sum(df$SEX == 2)

  tibble(x_options = names(x_options), count) %>%
    mutate(percentage = count / sum(count),
           Male = sapply(start:end, function(i) {
             if (!is.null(col2)) {
               sum(df$SEX == 1 & df[[cols]] == i & df[[col2]] == response_code)
             } else {
               sum(df$SEX == 1 & df[[cols]] == i)
             }
           }),
           Female = sapply(start:end, function(i) {
             if (!is.null(col2)) {
               sum(df$SEX == 2 & df[[cols]] == i & df[[col2]] == response_code)
             } else {
               sum(df$SEX == 2 & df[[cols]] == i)
             }
           }),
           male_percentage = round(Male/total_male, 2),
           female_percentage = round(Female/total_female, 2),
    )
}

# tab_helper_multi_var(): returns a data frame where x values are consists of multiple variables adding sex counts and
# percentage columns
# df (tibble): data frame to be transformed
# count (vector): y-axis values (quantitative)
# x_options (vector): x-axis values (categorical)
# cols (vector): primary filter variables where each value in x_options is mapped to a different column
tab_helper_multi_var <- function(df, count, x_options, cols) {
  total_male <- sum(df$SEX == 1)
  total_female <- sum(df$SEX == 2)

  tibble(x_options, count) %>%
    mutate(percentage = count / sum(count),
           Male = sapply(seq_along(x_options), function(i) {
             sum(df$SEX == 1 & df[[cols[i]]] == 1)
           }),
           Female = sapply(seq_along(x_options), function(i) {
             sum(df$SEX == 2 & df[[cols[i]]] == 1)
           }),
           male_percentage = round(Male/total_male, 2),
           female_percentage = round(Female/total_female, 2),
    )
}

# General data ####
tab_pop_freq <- function() {
  count <- y_pop_freq(df_giver, df_receiver, df_receiver_65_74, df_receiver_75, df_need_help)
  df_pops <- tibble(pop_name, count) %>%
    mutate(percentage = count / sum(count))

  return(df_pops)
}


# Care receiver responses #####

tab_health_conditions <- function(df) {
  # count <- y_health_condition(df)
  count <- count_map(df, health_conditions, "PRA_10GR")
  x_options <- health_conditions
  cols <- "PRA_10GR"

  df_output <- tab_helper(df, count, x_options, cols) %>%
    rename(health_conditions = x_options)

  return(df_output)
}



### Types of activities respondents received help with
tab_activity_receive_help <- function(df) {
  count <- y_activity_receive_help(df)
  x_options <- help_activities
  cols <- help_activity_codes

  df_output <- tab_helper_multi_var(df, count, x_options, cols) %>%
    rename(help_activities = x_options)

  return(df_output)
}



### Age of respondent's primary caregiver
tab_age_primary_giver <- function(df) {
  count <- y_age_primary_giver(df)

  df_output <- tab_helper(df, count, giver_age_group, "CRGVAGGR") %>%
    rename(giver_age_group = x_options)

  return(df_output)
}


### Types of activities respondents received professional help with
tab_activity_receive_help_pro <- function(df) {
  count <- y_activity_receive_help_pro(df)

  df_output <- tab_helper_multi_var(df, count, help_activities, help_activity_pro_codes) %>%
    rename(help_activities = x_options)

  return(df_output)
}

### Numbers of hours of help received - Per average week per activity
tab_hours_help_received <- function(df) {
  count <- y_hours_help_received(df)

  df_output <- tab_helper(df, count, help_hours,"HAR_10C") %>%
    rename(help_hours = x_options)

  return(df_output)
}

### Distance between the respondent's and the caregiver's dwellings
tab_primary_giver_distance <- function(df) {
  count <- y_primary_giver_distance(df)

  df_output <- tab_helper(df, count, dwelling_distances,"PGD_10") %>%
    rename(dwelling_distances = x_options)

  return(df_output)
}

### Primary caregiver helped with banking - Frequency
tab_receive_help_banking_freq <- function(df) {
  count <- y_receive_help_banking_freq(df)

  df_output <- tab_helper(df, count, primary_help_banking_freq,"AGB_20") %>%
    rename(primary_help_banking_freq = x_options)

  return(df_output)
}

### Primary caregiver helped with banking - Number of hours
tab_receive_help_banking_hours <- function(df) {
  count <- y_receive_help_banking_hours(df)

  df_output <- tab_helper(df, count, primary_help_banking_hours,"AGB_30C") %>%
    rename(primary_help_banking_hours = x_options)

  return(df_output)
}

### How often and number of hours a respondent received help from with banking
### daily
tab_help_banking_hours_daily <- function(df) {
  response_code <- 1
  count <- y_receive_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours,"AGB_30C", "AGB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)

  return(df_output)
}

### at least once a week
tab_help_banking_hours_weekly <- function(df) {
  response_code <- 2
  count <- y_receive_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours,"AGB_30C", "AGB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)
  return(df_output)
}

### monthly
tab_help_banking_hours_monthly <- function(df) {
  response_code <- 3
  count <- y_receive_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours,"AGB_30C", "AGB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)
  return(df_output)
}

# less than monthly
tab_help_banking_hours_monthly_less <- function(df) {
  response_code <- 4
  count <- y_receive_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours,"AGB_30C", "AGB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)
  return(df_output)
}


# giver tables ####

tab_activity_give_help <- function(df) {
  count <- y_activity_give_help(df)

  df_output <- tab_helper_multi_var(df, count, help_activities, activity_give_help_codes) %>%
    rename(help_activities = x_options)

  return(df_output)
}

tab_age_primary_receiver <- function(df) {
  count <- y_age_primary_receiver(df)

  df_output <- tab_helper(df, count, primary_receiver_age_group, "CRRCPAGR") %>%
    rename(primary_receiver_age_group = x_options)

  return(df_output)
}

tab_hours_help_provided <- function(df) {
  count <- y_hours_help_provided(df)

  df_output <- tab_helper(df, count, help_hours, "HAP_10C") %>%
    rename(help_hours = x_options)

  return(df_output)
}

tab_primary_receiver_distance <- function(df) {
  count <- y_primary_receiver_distance(df)

  df_output <- tab_helper(df, count, dwelling_distances, "PRD_10") %>%
    rename(dwelling_distances = x_options)

  return(df_output)
}

tab_give_help_banking_freq <- function(df) {
  count <- y_give_help_banking_freq(df)

  df_output <- tab_helper(df, count, primary_help_banking_freq, "AGB_20") %>%
    rename(primary_help_banking_freq = x_options)

  return(df_output)
}

tab_give_help_banking_hours <- function(df) {
  count <- y_give_help_banking_hours(df)

  df_output <- tab_helper(df, count, primary_help_banking_hours, "ARB_30C") %>%
    rename(primary_help_banking_hours = x_options)

  return(df_output)
}

tab_give_help_banking_daily <- function(df) {
  response_code <- 1
  count <- y_give_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours, "ARB_30C", "ARB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)
  return(df_output)
}

tab_give_help_banking_weekly <- function(df) {
  response_code <- 2
  count <- y_give_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours, "ARB_30C", "ARB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)
  return(df_output)
}

tab_give_help_banking_monthly <- function(df) {
  response_code <- 3
  count <- y_give_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours, "ARB_30C", "ARB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)
  return(df_output)
}

tab_give_help_banking_monthly_less <- function(df) {
  response_code <- 4
  count <- y_give_help_banking_hours_freq(df, response_code)

  df_output <- tab_helper(df, count, primary_help_banking_hours, "ARB_30C", "ARB_20", response_code) %>%
    rename(primary_help_banking_hours = x_options)
  return(df_output)
}

tab_out_of_pocket <- function(df) {
  count <- y_out_of_pocket(df)

  df_output <- tab_helper_multi_var(df, count, out_of_pocket_expenses, out_of_pocket_codes) %>%
    rename(out_of_pocket_expenses = x_options)
  return(df_output)
}

tab_financial_hardship <- function(df) {
  count <- y_financial_hardship(df)

  total_male <- sum(df$SEX == 1)
  total_female <- sum(df$SEX == 2)
  x_options <- financial_hardship
  cols <- financial_hardship_codes
  cols2 <- "CRRCPAGR"

  df_output <- tibble(x_options, count) %>%
    mutate(percentage = count / sum(count),
           Male = sapply(seq_along(x_options), function(i) {
             sum(df$SEX == 1 & df[[cols[i]]] == 1 & (df[[cols2]] >= 14 & df[[cols2]] <= 20))
           }),
           Female = sapply(seq_along(x_options), function(i) {
             sum(df$SEX == 2 & df[[cols[i]]] == 1 & (df[[cols2]] >= 14 & df[[cols2]] <= 20))
           }),
           male_percentage = round(Male/total_male, 2),
           female_percentage = round(Female/total_female, 2),
    ) %>%
    rename(financial_hardship = x_options)

  return(df_output)
}
