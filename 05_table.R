tab_pop_freq <- function() {
  count <- y_pop_freq(df_giver, df_receiver, df_receiver_65_74, df_receiver_75, df_need_help)
  df_pops <- tibble(pop_name, count) %>%
    mutate(percentage = count / sum(count))

  return(df_pops)
}


# Care receiver responses #####

tab_health_conditions <- function(df) {
  count <- y_health_condition(df)
  df_health_conditions <- tibble(health_conditions, count) %>%
    mutate(percentage = count / sum(count))

  return(df_health_conditions)
}

### Types of activities respondents received help with
tab_activity_receive_help <- function(df) {
  count <- y_activity_receive_help(df)
  df_activity_receive_help <- tibble(help_activities, count) %>%
    mutate(percentage = count / sum(count))

  return(df_activity_receive_help)
}

### Age of respondent's primary caregiver
tab_age_primary_giver <- function(df) {
  count <- y_age_primary_giver(df)
  df_output <- tibble(giver_age_group, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### Types of activities respondents received professional help with
tab_activity_receive_help_pro <- function(df) {
  count <- y_activity_receive_help_pro(df)
  df_output <- tibble(help_activities, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### Numbers of hours of help received - Per average week per activity
tab_hours_help_received <- function(df) {
  count <- y_hours_help_received(df)
  df_output <- tibble(help_hours, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### Distance between the respondent's and the caregiver's dwellings
tab_primary_giver_distance <- function(df) {
  count <- y_primary_giver_distance(df)
  df_output <- tibble(dwelling_distances, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### Primary caregiver helped with banking - Frequency
tab_receive_help_banking_freq <- function(df) {
  count <- y_receive_help_banking_freq(df)
  df_output <- tibble(primary_help_banking_freq, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### Primary caregiver helped with banking - Number of hours
tab_receive_help_banking_hours <- function(df) {
  count <- y_receive_help_banking_hours(df)
  df_output <- tibble(primary_help_banking_hours, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### How often and number of hours a respondent received help from with banking
### daily
tab_help_banking_hours_daily <- function(df) {
  count <- y_receive_help_banking_hours_freq(df, 1)
  df_output <- tibble(primary_help_banking_hours, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### at least once a week
tab_help_banking_hours_weekly <- function(df) {
  count <- y_receive_help_banking_hours_freq(df, 2)
  df_output <- tibble(primary_help_banking_hours, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

### monthly
tab_help_banking_hours_monthly <- function(df) {
  count <- y_receive_help_banking_hours_freq(df, 3)
  df_output <- tibble(primary_help_banking_hours, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}

# less than monthly
tab_help_banking_hours_monthly_less <- function(df) {
  count <- y_receive_help_banking_hours_freq(df, 4)
  df_output <- tibble(primary_help_banking_hours, count) %>%
    mutate(percentage = count / sum(count))

  return(df_output)
}


# giver tables

tab_activity_give_help <- function(df) {
  count <- y_activity_give_help(df)
  df_output<- tibble(help_activities, count) %>%
    mutate(percentage = count / sum(count))
  
  return(df_output)
}