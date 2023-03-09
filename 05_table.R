# Care receiver responses #####

tab_health_conditions <- function(df) {
  health_conditions_freq <- y_health_condition(df)
  df_health_conditions <- tibble(health_conditions, health_conditions_freq)
  
  return(df_health_conditions)
}

### Types of activities respondents received help with
tab_activity_receive_help <- function(df) {
  activity_receive_help_freq <- y_activity_receive_help(df)
  df_activity_receive_help <- tibble(activity_receive_help, activity_receive_help_freq)
  
  return(df_activity_receive_help)
}

### Age of respondent's primary caregiver
tab_age_primary_giver <- function(df) {
  age_primary_giver_freq <- y_age_primary_giver(df)
  df_output <- tibble(giver_age_group, age_primary_giver_freq)
  
  return(df_output)
}

### Types of activities respondents received professional help with
tab_activity_receive_help_pro<- function(df) {
  activity_receive_help_pro_freq <- y_activity_receive_help_pro(df)
  df_output <- tibble(activity_receive_help, activity_receive_help_pro_freq)
  
  return(df_output)
}

### Numbers of hours of help received - Per average week per activity
tab_hours_help_received <- function(df) {
  hours_help_received_freq <- y_hours_help_received(df)
  df_output <- tibble(hours_help_received, hours_help_received_freq)
  return(df_output)
}

### Distance between the respondent's and the caregiver's dwellings
tab_primary_giver_distance <- function(df) {
  primary_giver_distance_freq <- y_primary_giver_distance(df)
  df_output <- tibble(primary_giver_distance, primary_giver_distance_freq)
  return(df_output)
}

### Primary caregiver helped with banking - Frequency
tab_receive_help_banking_freq<- function(df) {
  receive_help_banking_freq <- y_receive_help_banking_freq(df)
  df_output <- tibble(primary_help_banking_freq, receive_help_banking_freq)
  return(df_output)
}

### Primary caregiver helped with banking - Number of hours
tab_receive_help_banking_hours <- function(df) {
  receive_help_banking_hours_freq <- y_receive_help_banking_hours(df)
  df_output <- tibble(primary_help_banking_hours, receive_help_banking_hours_freq)
  return(df_output)
}

### How often and number of hours a respondent received help from with banking
### daily
tab_help_banking_hours_daily <- function(df) {
  help_banking_hours_daily_freq <- y_receive_help_banking_hours_freq(df, 1)
  df_output <- tibble(primary_help_banking_hours, help_banking_hours_daily_freq)
  return(df_output)
}

### at least once a week
tab_help_banking_hours_weekly <- function(df) {
  help_banking_hours_freq <- y_receive_help_banking_hours_freq(df, 2)
  df_output <- tibble(primary_help_banking_hours, help_banking_hours_freq)
  return(df_output)
}

### monthly
tab_help_banking_hours_monthly <- function(df) {
  help_banking_hours_freq <- y_receive_help_banking_hours_freq(df, 3)
  df_output <- tibble(primary_help_banking_hours, help_banking_hours_freq) 
  return(df_output)
}

# less than monthly
tab_help_banking_hours_monthly_less <- function(df) {
  help_banking_hours_freq <- y_receive_help_banking_hours_freq(df, 4)
  df_output <- tibble(primary_help_banking_hours, help_banking_hours_freq)
  return(df_output)
}






