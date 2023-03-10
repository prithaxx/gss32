library(viridis)
source("01_main.R")
source("02_var_x.R")
source("03_var_y.R")
source("04_general_df.R")
source("05_table.R")
source("06_percentage.R")
# General Charts ####

## Respondent groups ####
c_respondent_groups <-
  ggplot(
    data = df_pops,
    mapping = aes(x = fct_inorder(pop_name), y = pop_freq, fill = pop_name)
  ) +
  geom_col() +
  geom_text(aes(label = pop_freq), position = position_stack(vjust = 0.5)) +
  ggtitle("GSS 2018 repsondent groups") +
  labs(caption = str_wrap("Count for respondent groupings: caregiver, care receivers 65 years and over, care receivers 65 to 74 years, care receivers 75 years and over, care receiver and caregiver, and unmet needs for GSS 2018.", width = 115)) +
  xlab("Respondent group") +
  ylab("Count") +
  scale_x_discrete(labels = str_wrap(df_pops$pop_name, width = 15)) +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  theme(plot.caption = element_text(hjust = 0))

## Sex of primary caregiver and primary care receiver ####
c_primary_sex <- ggplot(data = df_primary_sex, mapping = aes(x = sex, y = freq, fill = sex)) +
  geom_col() +
  geom_text(aes(label = freq), position = position_stack(vjust = 0.5)) +
  ggtitle("Primary Care Giver and Receiver by Sex (age 65+)") +
  labs(caption = str_wrap("Top row. Sex count for individuals that received care and are 65 years of age or older from respondents considered to be a caregiver. Bottom row. Sex count of individuals who provided care to respondents considered to be a care receiver that are 65 years of age or older", width = 115)) +
  xlab("Sex") +
  ylab("Count") +
  facet_wrap(~type, ncol = 1) +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  theme(plot.caption = element_text(hjust = 0))


# Care receiver responses #####
chart_health_conditions <- function(df_receiver) {
  df_health_conditions <- tab_health_conditions(df_receiver)

  c_health_conditions <- ggplot(data = df_health_conditions, mapping = aes(
    x = fct_inorder(health_conditions), y =
      count, fill = health_conditions
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    labs(caption = str_wrap("Count for main health conditions for which respondents considered to be a care receiver and 65 years of age or older received help.", width = 115)) +
    xlab("Health Condition") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_health_conditions$health_conditions, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_health_conditions)
}


### Types of activities respondents received help with
chart_activity_receive_help <- function(df_receiver) {
  df_activity_receive_help <- tab_activity_receive_help(df_receiver)

  c_activity_receive_help <- ggplot(data = df_activity_receive_help, mapping = aes(
    x = fct_inorder(activity_receive_help), y =
      count, fill = activity_receive_help
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received with from family, friends or neighbours in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_receive_help$activity_receive_help, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_activity_receive_help)
}

### Age of respondent's primary caregiver
chart_age_primary_giver <- function(df_receiver, CRGVAGGR) {
  df_age_primary_giver <- tab_age_primary_giver(df_receiver)
  
  c_age_primary_giver <- ggplot(data = df_age_primary_giver, mapping = aes(x = fct_inorder(giver_age_group), y =  age_primary_giver_freq, fill = giver_age_group)) +
    geom_col() +
    geom_text(aes(label =  age_primary_giver_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Age of respondent's primary caregivers") +
    labs(caption = str_wrap("Count of the age (groups of 5) of primary caregivers for respondents considered to be a care receiver and 65 years of age or older.", width = 120)) +
    xlab("Age (years)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))
  return(c_age_primary_giver)
}

### Types of activities respondents received professional help with
chart_activity_receive_help_pro <- function(df_receiver) {
  df_activity_receive_help_pro <- tab_activity_receive_help_pro(df_receiver)
  
  c_activity_receive_help_pro <- ggplot(data = df_activity_receive_help_pro, mapping = aes(
    x = fct_inorder(activity_receive_help), y =
      activity_receive_help_pro_freq, fill = activity_receive_help
  )) +
    geom_col() +
    geom_text(aes(label = activity_receive_help_pro_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Activities received professional help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received help from a professional in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_receive_help_pro$activity_receive_help, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))
  return(c_activity_receive_help_pro)
}


### Numbers of hours of help received - Per average week per activity
chart_hours_help_received <- function(df_receiver, HAR_10C) {
  df_hours_help_received <- tab_hours_help_received(df_receiver)

  c_hours_help_received <- ggplot(data = df_hours_help_received, mapping = aes(
    x = hours_help_received, y = hours_help_received_freq, fill =
      hours_help_received
  )) +
    geom_col() +
    geom_text(aes(label = hours_help_received_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Numbers of hours of help received - Per average week per activity") +
    labs(caption = str_wrap("Count for the number of hours of help received, per average week per activity (transportation, house maintenance, household chores, scheduling, banking, medical treatment, personal care, other) for respondents considered to be a care receiver and 65 years of age or older from family, friends or neighbours in the past 12 months.", width = 115)) +
    xlab("Time (hour)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_hours_help_received)
}

### Distance between the respondent's and the caregiver's dwellings
chart_primary_giver_distance <- function(df_receiver) {
  df_primary_giver_distance <- tab_primary_giver_distance(df_receiver)
  
  c_primary_giver_distance <- ggplot(data = df_primary_giver_distance, mapping = aes(
    x = fct_inorder(primary_giver_distance), y =
      primary_giver_distance_freq, fill = primary_giver_distance
  )) +
    geom_col() +
    geom_text(aes(label = primary_giver_distance_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Distance between the respondent's and caregiver's dwellings") +
    labs(caption = str_wrap("Counts for the distance by car between respondents considered to be a care receiver and 65 years of age or older, and their primary caregiver during the time they were receiving help in the past 12 months.", width = 115)) +
    xlab("Distance (time)") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_primary_giver_distance$primary_giver_distance, width = 13)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_primary_giver_distance)
}

### Primary caregiver helped with banking - Frequency
chart_receive_help_banking_freq <- function(df_receiver) {
  df_receive_help_banking_freq <- tab_receive_help_banking_freq(df_receiver)
  
  c_receive_help_banking_freq <- ggplot(data = df_receive_help_banking_freq, mapping = aes(
    x = fct_inorder(primary_help_banking_freq ), y = receive_help_banking_freq, fill =
      primary_help_banking_freq 
  )) +
    geom_col() +
    geom_text(aes(label = receive_help_banking_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Primary caregiver helped with banking - Frequency") +
    labs(caption = str_wrap("Count for how often respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.", width = 120)) +
    xlab("Help Frequency") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_receive_help_banking_freq)
}

### Primary caregiver helped with banking - Number of hours
chart_receive_help_banking_hours <- function(df_receiver, AGB_30C) {
  df_receive_help_banking_hours <- tab_receive_help_banking_hours(df_receiver)
  
  c_receive_help_banking_hours <- ggplot(data = df_receive_help_banking_hours, mapping = aes(
    x = primary_help_banking_hours, y =
      receive_help_banking_hours_freq, fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = receive_help_banking_hours_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Primary caregiver helped with banking - Number of hours") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_receive_help_banking_hours)
}

### The following 4 charts show How often and number of hours a respondent received help from with banking

### daily
chart_help_banking_hours_daily <- function(df_receiver) {
  df_help_banking_hours_freq <- tab_help_banking_hours_daily(df_receiver)

  c_help_banking_hours_daily <- ggplot(data = df_help_banking_hours_freq, mapping = aes(
    x = primary_help_banking_hours, y =
      help_banking_hours_daily_freq, fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_hours_daily_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Hours primary caregiver helped with banking - Daily") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances daily", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_help_banking_hours_daily)
}

### at least once a week
chart_help_banking_weekly <- function(df_receiver) {
  df_help_banking_hours_weekly <- tab_help_banking_hours_weekly(df_receiver)

  c_help_banking_weekly <- ggplot(data = df_help_banking_hours_weekly, mapping = aes(
    x = primary_help_banking_hours, y =
      help_banking_hours_freq, fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_hours_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Hours primary caregiver helped with banking - At least once a week") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances at least once a week", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_help_banking_weekly)
}

### monthly

chart_help_banking_monthly <- function(df_receiver) {
  df_help_banking_hours_monthly <- tab_help_banking_hours_monthly(df_receiver)
  
  c_help_banking_monthly <- ggplot(data = df_help_banking_hours_monthly, mapping = aes(
    x = primary_help_banking_hours, y =
      help_banking_hours_freq, fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_hours_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Hours primary caregiver helped with banking - At least once a month") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances at least once a month", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_help_banking_monthly)
}

# less than monthly
chart_help_banking_monthly_less <- function(df_receiver, AGB_30C, AGB_20) {
  df_help_banking_hours_monthly_less <- tab_help_banking_hours_monthly_less(df_receiver)
  
  c_help_banking_monthly_less <- ggplot(data = df_help_banking_hours_monthly_less, mapping = aes(
    x = primary_help_banking_hours, y =
      help_banking_hours_freq, fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_hours_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Hours primary caregiver helped with banking - Less than once a month") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances less than once a month", width = 115)) +
    xlab("Time (hours)") +
    ylab("Frequencyt") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_help_banking_monthly_less)
}


# Caregiver responses ####

### Types of activities respondents provided help with
chart_activity_give_help <- function(df_giver, APR_10, APR_20, APR_30, APR_40, APR_50, APR_60, APR_70, APR_80) {
  transportation <- nrow(filter(df_giver, APR_10 == 1))
  household_chores <- nrow(filter(df_giver, APR_20 == 1))
  house_maintenance <- nrow(filter(df_giver, APR_30 == 1))
  personal_care <- nrow(filter(df_giver, APR_40 == 1))
  medical_treatment <- nrow(filter(df_giver, APR_50 == 1))
  scheduling <- nrow(filter(df_giver, APR_60 == 1))
  banking <- nrow(filter(df_giver, APR_70 == 1))
  help_activity_other <- nrow(filter(df_giver, APR_80 == 1))

  activity_give_help <- c("transportation", "household chorse", "house maintenance", "personal care", "medical
  treatment", "scheduling", "banking", "other")
  activity_give_help_freq <- c(
    transportation, household_chores, house_maintenance, personal_care, medical_treatment,
    scheduling, banking, help_activity_other
  )

  df_activity_give_help <- tibble(activity_give_help, activity_give_help_freq)

  c_activity_give_help <- ggplot(data = df_activity_give_help, mapping = aes(
    x = fct_inorder(activity_give_help), y =
      activity_give_help_freq, fill = activity_give_help
  )) +
    geom_col() +
    geom_text(aes(label = activity_give_help_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Types of activities respondents provided help with - Past 12 months") +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_give_help$activity_give_help, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_activity_give_help)
}


### Age of respondent's care receiver
chart_age_primary_receiver <- function(df_giver, CRRCPAGR) {
  age_receiver_1 <- nrow(filter(df_giver, CRRCPAGR == 1))
  age_receiver_2 <- nrow(filter(df_giver, CRRCPAGR == 2))
  age_receiver_3 <- nrow(filter(df_giver, CRRCPAGR == 3))
  age_receiver_4 <- nrow(filter(df_giver, CRRCPAGR == 4))
  age_receiver_5 <- nrow(filter(df_giver, CRRCPAGR == 5))
  age_receiver_6 <- nrow(filter(df_giver, CRRCPAGR == 6))
  age_receiver_7 <- nrow(filter(df_giver, CRRCPAGR == 7))
  age_receiver_8 <- nrow(filter(df_giver, CRRCPAGR == 8))
  age_receiver_9 <- nrow(filter(df_giver, CRRCPAGR == 9))
  age_receiver_10 <- nrow(filter(df_giver, CRRCPAGR == 10))
  age_receiver_11 <- nrow(filter(df_giver, CRRCPAGR == 11))
  age_receiver_12 <- nrow(filter(df_giver, CRRCPAGR == 12))
  age_receiver_13 <- nrow(filter(df_giver, CRRCPAGR == 13))
  age_receiver_14 <- nrow(filter(df_giver, CRRCPAGR == 14))
  age_receiver_15 <- nrow(filter(df_giver, CRRCPAGR == 15))
  age_receiver_16 <- nrow(filter(df_giver, CRRCPAGR == 16))
  age_receiver_17 <- nrow(filter(df_giver, CRRCPAGR == 17))
  age_receiver_18 <- nrow(filter(df_giver, CRRCPAGR == 18))
  age_receiver_19 <- nrow(filter(df_giver, CRRCPAGR == 19))
  age_receiver_20 <- nrow(filter(df_giver, CRRCPAGR == 20))

  age_receiver <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65-69", "70-74", "75-80", "80-84", "85-89", "90-94", "95+"
  )
  age_receiver_freq <- c(
    age_receiver_1, age_receiver_2, age_receiver_3, age_receiver_4, age_receiver_5, age_receiver_6,
    age_receiver_7, age_receiver_8, age_receiver_9, age_receiver_10, age_receiver_11,
    age_receiver_12, age_receiver_13, age_receiver_14, age_receiver_15, age_receiver_16,
    age_receiver_17, age_receiver_18, age_receiver_19, age_receiver_20
  )

  df_age_primary_receiver <- tibble(age_receiver, age_receiver_freq)

  c_age_primary_receiver <- ggplot(data = df_age_primary_receiver, mapping = aes(x = fct_inorder(age_receiver), y = age_receiver_freq, fill = age_receiver)) +
    geom_col() +
    geom_text(aes(label = age_receiver_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Age of primary care receiver") +
    xlab("Age Group (years)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_age_primary_receiver)
}


# ### Number of hours are or help provided by respondent - Per average week
chart_hours_help_provided <- function(df_giver, HAR_10C) {
  hours_0 <- nrow(filter(df_giver, HAR_10C == 0))
  hours_1 <- nrow(filter(df_giver, HAR_10C == 1))
  hours_2 <- nrow(filter(df_giver, HAR_10C == 2))
  hours_3 <- nrow(filter(df_giver, HAR_10C == 3))
  hours_4 <- nrow(filter(df_giver, HAR_10C == 4))
  hours_5 <- nrow(filter(df_giver, HAR_10C == 5))

  hours_help_provided <- c("0", "1-9", "10-19", "20-29", "30-39", "40+")
  hours_help_provided_freq <- c(hours_0, hours_1, hours_2, hours_3, hours_4, hours_5)

  df_hours_help_provided <- tibble(hours_help_provided, hours_help_provided_freq)

  c_hours_help_provided <- ggplot(data = df_hours_help_provided, mapping = aes(
    x = fct_inorder(hours_help_provided), y = hours_help_provided_freq, fill =
      hours_help_provided
  )) +
    geom_col() +
    geom_text(aes(label = hours_help_provided_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Numbers of hours of help provided - Per average week per activity") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_hours_help_provided)
}

# ### Distance between the respondent's and the care receiver's dwellings PRD_10
chart_primary_receiver_distance <- function(df_giver, PRD_10) {
  primary_receiver_distance_1 <- nrow(filter(df_giver, PRD_10 == 1))
  primary_receiver_distance_2 <- nrow(filter(df_giver, PRD_10 == 2))
  primary_receiver_distance_3 <- nrow(filter(df_giver, PRD_10 == 3))
  primary_receiver_distance_4 <- nrow(filter(df_giver, PRD_10 == 4))
  primary_receiver_distance_5 <- nrow(filter(df_giver, PRD_10 == 5))
  primary_receiver_distance_6 <- nrow(filter(df_giver, PRD_10 == 6))
  primary_receiver_distance_7 <- nrow(filter(df_giver, PRD_10 == 7))

  primary_receiver_distance <- c("same household", "same building", "<10 min", "10 to <30 min", "30 min to <1
  hour", "1 to <3 hours", ">3 hours")
  primary_receiver_distance_freq <- c(
    primary_receiver_distance_1, primary_receiver_distance_2,
    primary_receiver_distance_3,
    primary_receiver_distance_4, primary_receiver_distance_5, primary_receiver_distance_6,
    primary_receiver_distance_7
  )

  df_primary_receiver_distance <- tibble(primary_receiver_distance, primary_receiver_distance_freq)

  c_primary_receiver_distance <- ggplot(data = df_primary_receiver_distance, mapping = aes(
    x = fct_inorder(primary_receiver_distance), y =
      primary_receiver_distance_freq, fill = primary_receiver_distance
  )) +
    geom_col() +
    geom_text(aes(label = primary_receiver_distance_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Distance between the respondent's and carereceiver's dwellings") +
    xlab("Distance (by car)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_primary_receiver_distance)
}

### Helped primary care receiver with banking - Frequency
chart_give_help_banking_freq <- function(df_giver, AGB_20) {
  primary_help_banking_1 <- nrow(filter(df_giver, AGB_20 == 1))
  primary_help_banking_2 <- nrow(filter(df_giver, AGB_20 == 2))
  primary_help_banking_3 <- nrow(filter(df_giver, AGB_20 == 3))
  primary_help_banking_4 <- nrow(filter(df_giver, AGB_20 == 4))

  primary_help_banking <- c("daily", "at least once a week", "at least once a month", "less than once a month")
  primary_help_banking_freq <- c(
    primary_help_banking_1, primary_help_banking_2, primary_help_banking_3,
    primary_help_banking_4
  )

  df_primary_help_banking <- tibble(primary_help_banking, primary_help_banking_freq)

  c_give_help_banking_freq <- ggplot(data = df_primary_help_banking, mapping = aes(
    x = fct_inorder(primary_help_banking), y = primary_help_banking_freq, fill =
      primary_help_banking
  )) +
    geom_col() +
    geom_text(aes(label = primary_help_banking_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - Frequency") +
    xlab("Help Frequency") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_freq)
}

### Helped primary care receiver with banking - Number of hours
chart_give_help_banking_hours <- function(df_giver, ARB_30C) {
  primary_receiver_banking_hours_1 <- nrow(filter(df_giver, ARB_30C == 1))
  primary_receiver_banking_hours_2 <- nrow(filter(df_giver, ARB_30C == 2))
  primary_receiver_banking_hours_3 <- nrow(filter(df_giver, ARB_30C == 3))
  primary_receiver_banking_hours_4 <- nrow(filter(df_giver, ARB_30C == 4))

  primary_receiver_banking_hours <- c("<1", "1 to <3", "3 to <5", "5+")
  primary_receiver_banking_hours_freq <- c(
    primary_receiver_banking_hours_1, primary_receiver_banking_hours_2,
    primary_receiver_banking_hours_3, primary_receiver_banking_hours_4
  )

  df_primary_receiver_banking_hours <- tibble(primary_receiver_banking_hours, primary_receiver_banking_hours_freq)

  c_give_help_banking_hours <- ggplot(data = df_primary_receiver_banking_hours, mapping = aes(x = fct_inorder(primary_receiver_banking_hours), y = primary_receiver_banking_hours_freq, fill = primary_receiver_banking_hours)) +
    geom_col() +
    geom_text(aes(label = primary_receiver_banking_hours_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Primary caregiver helped with banking - Number of hours") +
    xlab("Hours helped") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_hours)
}

### The following 4 charts show how often the respondent provided help with banking to their primary care receiver and for how many hours each time
### daily
chart_give_help_banking_daily <- function(df_giver, ARB_30C, ARB_20) {
  help_banking_1 <- nrow(filter(df_giver, ARB_30C == 1 & ARB_20 == 1))
  help_banking_2 <- nrow(filter(df_giver, ARB_30C == 2 & ARB_20 == 1))
  help_banking_3 <- nrow(filter(df_giver, ARB_30C == 3 & ARB_20 == 1))
  help_banking_4 <- nrow(filter(df_giver, ARB_30C == 4 & ARB_20 == 1))

  help_banking <- c("<1", "1 to <3", "3 to <5", "5+")
  help_banking_freq <- c(
    help_banking_1, help_banking_2,
    help_banking_3, help_banking_4
  )

  df_help_banking <- tibble(help_banking, help_banking_freq)

  c_give_help_banking_daily <- ggplot(data = df_help_banking, mapping = aes(
    x = fct_inorder(help_banking), y =
      help_banking_freq, fill = help_banking
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - Daily") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_daily)
}

### weekly
chart_give_help_banking_weekly <- function(df_giver, ARB_30C, ARB_20) {
  help_banking_1 <- nrow(filter(df_giver, ARB_30C == 1 & ARB_20 == 2))
  help_banking_2 <- nrow(filter(df_giver, ARB_30C == 2 & ARB_20 == 2))
  help_banking_3 <- nrow(filter(df_giver, ARB_30C == 3 & ARB_20 == 2))
  help_banking_4 <- nrow(filter(df_giver, ARB_30C == 4 & ARB_20 == 2))

  help_banking <- c("<1", "1 to <3", "3 to <5", "5+")
  help_banking_freq <- c(
    help_banking_1, help_banking_2,
    help_banking_3, help_banking_4
  )

  df_help_banking <- tibble(help_banking, help_banking_freq)

  c_give_help_banking_weekly <- ggplot(data = df_help_banking, mapping = aes(
    x = fct_inorder(help_banking), y =
      help_banking_freq, fill = help_banking
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - At least once a week") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_weekly)
}

### monthly
chart_give_help_banking_monthly <- function(df_giver, ARB_30C, ARB_20) {
  help_banking_1 <- nrow(filter(df_giver, ARB_30C == 1 & ARB_20 == 3))
  help_banking_2 <- nrow(filter(df_giver, ARB_30C == 2 & ARB_20 == 3))
  help_banking_3 <- nrow(filter(df_giver, ARB_30C == 3 & ARB_20 == 3))
  help_banking_4 <- nrow(filter(df_giver, ARB_30C == 4 & ARB_20 == 3))

  help_banking <- c("<1", "1 to <3", "3 to <5", "5+")
  help_banking_freq <- c(
    help_banking_1, help_banking_2,
    help_banking_3, help_banking_4
  )

  df_help_banking <- tibble(help_banking, help_banking_freq)

  c_give_help_banking_monthly <- ggplot(data = df_help_banking, mapping = aes(
    x = fct_inorder(help_banking), y =
      help_banking_freq, fill = help_banking
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - At least once a month") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_monthly)
}

### less than monthly
chart_give_help_banking_monthly_less <- function(df_giver, ARB_30C, ARB_20) {
  help_banking_1 <- nrow(filter(df_giver, ARB_30C == 1 & ARB_20 == 4))
  help_banking_2 <- nrow(filter(df_giver, ARB_30C == 2 & ARB_20 == 4))
  help_banking_3 <- nrow(filter(df_giver, ARB_30C == 3 & ARB_20 == 4))
  help_banking_4 <- nrow(filter(df_giver, ARB_30C == 4 & ARB_20 == 4))

  help_banking <- c("<1", "1 to <3", "3 to <5", "5+")
  help_banking_freq <- c(
    help_banking_1, help_banking_2,
    help_banking_3, help_banking_4
  )

  df_help_banking <- tibble(help_banking, help_banking_freq)

  c_give_help_banking_monthly_less <- ggplot(data = df_help_banking, mapping = aes(
    x = fct_inorder(help_banking), y =
      help_banking_freq, fill = help_banking
  )) +
    geom_col() +
    geom_text(aes(label = help_banking_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - Less than once a month") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_monthly_less)
}

## Consequences of caregiving on the caregiver

### Out-of-pocket expenses because of caregiving responsibilities
chart_out_of_pocket <- function(df_giver, ICF_210, ICF_220, ICF_230, ICF_240, ICF_250, ICF_260, ICF2_270) {
  out_of_pocket_1 <- nrow(filter(df_giver, ICF_210 == 1))
  out_of_pocket_2 <- nrow(filter(df_giver, ICF_220 == 1))
  out_of_pocket_3 <- nrow(filter(df_giver, ICF_230 == 1))
  out_of_pocket_4 <- nrow(filter(df_giver, ICF_240 == 1))
  out_of_pocket_5 <- nrow(filter(df_giver, ICF_250 == 1))
  out_of_pocket_6 <- nrow(filter(df_giver, ICF_260 == 1))
  out_of_pocket_7 <- nrow(filter(df_giver, ICF2_270 == 1))

  out_of_pocket <- c("home modifications", "professional service", "hiring people to help", "transportation,
  accommodation", "specialized aids/devices", "prescription/non-pres. drugs", "other")
  out_of_pocket_freq <- c(
    out_of_pocket_1, out_of_pocket_2, out_of_pocket_3, out_of_pocket_4, out_of_pocket_5,
    out_of_pocket_6, out_of_pocket_7
  )
  df_out_of_pocket <- tibble(out_of_pocket, out_of_pocket_freq)

  c_out_of_pocket <- ggplot(data = df_out_of_pocket, mapping = aes(x = fct_inorder(out_of_pocket), y = out_of_pocket_freq, fill = out_of_pocket)) +
    geom_col() +
    geom_text(aes(label = out_of_pocket_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Out-of-pocket expenses because of caregiving responsibilities") +
    xlab("Expense categories") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_out_of_pocket$out_of_pocket, width = 13)) +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_out_of_pocket)
}

### Financial hardship
chart_financial_hardship <- function(df_giver, ICF2_290, CRRCPAGR, ICF2_300, ICF2_310, ICF2_320, ICF2_330, ICF2_340, ICF2_350) {
  financial_hardship_1 <- nrow(filter(df_giver, ICF2_290 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))
  financial_hardship_2 <- nrow(filter(df_giver, ICF2_300 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))
  financial_hardship_3 <- nrow(filter(df_giver, ICF2_310 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))
  financial_hardship_4 <- nrow(filter(df_giver, ICF2_320 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))
  financial_hardship_5 <- nrow(filter(df_giver, ICF2_330 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))
  financial_hardship_6 <- nrow(filter(df_giver, ICF2_340 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))
  financial_hardship_7 <- nrow(filter(df_giver, ICF2_350 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))

  financial_hardship <- c("borrowed money from family or friends", "loans from a bank or financial institution", "use
  or defer savings", "modify spending", "sell off assets", "file for bankruptcy", "other")
  financial_hardship_freq <- c(
    financial_hardship_1, financial_hardship_2, financial_hardship_3, financial_hardship_4,
    financial_hardship_5, financial_hardship_6, financial_hardship_7
  )

  df_financial_hardship <- tibble(financial_hardship, financial_hardship_freq)

  c_financial_hardship <- ggplot(data = df_financial_hardship, mapping = aes(
    x = fct_inorder(financial_hardship), y =
      financial_hardship_freq, fill = financial_hardship
  )) +
    geom_col() +
    geom_text(aes(label = financial_hardship_freq), position = position_stack(vjust = 0.5)) +
    ggtitle("Financial hardship because of caregiving (65+) responsibilities from 735 caregivers") +
    xlab("Expense categories") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_financial_hardship$financial_hardship, width = 13)) +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_financial_hardship)
}
