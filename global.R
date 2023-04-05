library(conflicted)
library(tidyverse)
library(haven)
library(viridis)
conflicts_prefer(dplyr::filter)

source("01_main.R")
source("02_var_x.R")
source("03_var_y.R")
source("04_general_df.R")
source("05_table.R")
source("06_percentage.R")
source("07_group_by.R")


# apply_filter(): takes a frame and filter based on option selected
# df_input (tibble): data frame to be transformed
# select_option (integer): filter value mapped to the response category
# col_name (String): variable to filter by
apply_filter <- function(df_input, select_option, col_name) {
  filtered_df <- if (select_option != -1) {
    filtered_df <- df_input %>% filter(!!as.symbol(col_name) == select_option) # the value from the list: e.g. both sexes = -1, male = 1, female = 2
  } else {
    df_input
  }
  
  return(filtered_df)
}

# count_map(): takes a frame and returns the count for a categorical vector based on a chosen column
# df (tibble): data frame to be transformed
# x_options (vector): vector of variables to be counted
# col_name (String): variable to filter by
# col_name2 (String): second variable to filter by
# response_code (numeric): response value constant mapped to col_name2
count_map <- function(df_input, x_options, col_name, col_name2 = NULL, response_code = NULL) {
  counts <- unlist(map(x_options, function(f) {
    if (!is.null(col_name2) & !is.null(response_code)) {
      nrow(filter(df_input, !!as.symbol(col_name) == f & !!as.symbol(col_name2) == response_code))
    } else {
      nrow(filter(df_input, !!as.symbol(col_name) == f))
    }
  }
  ))
}

total_receiver_male <- nrow(apply_filter(df_receiver, 1, "SEX"))
total_receiver_female <- nrow(apply_filter(df_receiver, 2, "SEX"))
total_giver_male <- nrow(apply_filter(df_giver, 1, "SEX"))
total_giver_female <- nrow(apply_filter(df_giver, 2, "SEX"))

# General Charts ####

## Respondent groups ####
c_respondent_groups <- ggplot(
  data = df_pops,
  mapping = aes(x = fct_inorder(pop_name), y = pop_freq, fill = pop_name)
) +
  geom_col() +
  geom_text(aes(label = pop_freq), position = position_stack(vjust = 0.5)) +
  ggtitle("GSS 2018 repsondent groups") +
  labs(x = "Respondent group", 
       y = "Count", 
       caption = str_wrap("Count for respondent groupings: caregiver, care receivers 65 years and over, care receivers 65 to 74 years, care receivers 75 years and over, care receiver and caregiver, and unmet needs for GSS 2018.", width = 115)) +
  scale_x_discrete(labels = str_wrap(df_pops$pop_name, width = 15)) +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  theme(plot.caption = element_text(hjust = 0))

## Sex of primary caregiver and primary care receiver ####
c_primary_sex <- ggplot(
  data = df_primary_sex,
  mapping = aes(x = sex, y = freq, fill = sex)
) +
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

  c_health_conditions <- ggplot(
    data = df_health_conditions,
    mapping = aes(
      x = fct_inorder(health_conditions),
      y = count,
      fill = health_conditions
    )
  ) +
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

  c_activity_receive_help <- ggplot(
    data = df_activity_receive_help,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = count,
      fill = help_activities
    )
  ) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received with from family, friends or neighbours in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_receive_help$help_activities, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_activity_receive_help)
}

### Age of respondent's primary caregiver
chart_age_primary_giver <- function(df_receiver) {
  df_age_primary_giver <- tab_age_primary_giver(df_receiver)

  c_age_primary_giver <- ggplot(
    data = df_age_primary_giver,
    mapping = aes(
      x = fct_inorder(giver_age_group),
      y = count,
      fill = giver_age_group
    )
  ) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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

  c_activity_receive_help_pro <- ggplot(
    data = df_activity_receive_help_pro,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = count,
      fill = help_activities
    )
  ) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Activities received professional help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received help from a professional in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_receive_help_pro$help_activities, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))
  return(c_activity_receive_help_pro)
}


### Numbers of hours of help received - Per average week per activity
chart_hours_help_received <- function(df_receiver) {
  df_hours_help_received <- tab_hours_help_received(df_receiver)

  c_hours_help_received <- ggplot(
    data = df_hours_help_received,
    mapping = aes(
      x = help_hours,
      y = count,
      fill = help_hours
    )
  ) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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

  c_primary_giver_distance <- ggplot(
    data = df_primary_giver_distance,
    mapping = aes(
      x = fct_inorder(dwelling_distances),
      y = count, 
      fill = dwelling_distances
    )
  ) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Distance between the respondent's and caregiver's dwellings") +
    labs(caption = str_wrap("Counts for the distance by car between respondents considered to be a care receiver and 65 years of age or older, and their primary caregiver during the time they were receiving help in the past 12 months.", width = 115)) +
    xlab("Distance (time)") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_primary_giver_distance$dwelling_distances, width = 13)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_primary_giver_distance)
}

### Primary caregiver helped with banking - Frequency
chart_receive_help_banking_freq <- function(df_receiver) {
  df_receive_help_banking_freq <- tab_receive_help_banking_freq(df_receiver)

  c_receive_help_banking_freq <- ggplot(
    data = df_receive_help_banking_freq,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq),
      y = count,
      fill = primary_help_banking_freq
    )
  ) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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
chart_receive_help_banking_hours <- function(df_receiver) {
  df_receive_help_banking_hours <- tab_receive_help_banking_hours(df_receiver)

  c_receive_help_banking_hours <- ggplot(
    data = df_receive_help_banking_hours, 
    mapping = aes(
      x = primary_help_banking_hours, 
      y = count, 
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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

  c_help_banking_hours_daily <- ggplot(
    data = df_help_banking_hours_freq, 
    mapping = aes(
      x = primary_help_banking_hours, 
      y = count, 
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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

  c_help_banking_weekly <- ggplot(
    data = df_help_banking_hours_weekly, 
    mapping = aes(
      x = primary_help_banking_hours, 
      y = count, 
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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

  c_help_banking_monthly <- ggplot(
    data = df_help_banking_hours_monthly, 
    mapping = aes(
      x = primary_help_banking_hours, 
      y = count, 
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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
chart_help_banking_monthly_less <- function(df_receiver) {
  df_help_banking_hours_monthly_less <- tab_help_banking_hours_monthly_less(df_receiver)

  c_help_banking_monthly_less <- ggplot(
    data = df_help_banking_hours_monthly_less, 
    mapping = aes(
      x = primary_help_banking_hours, 
      y = count, 
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
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
chart_activity_give_help <- function(df_giver) {
  df_activity_give_help <- tab_activity_give_help(df_giver)

  c_activity_give_help <- ggplot(data = df_activity_give_help, mapping = aes(
    x = fct_inorder(help_activities), 
    y = count, 
    fill = help_activities
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Types of activities respondents provided help with - Past 12 months") +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_give_help$help_activities, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_activity_give_help)
}


### Age of respondent's care receiver
chart_age_primary_receiver <- function(df_giver) {
  df_age_primary_receiver <- tab_age_primary_receiver(df_giver)

  c_age_primary_receiver <- ggplot(
    data = df_age_primary_receiver, 
    mapping = aes(
      x = fct_inorder(primary_receiver_age_group), 
      y = count, 
      fill = primary_receiver_age_group)) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Age of primary care receiver") +
    xlab("Age Group (years)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_age_primary_receiver)
}


# ### Number of hours are or help provided by respondent - Per average week
chart_hours_help_provided <- function(df_giver) {
  df_hours_help_provided <- tab_hours_help_provided(df_giver)

  c_hours_help_provided <- ggplot(
    data = df_hours_help_provided, 
    mapping = aes(
      x = fct_inorder(help_hours),
      y = count, 
      fill = help_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Numbers of hours of help provided - Per average week per activity") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_hours_help_provided)
}

# ### Distance between the respondent's and the care receiver's dwellings PRD_10
chart_primary_receiver_distance <- function(df_giver) {
  df_primary_receiver_distance <- tab_primary_receiver_distance(df_giver)
  
  c_primary_receiver_distance <- ggplot(
    data = df_primary_receiver_distance, 
    mapping = aes(
      x = fct_inorder(dwelling_distances), 
      y = count,
      fill = dwelling_distances
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Distance between the respondent's and carereceiver's dwellings") +
    xlab("Distance by car") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_primary_receiver_distance)
}

### Helped primary care receiver with banking - Frequency
chart_give_help_banking_freq <- function(df_giver) {
  df_give_help_banking_freq <- tab_give_help_banking_freq(df_giver)

  c_give_help_banking_freq <- ggplot(
    data = df_give_help_banking_freq,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq), 
      y = count,
      fill = primary_help_banking_freq
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - Frequency") +
    xlab("Help Frequency") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_freq)
}

### Helped primary care receiver with banking - Number of hours
chart_give_help_banking_hours <- function(df_giver) {
  df_give_help_banking_hours <- tab_give_help_banking_hours(df_giver)

  c_give_help_banking_hours <- ggplot(
    data = df_give_help_banking_hours,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours), 
      y = count,
      fill = primary_help_banking_hours)) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Primary caregiver helped with banking - Number of hours") +
    xlab("Hours helped") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_hours)
}

### The following 4 charts show how often the respondent provided help with banking to their primary care receiver and for how many hours each time
### daily
chart_give_help_banking_daily <- function(df_giver) {
  df_give_help_banking_daily <- tab_give_help_banking_daily(df_giver)

  c_give_help_banking_daily <- ggplot(
    data = df_give_help_banking_daily,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours), 
      y = count,
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - Daily") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_daily)
}

### weekly
chart_give_help_banking_weekly <- function(df_giver) {
  df_give_help_banking_weekly <- tab_give_help_banking_weekly(df_giver)

  c_give_help_banking_weekly <- ggplot(
    data = df_give_help_banking_weekly,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours), 
      y = count,
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - At least once a week") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_weekly)
}

### monthly
chart_give_help_banking_monthly <- function(df_giver) {
  df_give_help_banking_monthly <- tab_give_help_banking_monthly(df_giver)

  c_give_help_banking_monthly <- ggplot(
    data = df_give_help_banking_monthly,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours), 
      y = count,
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - At least once a month") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_monthly)
}

### less than monthly
chart_give_help_banking_monthly_less <- function(df_giver) {
  df_give_help_banking_monthly_less <- tab_give_help_banking_monthly_less(df_giver)

  c_give_help_banking_monthly_less <- ggplot(
    data = df_give_help_banking_monthly_less,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours), 
      y = count,
      fill = primary_help_banking_hours
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Helped primary care receiver with banking - Less than once a month") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_give_help_banking_monthly_less)
}

## Consequences of caregiving on the caregiver

### Out-of-pocket expenses because of caregiving responsibilities
chart_out_of_pocket <- function(df_giver) {
  df_out_of_pocket <- tab_out_of_pocket(df_giver)

  c_out_of_pocket <- ggplot(
    data = df_out_of_pocket, 
    mapping = aes(
      x = fct_inorder(out_of_pocket_expenses), 
      y = count,
      fill = out_of_pocket_expenses)) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Out-of-pocket expenses because of caregiving responsibilities") +
    xlab("Expense categories") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_out_of_pocket$out_of_pocket_expenses, width = 13)) +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_out_of_pocket)
}

### Financial hardship
chart_financial_hardship <- function(df_giver) {
  df_financial_hardship <- tab_financial_hardship(df_giver)

  c_financial_hardship <- ggplot(
    data = df_financial_hardship, 
    mapping = aes(
      x = fct_inorder(financial_hardship), 
      y = count,
      fill = financial_hardship
  )) +
    geom_col() +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    ggtitle("Financial hardship because of caregiving (65+) responsibilities from 735 caregivers") +
    xlab("Expense categories") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_financial_hardship$financial_hardship, width = 13)) +
    scale_fill_viridis_d() +
    guides(fill = "none")

  return(c_financial_hardship)
}
