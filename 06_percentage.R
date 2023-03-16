# Percentage charts

chart_respondent_groups_percent <- function() {
  df <- tab_pop_freq()

  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(pop_name),
      y = percentage,
      fill = pop_name
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
    ggtitle("GSS 2018 repsondent groups") +
    labs(caption = str_wrap("Count for respondent groupings: caregiver, care receivers 65 years and over, care receivers 65 to 74 years, care receivers 75 years and over, care receiver and caregiver, and unmet needs for GSS 2018.", width = 115)) +
    xlab("Respondent group") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$pop_name, width = 15)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(chart)
}


chart_health_conditions_percent <- function(df_receiver) {
  df <- tab_health_conditions(df_receiver)

  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(health_conditions),
      y = percentage,
      fill = health_conditions
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
    labs(caption = str_wrap("Count for main health conditions for which respondents considered to be a care receiver and 65 years of age or older received help.", width = 115)) +
    xlab("Health Condition") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$health_conditions, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(chart)
}

### Types of activities respondents received help with
chart_activity_receive_help_percent <- function(df_receiver) {
  df <- tab_activity_receive_help(df_receiver)

  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(activity_receive_help),
      y = percentage,
      fill = activity_receive_help
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received with from family, friends or neighbours in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$activity_receive_help, width = 12)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(chart)
}

### Age of respondent's primary caregiver
chart_age_primary_giver_percent <- function(df_receiver) {
  df <- tab_age_primary_giver(df_receiver)

  c_age_primary_giver <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(giver_age_group),
      y = percentage,
      fill = giver_age_group
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
    ggtitle("Age of respondent's primary caregivers") +
    labs(caption = str_wrap("Count of the age (groups of 5) of primary caregivers for respondents considered to be a care receiver and 65 years of age or older.", width = 120)) +
    xlab("Age (years)") +
    ylab("Count") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_age_primary_giver)
}

# ### Types of activities respondents received professional help with
chart_activity_receive_help_pro_percent <- function(df_receiver) {
  df <- tab_activity_receive_help_pro(df_receiver)

  c_activity_receive_help_pro <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(activity_receive_help),
      y = percentage,
      fill = activity_receive_help
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
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
chart_hours_help_received_percent <- function(df_receiver) {
  df <- tab_hours_help_received(df_receiver)

  c_hours_help_received <- ggplot(
    data = df,
    mapping = aes(
      x = hours_help_received,
      y = percentage,
      fill = hours_help_received
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
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
chart_primary_giver_distance_percent <- function(df_receiver) {
  df <- tab_primary_giver_distance(df_receiver)

  c_primary_giver_distance <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(dwelling_distances),
      y = percentage,
      fill = dwelling_distances
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
    ggtitle("Distance between the respondent's and caregiver's dwellings") +
    labs(caption = str_wrap("Counts for the distance by car between respondents considered to be a care receiver and 65 years of age or older, and their primary caregiver during the time they were receiving help in the past 12 months.", width = 115)) +
    xlab("Distance (time)") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$dwelling_distances, width = 13)) +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_primary_giver_distance)
}

### Primary caregiver helped with banking - Frequency
chart_receive_help_banking_freq_percent <- function(df_receiver) {
  df <- tab_receive_help_banking_freq(df_receiver)

  c_receive_help_banking_freq <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq),
      y = percentage,
      fill = primary_help_banking_freq
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
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
chart_receive_help_banking_hours_percent <- function(df_receiver) {
  df <- tab_receive_help_banking_hours(df_receiver)

  c_receive_help_banking_hours <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
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
chart_help_banking_hours_daily_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_daily(df_receiver)

  c_help_banking_hours_daily <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
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
chart_help_banking_weekly_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_weekly(df_receiver)

  c_help_banking_weekly <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
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

chart_help_banking_monthly_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_monthly(df_receiver)

  c_help_banking_monthly <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
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
chart_help_banking_monthly_less_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_monthly_less(df_receiver)

  c_help_banking_monthly_less <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    geom_text(aes(label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
    ggtitle("Hours primary caregiver helped with banking - Less than once a month") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances less than once a month", width = 115)) +
    xlab("Time (hours)") +
    ylab("Frequencyt") +
    scale_fill_viridis_d() +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0))

  return(c_help_banking_monthly_less)
}
