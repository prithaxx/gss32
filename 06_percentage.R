# Percentage charts

chart_respondent_groups_percent <- function() {
  df <- tab_pop_freq()
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(pop_name))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
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
    geom_text(aes(color=pop_name, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend = FALSE) +
    ggtitle("GSS 2018 repsondent groups") +
    labs(caption = str_wrap("Count for respondent groupings: caregiver, care receivers 65 years and over, care receivers 65 to 74 years, care receivers 75 years and over, care receiver and caregiver, and unmet needs for GSS 2018.", width = 115)) +
    xlab("Respondent group") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$pop_name, width = 15)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(chart)
}


chart_health_conditions_percent <- function(df_receiver) {
  df <- tab_health_conditions(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(health_conditions))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(health_conditions),
      y = percentage,
      fill = health_conditions
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=health_conditions, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    labs(caption = str_wrap("Count for main health conditions of care receivers aged 65 years of age or older.", width = 115)) +
    xlab("Health Condition") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$health_conditions, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(chart)
}

### Types of activities respondents received help with
chart_activity_receive_help_percent <- function(df_receiver) {
  df <- tab_activity_receive_help(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = percentage,
      fill = help_activities
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_activities, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received with from family, friends or neighbours in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(chart)
}

### Age of respondent's primary caregiver
chart_age_primary_giver_percent <- function(df_receiver) {
  df <- tab_age_primary_giver(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(giver_age_group))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_age_primary_giver <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(giver_age_group),
      y = percentage,
      fill = giver_age_group
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=giver_age_group, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Age of respondent's primary caregivers") +
    labs(caption = str_wrap("Count of the age (groups of 5) of primary caregivers for respondents considered to be a care receiver and 65 years of age or older.", width = 120)) +
    xlab("Age (years)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_age_primary_giver)
}

# ### Types of activities respondents received professional help with
chart_activity_receive_help_pro_percent <- function(df_receiver) {
  df <- tab_activity_receive_help_pro(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_activity_receive_help_pro <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = percentage,
      fill = help_activities
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_activities, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received professional help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received help from a professional in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_activity_receive_help_pro)
}


### Numbers of hours of help received - Per average week per activity
chart_hours_help_received_percent <- function(df_receiver) {
  df <- tab_hours_help_received(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(help_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_hours_help_received <- ggplot(
    data = df,
    mapping = aes(
      x = help_hours,
      y = percentage,
      fill = help_hours
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Numbers of hours of help received - Per average week per activity") +
    labs(caption = str_wrap("Count for the number of hours of help received, per average week per activity (transportation, house maintenance, household chores, scheduling, banking, medical treatment, personal care, other) for respondents considered to be a care receiver and 65 years of age or older from family, friends or neighbours in the past 12 months.", width = 115)) +
    xlab("Time (hour)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_hours_help_received)
}

### Distance between the respondent's and the caregiver's dwellings
chart_primary_giver_distance_percent <- function(df_receiver) {
  df <- tab_primary_giver_distance(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(dwelling_distances))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_primary_giver_distance <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(dwelling_distances),
      y = percentage,
      fill = dwelling_distances
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=dwelling_distances, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Distance between the respondent's and caregiver's dwellings") +
    labs(caption = str_wrap("Counts for the distance by car between respondents considered to be a care receiver and 65 years of age or older, and their primary caregiver during the time they were receiving help in the past 12 months.", width = 115)) +
    xlab("Distance (time)") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df$dwelling_distances, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_primary_giver_distance)
}

### Primary caregiver helped with banking - Frequency
chart_receive_help_banking_freq_percent <- function(df_receiver) {
  df <- tab_receive_help_banking_freq(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_freq))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_receive_help_banking_freq <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq),
      y = percentage,
      fill = primary_help_banking_freq
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_freq, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Primary caregiver helped with banking - Frequency") +
    labs(caption = str_wrap("Count for how often respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.", width = 120)) +
    xlab("Help Frequency") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_receive_help_banking_freq)
}

### Primary caregiver helped with banking - Number of hours
chart_receive_help_banking_hours_percent <- function(df_receiver) {
  df <- tab_receive_help_banking_hours(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_receive_help_banking_hours <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Primary caregiver helped with banking - Number of hours") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_receive_help_banking_hours)
}

### The following 4 charts show How often and number of hours a respondent received help from with banking

### daily
chart_help_banking_hours_daily_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_daily(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_help_banking_hours_daily <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Hours primary caregiver helped with banking - Daily") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances daily", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_help_banking_hours_daily)
}

### at least once a week
chart_help_banking_weekly_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_weekly(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_help_banking_weekly <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Hours primary caregiver helped with banking - At least once a week") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances at least once a week", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_help_banking_weekly)
}

### monthly

chart_help_banking_monthly_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_monthly(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_help_banking_monthly <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Hours primary caregiver helped with banking - At least once a month") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances at least once a month", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_help_banking_monthly)
}

# less than monthly
chart_help_banking_monthly_less_percent <- function(df_receiver) {
  df <- tab_help_banking_hours_monthly_less(df_receiver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_help_banking_monthly_less <- ggplot(
    data = df,
    mapping = aes(
      x = primary_help_banking_hours,
      y = percentage,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Hours primary caregiver helped with banking - Less than once a month") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older who received help with managing their finances less than once a month", width = 115)) +
    xlab("Time (hours)") +
    ylab("Frequencyt") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_help_banking_monthly_less)
}


# giver percent charts ####

chart_activity_give_help_percent <- function(df) {
  df <- tab_activity_give_help(df)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = percentage,
      fill = help_activities
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_activities, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received with from family, friends or neighbours in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Percentage") +
    scale_x_discrete(labels = str_wrap(df$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(chart)
}

chart_age_primary_receiver_percent <- function(df_giver) {
  df_age_primary_receiver <- tab_age_primary_receiver(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_receiver_age_group))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_age_primary_receiver <- ggplot(
    data = df_age_primary_receiver,
    mapping = aes(
      x = fct_inorder(primary_receiver_age_group),
      y = percentage,
      fill = primary_receiver_age_group)) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_receiver_age_group, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Age of primary care receiver") +
    xlab("Age Group (years)") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_age_primary_receiver)
}

chart_hours_help_provided_percent <- function(df_giver) {
  df_hours_help_provided <- tab_hours_help_provided(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(help_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_hours_help_provided <- ggplot(
    data = df_hours_help_provided,
    mapping = aes(
      x = fct_inorder(help_hours),
      y = percentage,
      fill = help_hours
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Numbers of hours of help provided - Per average week per activity") +
    xlab("Time (hours)") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_hours_help_provided)
}

chart_primary_receiver_distance_percent <- function(df_giver) {
  df_primary_receiver_distance <- tab_primary_receiver_distance(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(dwelling_distances))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_primary_receiver_distance <- ggplot(
    data = df_primary_receiver_distance,
    mapping = aes(
      x = fct_inorder(dwelling_distances),
      y = percentage,
      fill = dwelling_distances
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=dwelling_distances, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Distance between the respondent's and carereceiver's dwellings") +
    xlab("Distance by car") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_primary_receiver_distance)
}

### Helped primary care receiver with banking - Frequency
chart_give_help_banking_freq_percent <- function(df_giver) {
  df_give_help_banking_freq <- tab_give_help_banking_freq(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_freq))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_freq <- ggplot(
    data = df_give_help_banking_freq,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq),
      y = percentage,
      fill = primary_help_banking_freq
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_freq, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Helped primary care receiver with banking - Frequency") +
    xlab("Help Frequency") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_give_help_banking_freq)
}

### Helped primary care receiver with banking - Number of hours
chart_give_help_banking_hours_percent <- function(df_giver) {
  df_give_help_banking_hours <- tab_give_help_banking_hours(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_hours <- ggplot(
    data = df_give_help_banking_hours,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours),
      y = percentage,
      fill = primary_help_banking_hours)) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Primary caregiver helped with banking - Number of hours") +
    xlab("Hours helped") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_give_help_banking_hours)
}

### The following 4 charts show how often the respondent provided help with banking to their primary care receiver and for how many hours each time
### daily
chart_give_help_banking_daily_percent <- function(df_giver) {
  df_give_help_banking_daily <- tab_give_help_banking_daily(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_daily <- ggplot(
    data = df_give_help_banking_daily,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours),
      y = percentage,
      fill = primary_help_banking_hours
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Helped primary care receiver with banking - Daily") +
    xlab("Time (hours)") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_give_help_banking_daily)
}

### weekly
chart_give_help_banking_weekly_percent <- function(df_giver) {
  df_give_help_banking_weekly <- tab_give_help_banking_weekly(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_weekly <- ggplot(
    data = df_give_help_banking_weekly,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours),
      y = percentage,
      fill = primary_help_banking_hours
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Helped primary care receiver with banking - At least once a week") +
    xlab("Time (hours)") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_give_help_banking_weekly)
}

### monthly
chart_give_help_banking_monthly_percent <- function(df_giver) {
  df_give_help_banking_monthly <- tab_give_help_banking_monthly(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_monthly <- ggplot(
    data = df_give_help_banking_monthly,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours),
      y = percentage,
      fill = primary_help_banking_hours
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Helped primary care receiver with banking - At least once a month") +
    xlab("Time (hours)") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_give_help_banking_monthly)
}

### less than monthly
chart_give_help_banking_monthly_less_percent <- function(df_giver) {
  df_give_help_banking_monthly_less <- tab_give_help_banking_monthly_less(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_monthly_less <- ggplot(
    data = df_give_help_banking_monthly_less,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours),
      y = percentage,
      fill = primary_help_banking_hours
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Helped primary care receiver with banking - Less than once a month") +
    xlab("Time (hours)") +
    ylab("percentage") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_give_help_banking_monthly_less)
}

## Consequences of caregiving on the caregiver

### Out-of-pocket expenses because of caregiving responsibilities
chart_out_of_pocket_percent <- function(df_giver) {
  df_out_of_pocket <- tab_out_of_pocket(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(out_of_pocket_expenses))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_out_of_pocket <- ggplot(
    data = df_out_of_pocket,
    mapping = aes(
      x = fct_inorder(out_of_pocket_expenses),
      y = percentage,
      fill = out_of_pocket_expenses)) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=out_of_pocket_expenses, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Out-of-pocket expenses because of caregiving responsibilities") +
    xlab("Expense categories") +
    ylab("percentage") +
    scale_x_discrete(labels = str_wrap(df_out_of_pocket$out_of_pocket_expenses, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_out_of_pocket)
}

### Financial hardship
chart_financial_hardship_percent <- function(df_giver) {
  df_financial_hardship <- tab_financial_hardship(df_giver)
  hcl <- farver::decode_colour(viridisLite::magma(length(unique(financial_hardship))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_financial_hardship <- ggplot(
    data = df_financial_hardship,
    mapping = aes(
      x = fct_inorder(financial_hardship),
      y = percentage,
      fill = financial_hardship
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=financial_hardship, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Financial hardship because of caregiving (65+) responsibilities from 735 caregivers") +
    xlab("Expense categories") +
    ylab("percentage") +
    scale_x_discrete(labels = str_wrap(df_financial_hardship$financial_hardship, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "magma") +
    guides(fill = "none")

  return(c_financial_hardship)
}