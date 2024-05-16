# Percentage charts
chart_respondent_groups_percent <- function() {
  df <- tab_pop_freq()
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(pop_name))), "rgb", "hcl") 
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
    labs(caption = str_wrap("Proportion of respondents in each grouping: caregivers, care receivers, and persons with unmet caregiving needs.", width = 115)) +
    xlab("Respondent group") +
    ylab("Proportion of Respondents") +
    scale_x_discrete(labels = str_wrap(df$pop_name, width = 15)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13), axis.title.x = element_blank()) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(chart)
}


chart_health_conditions_percent <- function(df_receiver) {
  df <- tab_health_conditions(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(health_conditions))), "rgb", "hcl") 
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
    labs(caption = str_wrap("Proportion of care receiver respondents reporting item as their main health condition.", width = 115)) +
    xlab("Health Condition") +
    ylab("Proportion of Care Receiver Respondents (65+)") +
    scale_x_discrete(labels = str_wrap(df$health_conditions, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(chart)
}

### Types of activities respondents received help with
chart_activity_receive_help_percent <- function(df_receiver) {
  df <- tab_activity_receive_help(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = percentage, #TODO - fix - denominator should be number of receiver respondents (not total counts of activities)
      fill = help_activities
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_activities, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Proportion of care receiver respondents who report receiving help from family, friends or neighbours in the past 12 months with each type of activity.", width = 120)) +
    xlab("Activity") +
    ylab("Proportion of Care Receiver Respondents (65+)") +
    scale_x_discrete(labels = str_wrap(df$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(chart)
}

### Respondents with disability indicators 
chart_receiver_disability_indicator_percent <- function(df_receiver) {
  df_disability_indicator <- tab_disability_indicator(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(disability_indicators))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
  chart <- ggplot(
    data = df_disability_indicator,
    mapping = aes(
      x = fct_inorder(disability_indicators),
      y = percentage,
      fill = disability_indicators
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=disability_indicators, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Respondents with Disability Indicators") +
    labs(caption = str_wrap("Proportion of the type of Disability Indicators within Respondents", width = 120)) +
    xlab("Types of Disability Indicators") +
    ylab("Proportion of Care Receivers with a type of Disability") +
    scale_x_discrete(labels = str_wrap(df_disability_indicator$disability_indicators, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(chart)
}

### Age of respondent's primary caregiver
chart_age_primary_giver_percent <- function(df_receiver) {
  df <- tab_age_primary_giver(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(giver_age_group))), "rgb", "hcl") 
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
    labs(caption = str_wrap("Proportion of care receiver respondents reporting their primary caregiver's age.", width = 120)) +
    xlab("Age (years)") +
    ylab("Proportion") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_age_primary_giver)
}

# ### Types of activities respondents received professional help with
chart_activity_receive_help_pro_percent <- function(df_receiver) {
  df <- tab_activity_receive_help_pro(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_activity_receive_help_pro <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = percentage, #TODO - fix - denominator should be number of receiver respondents (not total counts of activities)
      fill = help_activities
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_activities, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received professional help with - Past 12 months") +
    labs(caption = str_wrap("Proportion of care receiver respondents reporting they received help from a professional in the past 12 months with each activity.", width = 120)) +
    xlab("Activity") +
    ylab("Proportion of Care Receiver Respondents (65+)") +
    scale_x_discrete(labels = str_wrap(df$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_activity_receive_help_pro)
}


### Numbers of hours of help received - Per average week per activity
chart_hours_help_received_percent <- function(df_receiver) {
  df <- tab_hours_help_received(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_hours))), "rgb", "hcl") 
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
    ggtitle("Numbers of hours of help received per week") +
    labs(caption = str_wrap("Proportion of care receiver respondents reporting average number of hours of help received, per week from family, friends or neighbours.", width = 115)) +
    xlab("Time (hour)") +
    ylab("Proportion of Care Receiver Respondents (65+)") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_hours_help_received)
}

### Distance between the respondent's and the caregiver's dwellings
chart_primary_giver_distance_percent <- function(df_receiver) {
  df <- tab_primary_giver_distance(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(dwelling_distances))), "rgb", "hcl") 
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
    labs(caption = str_wrap("Proportion of care receiver respondents reporting distance by car between themselves and their primary caregiver.", width = 115)) +
    xlab("Distance (time)") +
    ylab("Proportion of Care Receiver Respondents (65+)") +
    scale_x_discrete(labels = str_wrap(df$dwelling_distances, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_primary_giver_distance)
}

### Primary caregiver helped with banking - Frequency
chart_receive_help_banking_freq_percent <- function(df_receiver) {
  df <- tab_receive_help_banking_freq(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_freq))), "rgb", "hcl") 
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
    labs(caption = str_wrap("Of care receiver respondents reporting that they did receive help with managing their finances in the past 12 months, proportion reporting each frequency level of help.", width = 120)) +
    xlab("Help Frequency") +
    ylab("Proportion of Care Receiver Respondents (65+)") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_receive_help_banking_freq)
}

### Primary caregiver helped with banking - Number of hours
chart_receive_help_banking_hours_percent <- function(df_receiver) {
  df <- tab_receive_help_banking_hours(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
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
    labs(caption = str_wrap("Of care receiver respondents reporting that they did receive help with managing their finances in the past 12 months, proportion reporting number of hours of banking assistance received.", width = 115)) +
    xlab("Time (hours)") +
    ylab("Proportion of Care Receiver Respondents (65+)") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_receive_help_banking_hours)
}

### Respondent did not receive the care needed - reasons
chart_nohelp_received_percent <- function(df_receiver) {
  df <- tab_received_nohelp(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(received_nohelp_reasons))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(received_nohelp_reasons),
      y = percentage,
      fill = received_nohelp_reasons
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=received_nohelp_reasons, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Proportion of respondent who did not receive the care needed due to specific reasons") +
    labs(caption = str_wrap("Proportion of respondents who did not receive care needed with by reasons.", width = 115)) +
    xlab("Health Condition") +
    ylab("Proportion of Respondents (65+) not receiving care") +
    scale_x_discrete(labels = str_wrap(df$received_nohelp_reasons, width = 12)) +
    scale_color_manual(values = label_col) +
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(chart)
}

# giver percent charts ####

chart_activity_give_help_percent <- function(df) {
  df <- tab_activity_give_help(df)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
  chart <- ggplot(
    data = df,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = percentage, #help_activities/4677, #TODO: denominator here is wrong. It should be 4677 (number of caregiver respondents)
      fill = help_activities
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=help_activities, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Proportion of caregiver respondents who report providing help to caree in the past 12 months with each type of activity.", width = 120)) +
    xlab("Activity") +
    ylab("Proportion of Respondents") +
    scale_x_discrete(labels = str_wrap(df$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(chart)
}

chart_age_primary_receiver_percent <- function(df_giver) {
  df_age_primary_receiver <- tab_age_primary_receiver(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_receiver_age_group))), "rgb", "hcl") 
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
    ggtitle("Proportion of caregiver respondents providing assistance to caree of each age category") +
    xlab("Age Group (years)") +
    ylab("Proportion of Carees") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_age_primary_receiver)
}

chart_hours_help_provided_percent <- function(df_giver) {
  df_hours_help_provided <- tab_hours_help_provided(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_hours))), "rgb", "hcl") 
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
    ggtitle("Proportion of caregiver respondents who report providing their caree with specific hours of help per week over the past 12 months.") +
    xlab("Time (hours)") +
    ylab("Proportion of Caregiver Respondents") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_hours_help_provided)
}

chart_primary_receiver_distance_percent <- function(df_giver) {
  df_primary_receiver_distance <- tab_primary_receiver_distance(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(dwelling_distances))), "rgb", "hcl") 
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
    ggtitle("Proportion of caregiver respondents who report living specific distances from their caree's dwelling") +
    xlab("Distance by car") +
    ylab("Proportion of Caregiver Respondents") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_primary_receiver_distance)
}

### Helped primary care receiver with banking - Frequency
chart_give_help_banking_freq_percent <- function(df_giver) {
  df_give_help_banking_freq <- tab_give_help_banking_freq(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_freq))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_freq <- ggplot(
    data = df_give_help_banking_freq,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq),
      y = percentage, #TODO: denominator here is wrong. It should be 4677 (number of caregiver respondents)
      fill = primary_help_banking_freq
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_freq, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Of caregiver respondents reporting that they provided caree help with banking in the past 12 months, proportion reporting each frequency level of help.") +
    xlab("Frequency of Providing Caree Assistance with Banking Tasks") +
    ylab("Proportion of Caregiver Respondents") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_give_help_banking_freq)
}

### Helped primary care receiver with banking - Number of hours
chart_give_help_banking_hours_percent <- function(df_giver) {
  df_give_help_banking_hours <- tab_give_help_banking_hours(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_give_help_banking_hours <- ggplot(
    data = df_give_help_banking_hours,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours),
      y = percentage, #TODO: denominator here is wrong. It should be 4677 (number of caregiver respondents)
      fill = primary_help_banking_hours)) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=primary_help_banking_hours, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Of caregiver respondents reporting that they provided caree help with banking in the past 12 months, proportion reporting each weekly number of hours of help.") +
    xlab("Hours per Week Spent Providing Banking Task Assistance to Caree") +
    ylab("Proportion of Caregiver Respondents") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_give_help_banking_hours)
}


## Consequences of caregiving on the caregiver

### Out-of-pocket expenses because of caregiving responsibilities
chart_out_of_pocket_percent <- function(df_giver) {
  df_out_of_pocket <- tab_out_of_pocket(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(out_of_pocket_expenses))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_out_of_pocket <- ggplot(
    data = df_out_of_pocket,
    mapping = aes(
      x = fct_inorder(out_of_pocket_expenses),
      y = percentage, #TODO: denominator here is wrong. It should be 4677 (number of caregiver respondents)
      fill = out_of_pocket_expenses)) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=out_of_pocket_expenses, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Proportion of caregiver respondents who report out-of-pocket expenses due to each category of caregiving responsibilities.") +
    xlab("Expense categories") +
    ylab("Proportion of Caregiver Respondents") +
    scale_x_discrete(labels = str_wrap(df_out_of_pocket$out_of_pocket_expenses, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_out_of_pocket)
}

### Financial hardship
chart_financial_hardship_percent <- function(df_giver) {
  df_financial_hardship <- tab_financial_hardship(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(financial_hardship))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 

  c_financial_hardship <- ggplot(
    data = df_financial_hardship,
    mapping = aes(
      x = fct_inorder(financial_hardship),
      y = percentage, #TODO: denominator here is wrong. It should be 4677 (number of caregiver respondents) tried: ..count../4677, #
      fill = financial_hardship
    )) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=financial_hardship, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Proportion of caregiver respondents who report experiencing various forms of financial hardship because of caregiving (65+) responsibilities. Denominator: 735 caregiver respondents who report experiencing financial hardship.") +
    xlab("Expense categories") +
    ylab("Proportion of Caregiver Respondents") +
    scale_x_discrete(labels = str_wrap(df_financial_hardship$financial_hardship, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_financial_hardship)
}

### Respondents with disability indicators 
chart_giver_disability_indicator_percent <- function(df_giver) {
  df_disability_indicator <- tab_disability_indicator(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(disability_indicators))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
  chart <- ggplot(
    data = df_disability_indicator,
    mapping = aes(
      x = fct_inorder(disability_indicators),
      y = percentage,
      fill = disability_indicators
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=disability_indicators, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Respondents with Disability Indicators") +
    labs(caption = str_wrap("Proportion of the type of Disability Indicators within Respondents", width = 120)) +
    xlab("Types of Disability Indicators") +
    ylab("Proportion of Care Receivers with a type of Disability") +
    scale_x_discrete(labels = str_wrap(df_disability_indicator$disability_indicators, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(chart)
}