library(conflicted)
library(tidyverse)
library(haven)
library(rlang)
library(viridis)
conflicts_prefer(dplyr::filter)

source("01_main.R")
source("02_var_x.R")
source("03_var_y.R")
source("04_general_df.R")
source("05_table.R")
source("06_percentage.R")
source("07_group_by.R")

or_filter <- function(filters) {

}

# apply_filter(): takes a frame and filter based on option selected
# df_input (tibble): data frame to be transformed
# select_option (integer, character): filter value mapped to the response
#     character selctions are split by comma and used to "or" multiple filters
# col_name (String): variable to filter by
apply_filter <- function(df_input, select_option, col_name) {
  filtered_df <- if (select_option == -1) {
    df_input
  } else if (is.numeric(select_option)) {
    # the value from the list: e.g. both sexes = -1, male = 1, female = 2
    filter <- quo(!!as.symbol(col_name) == !!select_option)
    df_input %>% filter(!!filter)
  } else if (is.character(select_option)) {
    # Split comma separated options, craft a filter string with comparisons
    # separated by "|", convert back to expression and evaluate. Gross, but I
    # guess this is just R things?
    select_options <- as.integer(unlist(strsplit(select_option, ",")))
    filter_str <- paste(
      map(
        select_options,
        function(x) expr_text(expr(!!as.symbol(col_name) == !!x))
      ),
      collapse = "|"
    )
    df_input %>% filter(eval_tidy(parse_expr(filter_str)))
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
  }))
}

total_receiver_male <- nrow(apply_filter(df_receiver, 1, "SEX"))
total_receiver_female <- nrow(apply_filter(df_receiver, 2, "SEX"))
total_giver_male <- nrow(apply_filter(df_giver, 1, "SEX"))
total_giver_female <- nrow(apply_filter(df_giver, 2, "SEX"))

# General Charts ####

## Respondent groups ####
phcl <- farver::decode_colour(viridisLite::viridis(length(unique(df_pops$pop_name))), "rgb", "hcl") 
plabel_col <- ifelse(phcl[, "l"] > 50, "black", "white") 
c_respondent_groups <- ggplot(
  data = df_pops,
  mapping = aes(x = fct_inorder(pop_name), y = pop_freq, fill = pop_name)
) +
  geom_col() +
  geom_text(aes(color=pop_name, label = pop_freq), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  ggtitle("GSS 2018 repsondent groups") +
  labs(
    x = "Respondent group",
    y = "Count",
    caption = str_wrap("Count of respondents in each grouping: caregivers, care receivers, and persons with unmet caregiving needs.", width = 115)
  ) +
  scale_x_discrete(labels = str_wrap(df_pops$pop_name, width = 15)) +
  scale_color_manual(values = plabel_col) + 
  scale_fill_viridis_d(begin = 0.2, end = 0.8, option  = "viridis") +
  theme(axis.text.x = element_text(size=13), axis.title.x = element_blank()) +
  guides(fill = "none") +
  theme(plot.caption = element_text(hjust = 0, size = 14)) 

## Sex of primary caregiver and primary care receiver ####
shcl <- farver::decode_colour(viridisLite::viridis(length(unique(df_primary_sex$sex))), "rgb", "hcl") 
slabel_col <- ifelse(shcl[, "l"] > 50, "black", "white") 

c_primary_sex <- ggplot(
  data = df_primary_sex,
  mapping = aes(x = sex, y = freq, fill = sex)
) +
  geom_col() +
  geom_text(aes(color=sex,label = freq), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  ggtitle("Primary Care Giver and Receiver by Sex (age 65+)") +
  labs(caption = str_wrap("Top row. Caree sex, as reported by caregiver respondents. Bottom row. Caregiver Sex as reported by care receiver respondents", width = 115)) +
  xlab("Sex") +
  ylab("Count") +
  facet_wrap(~type, ncol = 1) +
#  scale_fill_viridis_d(begin = 0.2, end = 0.8) +    
  scale_color_manual(values = slabel_col) + 
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  theme(axis.text.x = element_text(size=13)) +
  guides(fill = "none") +
  theme(plot.caption = element_text(hjust = 0, size = 14))


### Relationship between Caree and Receiver
rhcl <- farver::decode_colour(viridisLite::viridis(length(unique(df_caree_relationship_pops$caree_relationship))), "rgb", "hcl") 
rlabel_col <- ifelse(rhcl[, "l"] > 50, "black", "white") 
c_caree_groups <- ggplot(
  data = df_caree_relationship_pops,
  mapping = aes(x = fct_inorder(caree_relationship), y = y_caree_freq(df_caree_relations), fill = caree_relationship)
) +
  geom_col() +
  geom_text(aes(color=caree_relationship, label = y_caree_freq(df_caree_relations)), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  ggtitle("GSS 2018 Relationship between Caree and Receiver") +
  labs(
    x = "Caree Relationships",
    y = "Count",
    caption = str_wrap("Count of respondents in each grouping: Spouse/Partner, Son, Daughter, Parent, Other Family Members, Other.", width = 115)
  ) +
  scale_x_discrete(labels = str_wrap(df_caree_relationship_pops$caree_relationship, width = 15)) +
  scale_color_manual(values = rlabel_col) + 
  scale_fill_viridis_d(begin = 0.2, end = 0.8, option  = "viridis") +
  theme(axis.text.x = element_text(size=13), axis.title.x = element_blank()) +
  guides(fill = "none") +
  theme(plot.caption = element_text(hjust = 0, size = 14)) 



# Care receiver responses #####
chart_health_conditions <- function(df_receiver) {
  df_health_conditions <- tab_health_conditions(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(health_conditions))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_health_conditions <- ggplot(
    data = df_health_conditions,
    mapping = aes(
      x = fct_inorder(health_conditions),
      y = count,
      fill = health_conditions
    )
  ) +
    geom_col() +
    geom_text(aes(color=health_conditions, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    labs(caption = str_wrap("Count for main health conditions for which respondents considered to be a care receiver and 65 years of age or older received help.", width = 115)) +
    xlab("Health Condition") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_health_conditions$health_conditions, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d() +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_health_conditions)
}


### Types of activities respondents received help with
chart_activity_receive_help <- function(df_receiver) {
  df_activity_receive_help <- tab_activity_receive_help(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_activity_receive_help <- ggplot(
    data = df_activity_receive_help,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = count,
      fill = help_activities
    )
  ) +
    geom_col() +
    geom_text(aes(color=help_activities, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received with from family, friends or neighbours in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_receive_help$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_activity_receive_help)
}

### Age of respondent's primary caregiver
chart_age_primary_giver <- function(df_receiver) {
  df_age_primary_giver <- tab_age_primary_giver(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(giver_age_group))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_age_primary_giver <- ggplot(
    data = df_age_primary_giver,
    mapping = aes(
      x = fct_inorder(giver_age_group),
      y = count,
      fill = giver_age_group
    )
  ) +
    geom_col() +
    geom_text(aes(color=giver_age_group,label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Age of respondent's primary caregivers") +
    labs(caption = str_wrap("Count of the age (groups of 5) of primary caregivers for respondents considered to be a care receiver and 65 years of age or older.", width = 120)) +
    xlab("Age (years)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  return(c_age_primary_giver)
}

### Types of activities respondents received professional help with
chart_activity_receive_help_pro <- function(df_receiver) {
  df_activity_receive_help_pro <- tab_activity_receive_help_pro(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_activity_receive_help_pro <- ggplot(
    data = df_activity_receive_help_pro,
    mapping = aes(
      x = fct_inorder(help_activities),
      y = count,
      fill = help_activities
    )
  ) +
    geom_col() +
    geom_text(aes(color=help_activities, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Activities received professional help with - Past 12 months") +
    labs(caption = str_wrap("Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received help from a professional in the past 12 months.", width = 120)) +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_receive_help_pro$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  return(c_activity_receive_help_pro)
}


### Numbers of hours of help received - Per average week per activity
chart_hours_help_received <- function(df_receiver) {
  df_hours_help_received <- tab_hours_help_received(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_hours_help_received <- ggplot(
    data = df_hours_help_received,
    mapping = aes(
      x = help_hours,
      y = count,
      fill = help_hours
    )
  ) +
    geom_col() +
    geom_text(aes(color=help_hours, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Numbers of hours of help received per week") +
    labs(caption = str_wrap("Count for the number of hours of help received, average per week, from family, friends or neighbours in the past 12 months.", width = 115)) +
    xlab("Time (hour)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_hours_help_received)
}

### Distance between the respondent's and the caregiver's dwellings
chart_primary_giver_distance <- function(df_receiver) {
  df_primary_giver_distance <- tab_primary_giver_distance(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(dwelling_distances))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_primary_giver_distance <- ggplot(
    data = df_primary_giver_distance,
    mapping = aes(
      x = fct_inorder(dwelling_distances),
      y = count,
      fill = dwelling_distances
    )
  ) +
    geom_col() +
    geom_text(aes(color=dwelling_distances, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Distance between the respondent's and caregiver's dwellings") +
    labs(caption = str_wrap("Counts for the distance by car between respondents considered to be a care receiver and 65 years of age or older, and their primary caregiver during the time they were receiving help in the past 12 months.", width = 115)) +
    xlab("Distance (time)") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_primary_giver_distance$dwelling_distances, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_primary_giver_distance)
}

### Primary caregiver helped with banking - Frequency
chart_receive_help_banking_freq <- function(df_receiver) {
  df_receive_help_banking_freq <- tab_receive_help_banking_freq(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_freq))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_receive_help_banking_freq <- ggplot(
    data = df_receive_help_banking_freq,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq),
      y = count,
      fill = primary_help_banking_freq
    )
  ) +
    geom_col() +
    geom_text(aes(color=primary_help_banking_freq, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Primary caregiver helped with banking - Frequency") +
    labs(caption = str_wrap("Count for how often respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.", width = 120)) +
    xlab("Help Frequency") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_receive_help_banking_freq)
}

### Primary caregiver helped with banking - Number of hours
chart_receive_help_banking_hours <- function(df_receiver) {
  df_receive_help_banking_hours <- tab_receive_help_banking_hours(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_receive_help_banking_hours <- ggplot(
    data = df_receive_help_banking_hours,
    mapping = aes(
      x = primary_help_banking_hours,
      y = count,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    geom_text(aes(color=primary_help_banking_hours, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Primary caregiver helped with banking - Number of hours") +
    labs(caption = str_wrap("Count for the number of hours respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.", width = 115)) +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_receive_help_banking_hours)
}

### Respondent did not receive the care needed - Reasons
chart_nohelp_received <- function(df_receiver){
  df_nohelp_reasons <- tab_received_nohelp(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(received_nohelp_reasons))), "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_nohelp_reasons <- ggplot(
    data = df_nohelp_reasons,
    mapping = aes(
      x = fct_inorder(received_nohelp_reasons),
      y = count,
      fill = received_nohelp_reasons
    )
  ) +
    geom_col() +
    geom_text(aes(color=received_nohelp_reasons, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Respondent did not receive the care they needed - Reasons") +
    labs(caption = str_wrap("Count for the main reasons why respondents do not receive the care they need", width = 115)) +
    xlab("Reasons") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_nohelp_reasons$received_nohelp_reasons, width = 12)) +
    scale_color_manual(values = label_col) +
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_nohelp_reasons)
}

### Respondents with disability indicators 
chart_receiver_disability_indicator <- function(df_receiver) {
  df_disability_indicator <- tab_disability_indicator(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(disability_indicators))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_disability_indicator <- ggplot(
    data = df_disability_indicator,
    mapping = aes(
      x = fct_inorder(disability_indicators),
      y = count,
      fill = disability_indicators
    )
  ) +
    geom_col() +
    geom_text(aes(color=disability_indicators, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Respondents with Disability Indicators") +
    labs(caption = str_wrap("Frequency of the type of Disability Indicators within Respondents", width = 120)) +
    xlab("Types of Disability Indicators") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_disability_indicator$disability_indicators, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(c_disability_indicator)
}

### Services/ People who cared for the Respondent
chart_caree_type <- function(df_receiver) {
  df_caree_type <- tab_caree_type(df_receiver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(caree_type))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_caree_type <- ggplot(
    data = df_caree_type,
    mapping = aes(
      x = fct_inorder(caree_type),
      y = count,
      fill = caree_type
    )
  ) +
    geom_col() +
    geom_text(aes(color=caree_type, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Services/ people who cared for the Respondent's health condition") +
    labs(caption = str_wrap("Frequency of the type of caree (Friends/Family, Professionals or Both) ", width = 120)) +
    xlab("Types of Caree") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_caree_type$caree_type, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(c_caree_type)
}

# Caregiver responses ####

### Types of activities respondents provided help with
chart_activity_give_help <- function(df_giver) {
  df_activity_give_help <- tab_activity_give_help(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_activities))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_activity_give_help <- ggplot(data = df_activity_give_help, mapping = aes(
    x = fct_inorder(help_activities),
    y = count,
    fill = help_activities
  )) +
    geom_col() +
    geom_text(aes(color=help_activities, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Types of activities respondents provided help with - Past 12 months") +
    xlab("Activity") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_activity_give_help$help_activities, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none")+
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_activity_give_help)
}


### Age of respondent's care receiver
chart_age_primary_receiver <- function(df_giver) {
  df_age_primary_receiver <- tab_age_primary_receiver(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_receiver_age_group))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_age_primary_receiver <- ggplot(
    data = df_age_primary_receiver,
    mapping = aes(
      x = fct_inorder(primary_receiver_age_group),
      y = count,
      fill = primary_receiver_age_group
    )
  ) +
    geom_col() +
    geom_text(aes(color=primary_receiver_age_group, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Age of primary care receiver") +
    xlab("Age Group (years)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none")+
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_age_primary_receiver)
}


# ### Number of hours are or help provided by respondent - Per average week
chart_hours_help_provided <- function(df_giver) {
  df_hours_help_provided <- tab_hours_help_provided(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(help_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_hours_help_provided <- ggplot(
    data = df_hours_help_provided,
    mapping = aes(
      x = fct_inorder(help_hours),
      y = count,
      fill = help_hours
    )
  ) +
    geom_col() +
    geom_text(aes(color=help_hours, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Numbers of hours of help provided - Per average week per activity") +
    xlab("Time (hours)") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none")+
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_hours_help_provided)
}

# ### Distance between the respondent's and the care receiver's dwellings PRD_10
chart_primary_receiver_distance <- function(df_giver) {
  df_primary_receiver_distance <- tab_primary_receiver_distance(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(dwelling_distances))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_primary_receiver_distance <- ggplot(
    data = df_primary_receiver_distance,
    mapping = aes(
      x = fct_inorder(dwelling_distances),
      y = count,
      fill = dwelling_distances
    )
  ) +
    geom_col() +
    geom_text(aes(color=dwelling_distances, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Distance between the respondent's and carereceiver's dwellings") +
    xlab("Distance by car") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none")+
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_primary_receiver_distance)
}

### Helped primary care receiver with banking - Frequency
chart_give_help_banking_freq <- function(df_giver) {
  df_give_help_banking_freq <- tab_give_help_banking_freq(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_freq))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_give_help_banking_freq <- ggplot(
    data = df_give_help_banking_freq,
    mapping = aes(
      x = fct_inorder(primary_help_banking_freq),
      y = count,
      fill = primary_help_banking_freq
    )
  ) +
    geom_col() +
    geom_text(aes(color=primary_help_banking_freq, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Helped primary care receiver with banking - Frequency") +
    xlab("Help Frequency") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none")+
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_give_help_banking_freq)
}

### Helped primary care receiver with banking - Number of hours
chart_give_help_banking_hours <- function(df_giver) {
  df_give_help_banking_hours <- tab_give_help_banking_hours(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(primary_help_banking_hours))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_give_help_banking_hours <- ggplot(
    data = df_give_help_banking_hours,
    mapping = aes(
      x = fct_inorder(primary_help_banking_hours),
      y = count,
      fill = primary_help_banking_hours
    )
  ) +
    geom_col() +
    geom_text(aes(color=primary_help_banking_hours, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Primary caregiver helped with banking - Number of hours") +
    xlab("Hours helped") +
    ylab("Count") +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none")+
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_give_help_banking_hours)
}

## Consequences of caregiving on the caregiver

### Out-of-pocket expenses because of caregiving responsibilities
chart_out_of_pocket <- function(df_giver) {
  df_out_of_pocket <- tab_out_of_pocket(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(out_of_pocket_expenses))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_out_of_pocket <- ggplot(
    data = df_out_of_pocket,
    mapping = aes(
      x = fct_inorder(out_of_pocket_expenses),
      y = count,
      fill = out_of_pocket_expenses
    )
  ) +
    geom_col() +
    geom_text(aes(color=out_of_pocket_expenses, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Out-of-pocket expenses because of caregiving responsibilities") +
    xlab("Expense categories") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_out_of_pocket$out_of_pocket_expenses, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none")+
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_out_of_pocket)
}

### Financial hardship
chart_financial_hardship <- function(df_giver) {
  df_financial_hardship <- tab_financial_hardship(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(financial_hardship))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_financial_hardship <- ggplot(
    data = df_financial_hardship,
    mapping = aes(
      x = fct_inorder(financial_hardship),
      y = count,
      fill = financial_hardship
    )
  ) +
    geom_col() +
    geom_text(aes(color=financial_hardship, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Financial hardship because of caregiving (65+) responsibilities from 735 caregivers") +
    xlab("Expense categories") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_financial_hardship$financial_hardship, width = 13)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))

  return(c_financial_hardship)
}

chart_giver_disability_indicator <- function(df_giver) {
  df_disability_indicator <- tab_disability_indicator(df_giver)
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(disability_indicators))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  c_disability_indicator <- ggplot(
    data = df_disability_indicator,
    mapping = aes(
      x = fct_inorder(disability_indicators),
      y = count,
      fill = disability_indicators
    )
  ) +
    geom_col() +
    geom_text(aes(color=disability_indicators, label = count), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle("Respondents with Disability Indicators") +
    labs(caption = str_wrap("Frequency of the type of Disability Indicators within Respondents", width = 120)) +
    xlab("Types of Disability Indicators") +
    ylab("Count") +
    scale_x_discrete(labels = str_wrap(df_disability_indicator$disability_indicators, width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(c_disability_indicator)
}
