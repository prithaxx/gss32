receiver_group_by_titles <- list(
  "of People with Health Conditions",
  "of People who Received Help with an Activity",
  "of people and the Age of Respondent's Primary Caregiver",
  "of People who Received Professional Help with an Activity",
  "of People and the Number of Hours of Help Received - Per Average Week",
  "of People and the Distance Between the Respondent and the Caregiver's Dwellings",
  "of People and the Frequency Their Primary Caregiver Helped with Banking",
  "of People and Number of Hours their Primary Caregiver Helped with Banking",
  "of People and Number of Hours their Primary Caregiver Helped with Banking Daily",
  "of People and Number of Hours their Primary Caregiver Helped with Banking Weekly",
  "of People and Number of Hours their Primary Caregiver Helped with Banking Monthly",
  "of People and Number of Hours their Primary Caregiver Helped with Banking Less Than Once a Month"
)

giver_group_by_titles <- list(
  "of People who Provided Help with an Activity",
  "of people and the Age of Respondent's Primary Care Receiver",
  "of People and the Number of Hours of Help Provided - Per Average Week",
  "of People and the Distance Between them and the Care Receiver's Dwellings",
  "of People and the Frequency they Provided Help to Their Primary Care Receiver with Banking",
  "of People and Number of Hours they Provided Help with Banking",
  "of People and Number of Hours they Provided Help with Banking Daily",
  "of People and Number of Hours they Provided Help with Banking Weekly",
  "of People and Number of Hours they Provided Help with Banking Monthly",
  "of People and Number of Hours they Provided Help with Banking Less Than Once a Month",
  "of People who had out-of-pocket Expenses From Caregiving- Past 12 months",
  "who Experienced Financial Hardship Because of Caregiving Responsibilities"
)

group_by_age <- function(df_input) {




}

group_by_age_percent <- function(df_input) {

}

# group_by_sex(): Returns a chart where the y variable *counts* are grouped by sex
# df_input (tibble): data frame to be transformed
# tab_option (function): the function from the receiver_tabs list to transform df_input
# x_lab (string): x-axis label
# title_lab (string): title for the chart that is returned
group_by_sex <- function(df_input, tab_option, x_lab, title_lab) {
  df <- tab_option(df_input)

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = names(filter_sex[2:3]), names_to = "sex", values_to = "Count")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = Count, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Number", title_lab, "by sex"),
      x = x_lab,
      y = "Number of People"
    )

  return(chart_output)
}

# group_by_sex_percent(): Returns a chart where the y variable as a *percentage* are grouped by sex
# df_input (tibble): data frame to be transformed
# tab_option (function): the function from the receiver_tabs list to transform df_input
# x_lab (string): x-axis label
# title_lab (string): title for the chart that is returned
group_by_sex_percent <- function(df_input, tab_option, x_lab, title_lab) {
  df <- tab_option(df_input)

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = c(male_percentage, female_percentage), names_to = "sex", values_to = "sex_percent")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = sex_percent, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Percentage", title_lab, "by sex"),
      x = x_lab,
      y = "Percentage of people"
    )

  return(chart_output)
}
