group_by_titles <- list(
  "Number of People with Health Conditions by Sex",
  "Number of People who Received Help with an Activity by Sex",
  "Age of Respondent's Primary Caregiver by Sex",
  "Number of People who Received Professional Help with an Activity by Sex",
  "Number of People and the Number of Hours of Help Received - Per Average Week by Sex",
  "Number of People and the Distance Between the Respondent and the Caregiver's Dwellings by Sex",
  "Number of People and the Frequency Their Primary Caregiver Helped with Banking by Sex",
  "Number of People and Number of Hours the Primary Caregiver Helped with Banking by Sex",
  "Number of People and Number of Hours the Primary Caregiver Helped with Banking Daily by Sex",
  "Number of People and Number of Hours the Primary Caregiver Helped with Banking Weekly by Sex",
  "Number of People and Number of Hours the Primary Caregiver Helped with Banking Monthly by Sex",
  "Number of People and Number of Hours the Primary Caregiver Helped with Banking Less Than Once a Month by Sex"
)

# group_by_sex():
# df_input (tibble):
# tab_option (function):
# x_lab (string):
group_by_sex <- function(df_input, tab_option,  x_lab, title_lab) {
  df <- tab_option(df_input)

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = names(filter_sex[2:3]), names_to = "sex", values_to = "Count")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = Count, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = title_lab,
      x = x_lab,
      y = "Number of People"
    )

  return(chart_output)
}

# group_by_percent
group_by_sex_percent <- function(df_input, tab_option, x_lab, title_lab) {
  df <- tab_option(df_input)

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = c(male_percentage, female_percentage), names_to = "sex", values_to = "sex_percent")
  # print(df_long)

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = sex_percent, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = title_lab,
      x = x_lab,
      y = "Percentage of people"
    )

  return(chart_output)
}
