receiver_x_options <- list(
  health_conditions,
  help_activities,
  giver_age_group,
  primary_receiver_age_group,
  help_hours,
  dwelling_distances,
  primary_help_banking_freq,
  primary_help_banking_hours
)

receiver_x_lab <- list(
  "Health Conditions"

)


group_by_sex <- function(df_input, tab_option,  x_lab) {
  df <- tab_option(df_input)

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = names(filter_sex[2:3]), names_to = "sex", values_to = "Count")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = Count, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = "Number of People with Health Conditions by Sex",
      x = x_lab,
      y = "Number of People"
    )

  return(chart_output)
}

# group_by_percent

group_by_sex_percent <- function(df) {
  df <- tab_health_conditions(df)
  
  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = c(male_percentage, female_percentage), names_to = "sex", values_to = "sex_percent")
  # print(df_long)
 
  chart_output <- ggplot(df_long, aes(x = fct_inorder(health_conditions), y = sex_percent, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = "Number of People with Health Conditions by Sex",
      x = "Health Conditions",
      y = "Number of People"
    ) 
  
  return(chart_output)
}