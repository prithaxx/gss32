# group_by_sex(): Returns a chart where the y variable *counts* are grouped by sex
# df_input (tibble): data frame to be transformed
# y (function): checks which function from 03_var_y.R is to be used
# input: the vector on which table is made
# code: column code
# x_lab (string): x-axis label
# title_lab (string): title for the chart that is returned
group_by_sex <- function(df_input, y, input, code, x_lab, title_lab) {
  
  # if y is null then we are using a single table maker or else we are using multi-var table maker
  if(is.null(y)){
    df <- tab_maker(df_input, input, code)
  } else{
    df <- tab_multi_var_maker(df_input, input, code, y)
  }
  
  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = names(filter_sex[2:3]), names_to = "sex", values_to = "Count")
  
  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = Count, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Number", title_lab, "by sex"),
      x = x_lab,
      y = "Number of People"
    ) +
    scale_x_discrete(labels = str_wrap(df[[1]], width = 12)) +
    scale_fill_viridis_d(begin = 0.2, end = 0.8)
  
  return(chart_output)
}


# group_by_sex_percent(): Returns a chart where the y variable as a *percentage* are grouped by sex
# df_input (tibble): data frame to be transformed
# tab_option (function): the function from the receiver_tabs list to transform df_input
# x_lab (string): x-axis label
# title_lab (string): title for the chart that is returned
group_by_sex_percent <- function(df_input, y, input, code, x_lab, title_lab) {
  if(is.null(y)){
    df <- tab_maker(df_input, input, code)
  } else{
    df <- tab_multi_var_maker(df_input, input, code, y)
  }

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = c(male_percentage, female_percentage), names_to = "sex", values_to = "sex_percent")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = sex_percent, fill = sex)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Percentage", title_lab, "by sex"),
      x = x_lab,
      y = "Percentage of people"
    ) +
    scale_x_discrete(labels = str_wrap(df[[1]], width = 12)) +
    scale_fill_viridis_d(begin = 0.2, end = 0.8)

  return(chart_output)
}

group_by_age <- function(df_input, y, input, code, x_lab, title_lab) {
  if(is.null(y)){
    df <- tab_maker(df_input, input, code)
  } else{
    df <- tab_multi_var_maker(df_input, input, code, y)
  }
  
  df_long <- pivot_longer(df, cols = c("age_65_74", "age_75"), names_to = "age_group", values_to = "age_count")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = age_count, fill = age_group)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Number", title_lab, "by age"),
      x = x_lab,
      y = "Number of People"
    ) +
  scale_x_discrete(labels = str_wrap(df[[1]], width = 12)) +
    scale_fill_viridis_d(begin = 0.2, end = 0.8)

  return(chart_output)
}

group_by_age_percent <- function(df_input, y, input, code, x_lab, title_lab) {
  if(is.null(y)){
    df <- tab_maker(df_input, input, code)
  } else{
    df <- tab_multi_var_maker(df_input, input, code, y)
  }
  
  df_long <- pivot_longer(df,
    cols = c("age_65_74_percentage", "age_75_percentage"), names_to = "age_group",
    values_to = "age_group_percent"
  )

  chart_output <- ggplot(df_long, aes(
    x = fct_inorder(.data[[names(df_long)[1]]]), y = age_group_percent, fill =
      age_group
  )) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Number", title_lab, "by age"),
      x = x_lab,
      y = "Percentage of People"
    ) +
    scale_x_discrete(labels = str_wrap(df[[1]], width = 12)) +
    scale_fill_viridis_d(begin = 0.2, end = 0.8)

  return(chart_output)
}

group_by_alzheimers <- function(df_input, y, input, code, x_lab, title_lab) {
  if(is.null(y)){
    df <- tab_maker(df_input, input, code)
  } else{
    df <- tab_multi_var_maker(df_input, input, code, y)
  }

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = c(alzheimers, non_alzheimers), names_to = "alzheimers_count", values_to = "Count")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = Count, fill = alzheimers_count)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Number", title_lab, "by whether or not the care recipient has Alzheimer's/dementia"),
      x = x_lab,
      y = "Number of People"
    ) +
    scale_x_discrete(labels = str_wrap(df[[1]], width = 12)) +
    scale_fill_viridis_d(begin = 0.2, end = 0.8)

  return(chart_output)
}

group_by_alzheimers_percent <- function(df_input, y, input, code, x_lab, title_lab) {
  if(is.null(y)){
    df <- tab_maker(df_input, input, code)
  } else{
    df <- tab_multi_var_maker(df_input, input, code, y)
  }

  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = c(alzheimers_percentage, non_alzheimers_percentage), names_to = "alzheimers_group", values_to = "alzheimers_group_percent")

  chart_output <- ggplot(df_long, aes(x = fct_inorder(.data[[names(df_long)[1]]]), y = alzheimers_group_percent, fill = alzheimers_group)) +
    geom_col(position = "dodge") +
    labs(
      title = paste("Percentage", title_lab, "by whether or not the care recipient has Alzheimer's/dementia"),
      x = x_lab,
      y = "Percentage of people"
    ) +
    scale_x_discrete(labels = str_wrap(df[[1]], width = 12)) +
    scale_fill_viridis_d(begin = 0.2, end = 0.8)

  return(chart_output)
}
