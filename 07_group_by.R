# df <- tab_health_conditions(df_receiver)



# create the ggplot object
group_by_sex <- function(df) {
  df <- tab_health_conditions(df)
  
  # convert the data from wide to long format
  df_long <- pivot_longer(df, cols = names(filter_sex[2:3]), names_to = "sex", values_to = "Count")
  print(df_long)
  
  chart_output <- ggplot(df_long, aes(x = fct_inorder(health_conditions), y = Count, fill = sex)) +
    geom_col( position = "dodge") +
    labs(
      title = "Number of People with Health Conditions by Sex",
      x = "Health Conditions",
      y = "Number of People"
    ) 
    
  return(chart_output)
}