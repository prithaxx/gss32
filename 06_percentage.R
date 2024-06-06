# -------- "GENERAL" PERCENTAGE CHART FUNCTION -----------------
# input : vector on which we are working. 
# frequency : vector of counts from var_y
# caption : title of the chart
# x_axis : labels the x axis
# y_axis : labels the y_axis
# --------------------------------------------------------------
chart_general_pct <- function(input, frequency, title, caption, x_axis, y_axis){
  df <- tab_general(input, frequency)
  f <- fct_inorder(factor(input))
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(df$input))), "rgb", "hcl")
  label_col <- ifelse(hcl[,"l"] > 50, "black", "white")
  
  c <- ggplot(
    data = df,
    mapping = aes(
      x = f,
      y = percentage,
      fill = f
    )
  ) +
    geom_col()+
    ylim(0,1)+
    geom_text(aes(color=f, label=round(percentage,2)), position=position_stack(vjust=0.5), show.legend = FALSE) +
    ggtitle(title) +
    labs(caption = str_wrap(caption, width = 115)) +
    xlab(x_axis) +
    ylab(y_axis) +
    scale_x_discrete(labels = str_wrap(df$input, width = 15)) +
    scale_color_manual(values = label_col) +
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13), axis.title.x = element_blank()) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(c)
}

#-----------GENERAL PERCENTAGE CHART FUNCTION-------------
# df : data-frame
# input : the vector from 02_var_x.R
# code : The column code from the original dataset
# y : Which y function is being used from 03_var_y.R
# caption : title of the chart
# x_axis : labels the x axis
# y_axis : labels the y_axis
#---------------------------------------------------------
chart_pct <- function(df, input, code, y, title, caption, x_axis, y_axis){
  df <- tab_chooser(df, input, code, y)
  f <- fct_inorder(factor(input))
  
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(input))), "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  
  c <- ggplot(
    data = df,
    mapping = aes(
      x = f,
      y = percentage,
      fill = f
    )
  ) +
    geom_col() +
    ylim(0, 1) +
    geom_text(aes(color=f, label = round(percentage, 2)), position = position_stack(vjust = 0.5), show.legend=FALSE) +
    ggtitle(title) +
    labs(caption = str_wrap(caption, width = 115)) +
    xlab(x_axis) +
    ylab(y_axis) +
    scale_x_discrete(labels = str_wrap(df$x_options, width = 12)) +
    scale_color_manual(values = label_col) +
    scale_fill_viridis_d(option  = "viridis") +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return(c)
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