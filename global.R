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
## Sex of primary caregiver and primary care receiver ####
chart_general_sex <- function(df, title, caption, x_axis, y_axis){
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(df$sex))), "rgb", "hcl") 
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white") 
  
  c <- ggplot(
    data = df,
    mapping = aes(x = sex, y = freq, fill = sex)
  ) +
    geom_col(position = position_dodge()) +  # Use position_dodge() for dodged bars
    geom_text(aes(color=sex,label = freq), position = position_stack(vjust = 0.5), show.legend = FALSE) +
    ggtitle(title) +
    labs(caption = str_wrap(caption, width = 115)) +
    xlab(x_axis) +
    ylab(y_axis) +
    facet_wrap(~type, ncol = 1) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(begin = 0.2, end = 0.8) +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return (c)
}



# ---------------------- "GENERAL" CHART FUNCTION ----------------------
# input : vector on which we are working. 
# frequency : vector of counts from var_y
# title : Title of the chart
# caption : title of the chart
# x_axis : labels the x axis
# y_axis : labels the y_axis
# ----------------------------------------------------------------------
chart_general <- function(input, frequency, title, caption, x_axis, y_axis){
  df <- df_general(input, frequency)
  f <- fct_inorder(factor(input))
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(df$input))), "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  c <- ggplot(
    data = df,
    mapping = aes(
      x = f,
      y = frequency,
      fill = f
    )) +
    geom_col() +
    geom_text(aes(color = f, label = frequency), position = position_stack(vjust = 0.5), show.legend = FALSE) +
    ggtitle(title) +
    labs(caption = str_wrap(caption, width = 115)) +
    xlab(x_axis) +
    ylab(y_axis) +
    scale_x_discrete(labels = str_wrap(df$input, width = 15)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d(begin = 0.2, end = 0.8, option  = "viridis") +
    theme(axis.text.x = element_text(size=13), axis.title.x = element_blank()) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14)) 
  
  return (c)
}


### -------------------- GENERAL CHART FUNCTION ------------------------
# df : data-frame
# input : the vector from 02_var_x.R
# code : The column code from the original dataset
# y : Which y function is being used from 03_var_y.R
# caption : title of the chart
# x_axis : labels the x axis
# y_axis : labels the y_axis
### --------------------------------------------------------------------
chart <- function(df, input, code, y, title, caption, x_axis, y_axis){
  df <- tab_chooser(df, input, code, y)
  f <- fct_inorder(factor(input))
  hcl <- farver::decode_colour(viridisLite::viridis(length(unique(input))), "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  c <- ggplot(
    data = df,
    mapping = aes(
      x = f,
      y = count,
      fill = f,
    )
  ) +
    geom_col() +
    geom_text(aes(color=f, label=count), position = position_stack(vjust=0.5), show.legend=FALSE) +
    ggtitle(title) +
    labs(caption = str_wrap(caption, width = 115)) +
    xlab(x_axis) +
    ylab(y_axis) +
    scale_x_discrete(labels = str_wrap(factor(df$x_options), width = 12)) +
    scale_color_manual(values = label_col) + 
    scale_fill_viridis_d() +
    theme(axis.text.x = element_text(size=13)) +
    guides(fill = "none") +
    theme(plot.caption = element_text(hjust = 0, size = 14))
  
  return (c)
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