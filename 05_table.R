# tab_helper(): returns a data frame adding sex counts and percentage columns
# df (tibble): data frame to be transformed
# count (vector): y-axis values (quantitative)
# x_options (vector): x-axis values (categorical)
# cols (string): primary filter variable
# cols2 (string = NULL): secondary filter variable
# response_code (integer): mapped value to cols2
tab_helper <- function(df, count, x_options, cols, col2 = NULL, response_code) {
  start <- x_options[1]
  end <- x_options[length(x_options)]
  
  # the total frequencies are creating problems with the percentage denominators. 
  # Not removing them because I'm not sure where else these values are being used.
  total_male <- sum(df$SEX == 1)
  total_female <- sum(df$SEX == 2)
  total_age_65_74 <- sum(df$AGEGR10 == 6)
  total_age_75 <- sum(df$AGEGR10 == 7)
  total_alzheimers <- sum(df$PRP10GR == 8)
  total_non_alzheimers <- sum(df$PRP10GR != 8)

  tibble(x_options = names(x_options), count) %>%
    mutate(
      percentage = count / sum(count),
      Male = sapply(start:end, function(i) {
        if (!is.null(col2)) {
          sum(df$SEX == 1 & df[[cols]] == i & df[[col2]] == response_code)
        } else {
          sum(df$SEX == 1 & df[[cols]] == i)
        }
      }),
      Female = sapply(start:end, function(i) {
        if (!is.null(col2)) {
          sum(df$SEX == 2 & df[[cols]] == i & df[[col2]] == response_code)
        } else {
          sum(df$SEX == 2 & df[[cols]] == i)
        }
      }),
      male_percentage = round(Male / count, 2),
      female_percentage = round(Female / count, 2),
      age_65_74 = sapply(start:end, function(i) {
        if (!is.null(col2)) {
          sum(df$AGEGR10 == 6 & df[[cols]] == i & df[[col2]] == response_code)
        } else {
          sum(df$AGEGR10 == 6 & df[[cols]] == i)
        }
      }),
      age_75 = sapply(start:end, function(i) {
        if (!is.null(col2)) {
          sum(df$AGEGR10 == 7 & df[[cols]] == i & df[[col2]] == response_code)
        } else {
          sum(df$AGEGR10 == 7 & df[[cols]] == i)
        }
      }),
      age_65_74_percentage = round(age_65_74 / count, 2),
      age_75_percentage = round(age_75 / count, 2),
      alzheimers = sapply(start:end, function(i) {
        if (!is.null(col2)) {
          sum(df$PRP10GR == 8 & df[[cols]] == i & df[[col2]] == response_code)
        } else {
          sum(df$AGEGR10 == 6 & df[[cols]] == i)
        }
      }),
      non_alzheimers = sapply(start:end, function(i) {
        if (!is.null(col2)) {
          sum(df$PRP10GR != 8 & df[[cols]] == i & df[[col2]] == response_code)
        } else {
          sum(df$AGEGR10 != 6 & df[[cols]] == i)
        }
      }),
      alzheimers_percentage = round(alzheimers / count, 2),
      non_alzheimers_percentage =
        round(non_alzheimers / count, 2),
    )
}

# tab_helper_multi_var(): returns a data frame where x values are consists of multiple variables adding sex counts and
# percentage columns
# df (tibble): data frame to be transformed
# count (vector): y-axis values (quantitative)
# x_options (vector): x-axis values (categorical)
# cols (vector): primary filter variables where each value in x_options is mapped to a different column
tab_helper_multi_var <- function(df, count, x_options, cols) {
  total_male <- sum(df$SEX == 1)
  total_female <- sum(df$SEX == 2)
  total_age_65_74 <- sum(df$AGEGR10 == 6)
  total_age_75 <- sum(df$AGEGR10 == 7)
  total_alzheimers <- sum(df$PRP10GR == 8)
  total_non_alzheimers <- sum(df$PRP10GR != 8)

  tibble(x_options, count) %>%
    mutate(
      percentage = count / sum(count),
      Male = sapply(seq_along(x_options), function(i) {
        sum(df$SEX == 1 & df[[cols[i]]] == 1)
      }),
      Female = sapply(seq_along(x_options), function(i) {
        sum(df$SEX == 2 & df[[cols[i]]] == 1)
      }),
      male_percentage = round(Male / count, 2),
      female_percentage = round(Female / count, 2),
      age_65_74 = sapply(seq_along(x_options), function(i) {
        sum(df$AGEGR10 == 6 & df[[cols[i]]] == 1)
      }),
      age_75 = sapply(seq_along(x_options), function(i) {
        sum(df$AGEGR10 == 7 & df[[cols[i]]] == 1)
      }),
      age_65_74_percentage = round(age_65_74 / count, 2),
      age_75_percentage = round(age_75 / count, 2),
      alzheimers = sapply(seq_along(x_options), function(i) {
        sum(df$PRP10GR == 8 & df[[cols[i]]] == 1)
      }),
      non_alzheimers = sapply(seq_along(x_options), function(i) {
        sum(df$PRP10GR != 8 & df[[cols[i]]] == 1)
      }),
      alzheimers_percentage = round(alzheimers / count, 2),
      non_alzheimers_percentage =
        round(non_alzheimers / count, 2),
    )
}

# --------- GENERAL TABLE MAKER - GENERAL CHARTS ---------------
# input : The main data we are working on
# frequency : vector caluclated in var_y
#---------------------------------------------------------------
tab_general <- function(input, frequency){
  count <- frequency
  df <- df_general(input, frequency) |>
    mutate(percentage = count/sum(count))
  
  return (df)
}

# --------- GENERAL TABLE MAKER - SINGLE VAR ------------------
# df : data-frame
# input : vector in var_x (NOTE: This gets renamed to x-options)
# code : column id in the dataset
# -------------------------------------------------------------
tab_maker <- function(df, input, code){
  count <- y_variable(df, input, code)
  df_output <- tab_helper(df, count, input, code)
  return (df_output)
}

# --------- GENERAL TABLE MAKER - MULTI VAR -------------------
# df : data-frame
# input : vector in var_x (NOTE: This gets renamed to x-options)
# code : column id in the dataset
# -------------------------------------------------------------
tab_multi_var_maker <- function(df, input, codes, y_function){
  count <- y_function(df)
  df_output <- tab_helper_multi_var(df, count, input, codes)
  return (df_output)
}

# --------- TABLE MAKER CHOOSER -----------------
# df : data-frame
# input : vector in var_x
# code : column id in the dataset
# y : function used from var_y
# -----------------------------------------------
tab_chooser <- function(df, input, code, y){
  if(is.null(y)){
    df <- tab_maker(df, input, code)
  } else{
    df <- tab_multi_var_maker(df, input, code, y)
  }
  return (df)
}


# TODO: Special table function, try to fix this later if possible.
tab_financial_hardship <- function(df) {
  count <- y_financial_hardship(df)

  total_male <- sum(df$SEX == 1)
  total_female <- sum(df$SEX == 2)
  x_options <- financial_hardship
  cols <- financial_hardship_codes
  cols2 <- "CRRCPAGR"

  df_output <- tibble(x_options, count) %>%
    mutate(
      percentage = count / sum(count),
      Male = sapply(seq_along(x_options), function(i) {
        sum(df$SEX == 1 & df[[cols[i]]] == 1 & (df[[cols2]] >= 14 & df[[cols2]] <= 20))
      }),
      Female = sapply(seq_along(x_options), function(i) {
        sum(df$SEX == 2 & df[[cols[i]]] == 1 & (df[[cols2]] >= 14 & df[[cols2]] <= 20))
      }),
      male_percentage = round(Male / total_male, 2),
      female_percentage = round(Female / total_female, 2),
    ) %>%
    rename(financial_hardship = x_options)

  return(df_output)
}
