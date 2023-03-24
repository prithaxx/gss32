# count_map(): takes a frame and returns the count for a categorical vector based on a chosen column
# df (tibble): data frame to be transformed
# x_options (vector): vector of variables to be counted
# col_name (String): variable to filter by
count_map <- function(df_input, x_options, col_name) {
  counts <- unlist(map(x_options, function(f) {
    nrow(filter(df_input, !!as.symbol(col_name) == f))
  }
  ))
}

###

pop_freq <- c(
  giver_pop <- nrow(df_giver),
  receiver_pop <- nrow(df_receiver),
  receiver_65_74_pop <- nrow(df_receiver_65_74),
  receiver_75_pop <- nrow(df_receiver_75),
  receiver_giver_pop <- nrow(df_giver_receiver),
  unmet_pop <- nrow(df_need_help)
)

y_pop_freq <- function(df_giver, df_receiver, df_receiver_65_74, df_receiver_75, df_need_help) {
  pop_freq <- c(
    giver_pop <- nrow(df_giver),
    receiver_pop <- nrow(df_receiver),
    receiver_65_74_pop <- nrow(df_receiver_65_74),
    receiver_75_pop <- nrow(df_receiver_75),
    receiver_giver_pop <- nrow(df_giver_receiver),
    unmet_pop <- nrow(df_need_help)
  )
}

primary_giver_sex_freq <- c(
  primary_giver_male <- nrow(filter(df_receiver, PGN_25 == 1)),
  primary_giver_female <- nrow(filter(df_receiver, PGN_25 == 2))
)

primary_receiver_sex_freq <- c(
  primary_receiver_male <- nrow(filter(df_giver, PRN_25 == 1)),
  primary_receiver_female <- nrow(filter(df_giver, PRN_25 == 2))
)

# Care receiver response y variables

y_health_condition <- function(df_receiver) {
  health_conditions_freq <- count_map(df_receiver, health_conditions, "PRA_10GR")
}


# y_health_condition <- function(df_receiver) {
#   health_conditions_freq <- c(
#     arthritis <- nrow(filter(df_receiver, PRA_10GR == 1)),
#     cardiovascular_disease <- nrow(filter(df_receiver, PRA_10GR == 2)),
#     back_problems <- nrow(filter(df_receiver, PRA_10GR == 3)),
#     cancer <- nrow(filter(df_receiver, PRA_10GR == 4)),
#     mental_illness <- nrow(filter(df_receiver, PRA_10GR == 5)),
#     alzheimer_dementia <- nrow(filter(df_receiver, PRA_10GR == 6)),
#     injury_from_accident <- nrow(filter(df_receiver, PRA_10GR == 7)),
#     aging_frailty <- nrow(filter(df_receiver, PRA_10GR == 8)),
#     other <- nrow(filter(df_receiver, PRA_10GR == 9))
#   )
# }

y_activity_receive_help <- function(df_receiver) {
  activity_receive_help_freq <- c(
    transportation <- nrow(filter(df_receiver, ARE_10 == 1)),
    household_chores <- nrow(filter(df_receiver, ARE_20 == 1)),
    house_maintenance <- nrow(filter(df_receiver, ARE_30 == 1)),
    personal_care <- nrow(filter(df_receiver, ARE_40 == 1)),
    medical_treatment <- nrow(filter(df_receiver, ARE_50 == 1)),
    scheduling <- nrow(filter(df_receiver, ARE_60 == 1)),
    banking <- nrow(filter(df_receiver, ARE_70 == 1)),
    help_activity_other <- nrow(filter(df_receiver, ARE_80 == 1))
  )
}


y_age_primary_giver <- function(df_receiver) {
  # age_primary_giver <- c(
  #   age_giver_1 <- nrow(filter(df_receiver, CRGVAGGR == 1)),
  #   age_giver_2 <- nrow(filter(df_receiver, CRGVAGGR == 2)),
  #   age_giver_3 <- nrow(filter(df_receiver, CRGVAGGR == 3)),
  #   age_giver_4 <- nrow(filter(df_receiver, CRGVAGGR == 4)),
  #   age_giver_5 <- nrow(filter(df_receiver, CRGVAGGR == 5)),
  #   age_giver_6 <- nrow(filter(df_receiver, CRGVAGGR == 6)),
  #   age_giver_7 <- nrow(filter(df_receiver, CRGVAGGR == 7)),
  #   age_giver_8 <- nrow(filter(df_receiver, CRGVAGGR == 8)),
  #   age_giver_9 <- nrow(filter(df_receiver, CRGVAGGR == 9)),
  #   age_giver_10 <- nrow(filter(df_receiver, CRGVAGGR == 10)),
  #   age_giver_11 <- nrow(filter(df_receiver, CRGVAGGR == 11)),
  #   age_giver_12 <- nrow(filter(df_receiver, CRGVAGGR == 12))
  # )
  age_primary_giver <- count_map(df_receiver, giver_age_group, "CRGVAGGR")
}

y_activity_receive_help_pro <- function(df_receiver) {
  activity_receive_help_pro <- c(
    transportation <- nrow(filter(df_receiver, PAA_10 == 1)),
    household_chores <- nrow(filter(df_receiver, PAA_20 == 1)),
    house_maintenance <- nrow(filter(df_receiver, PAA_30 == 1)),
    personal_care <- nrow(filter(df_receiver, PAA_40 == 1)),
    medical_treatment <- nrow(filter(df_receiver, PAA_50 == 1)),
    scheduling <- nrow(filter(df_receiver, PAA_60 == 1)),
    banking <- nrow(filter(df_receiver, PAA_70 == 1)),
    help_activity_other <- nrow(filter(df_receiver, PAA_80 == 1))
  )
}

y_hours_help_received <- function(df_receiver) {
  # hours_help_received <- c(
  #   hours_0 <- nrow(filter(df_receiver, HAR_10C == 0)),
  #   hours_1 <- nrow(filter(df_receiver, HAR_10C == 1)),
  #   hours_2 <- nrow(filter(df_receiver, HAR_10C == 2)),
  #   hours_3 <- nrow(filter(df_receiver, HAR_10C == 3)),
  #   hours_4 <- nrow(filter(df_receiver, HAR_10C == 4)),
  #   hours_5 <- nrow(filter(df_receiver, HAR_10C == 5))
  # )
  hours_help_received <- count_map(df_receiver, help_hours, "HAR_10C")
}

y_primary_giver_distance <- function(df) {
  # primary_giver_distance <- c(
  #   primary_giver_distance_1 <- nrow(filter(df, PGD_10 == 1)),
  #   primary_giver_distance_2 <- nrow(filter(df, PGD_10 == 2)),
  #   primary_giver_distance_3 <- nrow(filter(df, PGD_10 == 3)),
  #   primary_giver_distance_4 <- nrow(filter(df, PGD_10 == 4)),
  #   primary_giver_distance_5 <- nrow(filter(df, PGD_10 == 5)),
  #   primary_giver_distance_6 <- nrow(filter(df, PGD_10 == 6)),
  #   primary_giver_distance_7 <- nrow(filter(df, PGD_10 == 7))
  # )
  hours_help_received <- count_map(df, dwelling_distances, "PGD_10")
  
}

y_receive_help_banking_freq <- function(df) {
  receive_help_banking_freq <- c(
    receive_help_banking_1 <- nrow(filter(df, AGB_20 == 1)),
    receive_help_banking_2 <- nrow(filter(df, AGB_20 == 2)),
    receive_help_banking_3 <- nrow(filter(df, AGB_20 == 3)),
    receive_help_banking_4 <- nrow(filter(df, AGB_20 == 4))
  )
}

y_receive_help_banking_hours <- function(df) {
  receive_help_banking_hours <- c(
    help_banking_hours_1 <- nrow(filter(df, AGB_30C == 1)),
    help_banking_hours_2 <- nrow(filter(df, AGB_30C == 2)),
    help_banking_hours_3 <- nrow(filter(df, AGB_30C == 3)),
    help_banking_hours_4 <- nrow(filter(df, AGB_30C == 4))
  )
}

y_receive_help_banking_hours_freq <- function(df, response_code) {
  receive_help_banking_hours_freq <- c(
    help_banking_1 <- nrow(filter(df, AGB_30C == 1 & AGB_20 == response_code)),
    help_banking_2 <- nrow(filter(df, AGB_30C == 2 & AGB_20 == response_code)),
    help_banking_3 <- nrow(filter(df, AGB_30C == 3 & AGB_20 == response_code)),
    help_banking_4 <- nrow(filter(df, AGB_30C == 4 & AGB_20 == response_code))
  )
}



# Care giver response y variables
y_activity_give_help <- function(df) {
  activity_give_help_freq <- c(
    transportation <- nrow(filter(df, APR_10 == 1)),
    household_chores <- nrow(filter(df, APR_20 == 1)),
    house_maintenance <- nrow(filter(df, APR_30 == 1)),
    personal_care <- nrow(filter(df, APR_40 == 1)),
    medical_treatment <- nrow(filter(df, APR_50 == 1)),
    scheduling <- nrow(filter(df, APR_60 == 1)),
    banking <- nrow(filter(df, APR_70 == 1)),
    help_activity_other <- nrow(filter(df, APR_80 == 1))
  )
}

y_age_primary_receiver <- function(df) {
  age_receiever_freq <- c(
    age_receiver_1 <- nrow(filter(df, CRRCPAGR == 1)),
    age_receiver_2 <- nrow(filter(df, CRRCPAGR == 2)),
    age_receiver_3 <- nrow(filter(df, CRRCPAGR == 3)),
    age_receiver_4 <- nrow(filter(df, CRRCPAGR == 4)),
    age_receiver_5 <- nrow(filter(df, CRRCPAGR == 5)),
    age_receiver_6 <- nrow(filter(df, CRRCPAGR == 6)),
    age_receiver_7 <- nrow(filter(df, CRRCPAGR == 7)),
    age_receiver_8 <- nrow(filter(df, CRRCPAGR == 8)),
    age_receiver_9 <- nrow(filter(df, CRRCPAGR == 9)),
    age_receiver_10 <- nrow(filter(df, CRRCPAGR == 10)),
    age_receiver_11 <- nrow(filter(df, CRRCPAGR == 11)),
    age_receiver_12 <- nrow(filter(df, CRRCPAGR == 12)),
    age_receiver_13 <- nrow(filter(df, CRRCPAGR == 13)),
    age_receiver_14 <- nrow(filter(df, CRRCPAGR == 14)),
    age_receiver_15 <- nrow(filter(df, CRRCPAGR == 15)),
    age_receiver_16 <- nrow(filter(df, CRRCPAGR == 16)),
    age_receiver_17 <- nrow(filter(df, CRRCPAGR == 17)),
    age_receiver_18 <- nrow(filter(df, CRRCPAGR == 18)),
    age_receiver_19 <- nrow(filter(df, CRRCPAGR == 19)),
    age_receiver_20 <- nrow(filter(df, CRRCPAGR == 20))
  )
}

y_hours_help_provided <- function(df) {
  hours_help_provided <- c(
    hours_0 <- nrow(filter(df, HAR_10C == 0)),
    hours_1 <- nrow(filter(df, HAR_10C == 1)),
    hours_2 <- nrow(filter(df, HAR_10C == 2)),
    hours_3 <- nrow(filter(df, HAR_10C == 3)),
    hours_4 <- nrow(filter(df, HAR_10C == 4)),
    hours_5 <- nrow(filter(df, HAR_10C == 5))
  )
}

y_primary_receiver_distance <- function(df) {
  primary_receiver_distance <- c(
    primary_receiver_distance_1 <- nrow(filter(df, PRD_10 == 1)),
    primary_receiver_distance_2 <- nrow(filter(df, PRD_10 == 2)),
    primary_receiver_distance_3 <- nrow(filter(df, PRD_10 == 3)),
    primary_receiver_distance_4 <- nrow(filter(df, PRD_10 == 4)),
    primary_receiver_distance_5 <- nrow(filter(df, PRD_10 == 5)),
    primary_receiver_distance_6 <- nrow(filter(df, PRD_10 == 6)),
    primary_receiver_distance_7 <- nrow(filter(df, PRD_10 == 7))
  )
}

y_give_help_banking_freq <- function(df) {
  give_help_banking_freq <- c(
    give_help_banking_1 <- nrow(filter(df, AGB_20 == 1)),
    give_help_banking_2 <- nrow(filter(df, AGB_20 == 2)),
    give_help_banking_3 <- nrow(filter(df, AGB_20 == 3)),
    give_help_banking_4 <- nrow(filter(df, AGB_20 == 4))
  )
}

y_give_help_banking_hours <- function(df) {
  give_help_banking_hours_freq <- c(
    help_banking_hours_1 <- nrow(filter(df, ARB_30C == 1)),
    help_banking_hours_2 <- nrow(filter(df, ARB_30C == 2)),
    help_banking_hours_3 <- nrow(filter(df, ARB_30C == 3)),
    help_banking_hours_4 <- nrow(filter(df, ARB_30C == 4))
  )
}

y_give_help_banking_hours_freq <- function(df, response_code) {
  give_help_banking_hours_freq <- c(
    help_banking_1 <- nrow(filter(df, ARB_30C == 1 & ARB_20 == response_code)),
    help_banking_2 <- nrow(filter(df, ARB_30C == 2 & ARB_20 == response_code)),
    help_banking_3 <- nrow(filter(df, ARB_30C == 3 & ARB_20 == response_code)),
    help_banking_4 <- nrow(filter(df, ARB_30C == 4 & ARB_20 == response_code))
  )
}

y_out_of_pocket <- function(df) {
  out_of_pocket_freq <- c(
    out_of_pocket_1 <- nrow(filter(df, ICF_210 == 1)),
    out_of_pocket_2 <- nrow(filter(df, ICF_220 == 1)),
    out_of_pocket_3 <- nrow(filter(df, ICF_230 == 1)),
    out_of_pocket_4 <- nrow(filter(df, ICF_240 == 1)),
    out_of_pocket_5 <- nrow(filter(df, ICF_250 == 1)),
    out_of_pocket_6 <- nrow(filter(df, ICF_260 == 1)),
    out_of_pocket_7 <- nrow(filter(df, ICF2_270 == 1))
  )
}
y_financial_hardship <- function(df) {
  financial_hardship_freq <- c(
    financial_hardship_1 <- nrow(filter(df, ICF2_290 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20))),
    financial_hardship_2 <- nrow(filter(df, ICF2_300 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20))),
    financial_hardship_3 <- nrow(filter(df, ICF2_310 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20))),
    financial_hardship_4 <- nrow(filter(df, ICF2_320 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20))),
    financial_hardship_5 <- nrow(filter(df, ICF2_330 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20))),
    financial_hardship_6 <- nrow(filter(df, ICF2_340 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20))),
    financial_hardship_7 <- nrow(filter(df, ICF2_350 == 1 & (CRRCPAGR >= 14 & CRRCPAGR <= 20)))
  )
}