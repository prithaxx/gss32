

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
  hours_help_received <- count_map(df_receiver, help_hours, "HAR_10C")
}

y_primary_giver_distance <- function(df) {
  hours_help_received <- count_map(df, dwelling_distances, "PGD_10")

}

y_receive_help_banking_freq <- function(df) {
  receive_help_freq <- count_map(df, primary_help_banking_freq, "AGB_20")
}

y_receive_help_banking_hours <- function(df) {
  receive_help_banking_hours <- count_map(df, primary_help_banking_hours, "AGB_30C")
}

y_receive_help_banking_hours_freq <- function(df, response_code) {
  receive_help_banking_hours_freq <- count_map(df, primary_help_banking_hours, "AGB_30C", "AGB_20", response_code)
}



# Care giver response y variables ####


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
  age_receiever_freq <- count_map(df, primary_receiver_age_group, "CRRCPAGR")
}

y_hours_help_provided <- function(df) {

  hours_help_provided <- count_map(df, help_hours, "HAR_10C")
}

y_primary_receiver_distance <- function(df) {
  primary_receiver_distance <- count_map(df, dwelling_distances, "PRD_10")
}

y_give_help_banking_freq <- function(df) {
  give_help_banking_freq <- count_map(df, primary_help_banking_freq, "AGB_20")
}

y_give_help_banking_hours <- function(df) {
  give_help_banking_hours_freq <- count_map(df, primary_help_banking_hours, "ARB_30C")
}

y_give_help_banking_hours_freq <- function(df, response_code) {
  give_help_banking_hours_freq <- count_map(df, primary_help_banking_hours, "ARB_30C", "ARB_20", response_code)
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