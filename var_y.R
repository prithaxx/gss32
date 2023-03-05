source("df.R")

pop_freq <-
  c(
    giver_pop <- nrow(df_giver),
    receiver_pop <- nrow(df_receiver),
    receiver_65_74_pop <- nrow(df_receiver_65_74),
    receiver_75_pop <- nrow(df_receiver_75),
    receiver_giver_pop <- nrow(df_giver_receiver),
    unmet_pop <- nrow(df_need_help)
  )

primary_giver_sex_freq <- c(
  primary_giver_male <- nrow(filter(df_receiver, PGN_25 == 1)),
  primary_giver_female <- nrow(filter(df_receiver, PGN_25 == 2))
)

primary_receiver_sex_freq <- c(
  primary_receiver_male <- nrow(filter(df_giver, PRN_25 == 1)),
  primary_receiver_female <- nrow(filter(df_giver, PRN_25 == 2))
)






y_health_condition <- function(df_receiver) {
  health_conditions_freq <- c(
    arthritis <- nrow(filter(df_receiver, PRA_10GR == 1)),
    cardiovascular_disease <- nrow(filter(df_receiver, PRA_10GR == 2)),
    back_problems <- nrow(filter(df_receiver, PRA_10GR == 3)),
    cancer <- nrow(filter(df_receiver, PRA_10GR == 4)),
    mental_illness <- nrow(filter(df_receiver, PRA_10GR == 5)),
    alzheimer_dementia <- nrow(filter(df_receiver, PRA_10GR == 6)),
    injury_from_accident <- nrow(filter(df_receiver, PRA_10GR == 7)),
    aging_frailty <- nrow(filter(df_receiver, PRA_10GR == 8)),
    other <- nrow(filter(df_receiver, PRA_10GR == 9))
  )
}


y_activity_receive_help <- function(df_receiver, ARE_10, ARE_20, ARE_30, ARE_40, ARE_50, ARE_60, ARE_70, ARE_80) {
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
  age_primary_giver <- c(
    age_giver_1 <- nrow(filter(df_receiver, CRGVAGGR == 1)),
    age_giver_2 <- nrow(filter(df_receiver, CRGVAGGR == 2)),
    age_giver_3 <- nrow(filter(df_receiver, CRGVAGGR == 3)),
    age_giver_4 <- nrow(filter(df_receiver, CRGVAGGR == 4)),
    age_giver_5 <- nrow(filter(df_receiver, CRGVAGGR == 5)),
    age_giver_6 <- nrow(filter(df_receiver, CRGVAGGR == 6)),
    age_giver_7 <- nrow(filter(df_receiver, CRGVAGGR == 7)),
    age_giver_8 <- nrow(filter(df_receiver, CRGVAGGR == 8)),
    age_giver_9 <- nrow(filter(df_receiver, CRGVAGGR == 9)),
    age_giver_10 <- nrow(filter(df_receiver, CRGVAGGR == 10)),
    age_giver_11 <- nrow(filter(df_receiver, CRGVAGGR == 11)),
    age_giver_12 <- nrow(filter(df_receiver, CRGVAGGR == 12))
  )
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
  hours_help_received <- c(
    hours_0 <- nrow(filter(df_receiver, HAR_10C == 0)),
    hours_1 <- nrow(filter(df_receiver, HAR_10C == 1)),
    hours_2 <- nrow(filter(df_receiver, HAR_10C == 2)),
    hours_3 <- nrow(filter(df_receiver, HAR_10C == 3)),
    hours_4 <- nrow(filter(df_receiver, HAR_10C == 4)),
    hours_5 <- nrow(filter(df_receiver, HAR_10C == 5))
  )
}

y_primary_giver_distance <- function(df) {
  primary_giver_distance <- c(
    primary_giver_distance_1 <- nrow(filter(df, PGD_10 == 1)),
    primary_giver_distance_2 <- nrow(filter(df, PGD_10 == 2)),
    primary_giver_distance_3 <- nrow(filter(df, PGD_10 == 3)),
    primary_giver_distance_4 <- nrow(filter(df, PGD_10 == 4)),
    primary_giver_distance_5 <- nrow(filter(df, PGD_10 == 5)),
    primary_giver_distance_6 <- nrow(filter(df, PGD_10 == 6)),
    primary_giver_distance_7 <- nrow(filter(df, PGD_10 == 7))
  )
}



