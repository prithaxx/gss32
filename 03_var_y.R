pop_freq <- c(
  giver_pop <- nrow(df_giver),
  receiver_pop <- nrow(df_receiver),
  unmet_pop <- nrow(df_need_help)
)

pop_freq <- c(
  giver_pop <- nrow(df_giver),
  receiver_pop <- nrow(df_receiver),
  unmet_pop <- nrow(df_need_help)
)

caree_freq <- c(
  spouse_pop <- nrow(filter(df_caree_relations, PGG10GR == 1)),
  son_pop <- nrow(filter(df_caree_relations, PGG10GR == 2)),
  daughter_pop <- nrow(filter(df_caree_relations, PGG10GR == 3)),
  parent_pop <- nrow(filter(df_caree_relations, PGG10GR == 4)),
  family_pop <- nrow(filter(df_caree_relations, PGG10GR == 5)),
  other_pop <- nrow(filter(df_caree_relations, PGG10GR == 6))
)

disability_freq <- c(
  none_pop <- nrow(filter(df_union, DTYPER == 0)),
  one_pop <- nrow(filter(df_union, DTYPER == 1)),
  two_three_pop <- nrow(filter(df_union, DTYPER == 2)),
  more_three_pop <- nrow(filter(df_union, DTYPER == 3))
)

# Sex of Care Receiver and Giver Respondents 
receiver_sex_freq <- c(
  receiver_male <- nrow(filter(df_receiver, SEX == 1)),
  receiver_female <- nrow(filter(df_receiver, SEX == 2))
)

giver_sex_freq <- c(
  giver_male <- nrow(filter(df_giver, SEX == 1)),
  giver_female <- nrow(filter(df_giver, SEX == 2))
)

primary_giver_sex_freq <- c(
  nrow(df_male_carer_male_caree),
  nrow(df_male_carer_female_caree),
  nrow(df_female_carer_male_caree),
  nrow(df_female_carer_female_caree)
)

primary_receiver_sex_freq <- c(
  nrow(df_male_caree_male_carer),
  nrow(df_male_caree_female_carer),
  nrow(df_female_caree_male_carer),
  nrow(df_female_caree_female_carer)
)

# Care receiver response y variables

y_health_condition <- function(df_receiver) {
  health_conditions_freq <- count_map(df_receiver, health_conditions, "PRA_10GR")
}

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
help_activity_codes <- c(
  "ARE_10",
  "ARE_20",
  "ARE_30",
  "ARE_40",
  "ARE_50",
  "ARE_60",
  "ARE_70",
  "ARE_80"
)

y_disability_indicator <- function(df_receiver){
  disability <- c(
    seeing <- nrow(filter(df_receiver, DVIS_FL == 1)),
    hearing <- nrow(filter(df_receiver, DHEAR_FL == 1)),
    mobility <- nrow(filter(df_receiver, DMOB_FL == 1)),
    flexibility <- nrow(filter(df_receiver, DFLEX_FL == 1)),
    dexterity <- nrow(filter(df_receiver, DDEX_FL == 1)),
    pain <- nrow(filter(df_receiver, DPAIN_FL == 1)),
    learning <- nrow(filter(df_receiver, DLRN_FL == 1)),
    developmental <- nrow(filter(df_receiver, DDEV_FL == 1)),
    memory <- nrow(filter(df_receiver, DMEM_FL == 1)),
    mental_health <- nrow(filter(df_receiver, DMENT_FL == 1)),
    unknown <- nrow(filter(df_receiver, DUNK_FL == 1))
  )
}

disability_codes <- c(
  "DVIS_FL",
  "DHEAR_FL",
  "DMOB_FL",
  "DFLEX_FL",
  "DDEX_FL",
  "DPAIN_FL",
  "DLRN_FL",
  "DDEV_FL",
  "DMEM_FL",
  "DMENT_FL",
  "DUNL_FL"
)

y_caree_type <- function(df_receiver){
  caree <- c(
    friends_family <- nrow(filter(df_receiver, CARUNPAI == 1)),
    professional <- nrow(filter(df_receiver, CARPAID == 1)),
    both <- nrow(filter(df_receiver, CARBOTH == 1))
  )
}

caree_codes <- c(
  "CARUNPAI",
  "CARPAID",
  "CARBOTH"
)

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
help_activity_pro_codes <- c(
  "PAA_10",
  "PAA_20",
  "PAA_30",
  "PAA_40",
  "PAA_50",
  "PAA_60",
  "PAA_70",
  "PAA_80"
)


# Care giver response y variables ####
activity_give_help_codes <- c(
  "APR_10", # transportation
  "APR_20", # household chores
  "APR_30", # house maintenance
  "APR_40", # personal care
  "APR_50", # medical treatment
  "APR_60", # scheduling
  "APR_70", # banking
  "APR_80"  # other
)

y_activity_give_help <- function(df_giver) {
  activity_give_help_freq <- c(
    transportation <- nrow(filter(df_giver, APR_10 == 1)),
    household_chores <- nrow(filter(df_giver, APR_20 == 1)),
    house_maintenance <- nrow(filter(df_giver, APR_30 == 1)),
    personal_care <- nrow(filter(df_giver, APR_40 == 1)),
    medical_treatment <- nrow(filter(df_giver, APR_50 == 1)),
    scheduling <- nrow(filter(df_giver, APR_60 == 1)),
    banking <- nrow(filter(df_giver, APR_70 == 1)),
    help_activity_other <- nrow(filter(df_giver, APR_80 == 1))
  )
}

out_of_pocket_codes <- c(
  "ICF_210",
  "ICF_220",
  "ICF_230",
  "ICF_240",
  "ICF_250",
  "ICF_260",
  "ICF2_270"
)
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

financial_hardship_codes <- c(
  "ICF2_290",
  "ICF2_300",
  "ICF2_310",
  "ICF2_320",
  "ICF2_330",
  "ICF2_340",
  "ICF2_350"
)
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

y_end_of_life_care <- function(df){
  end_of_life_care_freq <- c(
    home_modifications <- nrow(filter(df, CEH_10 == 1)),
    financial_cost <- nrow(filter(df, CEH_20 == 1)),
    time_off_work <- nrow(filter(df, CEH_30 == 1)),
    health_stamina <- nrow(filter(df, CEH_40 == 1)),
    health_training <- nrow(filter(df, CEH_50 == 1)),
    home_care_support <- nrow(filter(df, CEH_60 == 1)),
    other_condition <- nrow(filter(df, CEH_70 == 1))
  )
}

end_of_life_care_codes <- c(
  "CEH_10",
  "CEH_20",
  "CEH_30",
  "CEH_40",
  "CEH_50",
  "CEH_60",
  "CEH_70"
)

y_caregiving_social_consequences <- function(df){
  caregiving_social_consequences_freq <- c(
    spouse <- nrow(filter(df, ICL_110 == 1)),
    children <- nrow(filter(df, ICL_120 == 1)),
    family <- nrow(filter(df, ICL_130 == 1)),
    friends <- nrow(filter(df, ICL_135 == 1)),
    hobbies <- nrow(filter(df, ICL_140 == 1)),
    relax <- nrow(filter(df, ICL_150 == 1)),
    volunteering <- nrow(filter(df, ICL_152 == 1)),
    groups <- nrow(filter(df, ICL_154 == 1)),
    plans <- nrow(filter(df, ICL_160 == 1)),
    holiday <- nrow(filter(df, ICL_170 == 1)),
    residences <- nrow(filter(df, ICL_180 == 1))
  )
}

caregiving_social_consequences_codes <- c(
  "ICL_110",
  "ICL_120",
  "ICL_130",
  "ICL_135",
  "ICL_140",
  "ICL_150",
  "ICL_152",
  "ICL_154",
  "ICL_160",
  "ICL_170",
  "ICL_180"
)

### Generic function for calculating y-variables
# df <- dataframe
# x <- vector
# input <- string code 
y_variable <- function(df, x, y_input){
  result <- count_map(df, x, y_input)
}