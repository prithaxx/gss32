df_pops <- tibble(pop_name, pop_freq) 

df_primary_sex <- tibble(
  sex = c(primary_sex, primary_sex),
  freq = c(primary_giver_sex_freq, primary_receiver_sex_freq),
  type = c(rep("Sex of Caregiver, reported by Care Receiver Respondents", 2), rep("Sex of Carees, reported by Caregiver Respondents", 2))
)

