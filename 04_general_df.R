#Sex of Care Receiver Respondents
df_receiver_sex <- tibble(
  sex = c(primary_sex, primary_receiver_sex),
  freq = c(receiver_sex_freq, primary_receiver_sex_freq),
  type = c(rep("Sex of Care Receiver Respondent", 2), rep("Sex of primary Caregiver, reported by Care Receiver Respondents", 4))
)
# 
# df_giver_sex <- tibble(
#   sex = c(primary_sex, primary_sex),
#   freq = c(giver_sex_freq, primary_receiver_sex_freq),
#   type = c(rep("Sex of Care Giver Respondent", 2), rep("Sex of primary Carees, reported by Caregiver Respondents", 2))
# )

df_general <- function(input, frequency){
  df <- tibble(input, frequency)
  return (df)
}