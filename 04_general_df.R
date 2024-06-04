df_pops <- tibble(pop_name, pop_freq) 

df_primary_sex <- tibble(
  sex = c(primary_sex, primary_sex),
  freq = c(primary_giver_sex_freq, primary_receiver_sex_freq),
  type = c(rep("Sex of Caregiver, reported by Care Receiver Respondents", 2), rep("Sex of Carees, reported by Caregiver Respondents", 2))
)

df_caree_relationship_pops <- tibble(caree_relationship, caree_freq)
df_disability_counter <- tibble(disability_counter, disability_freq)

df_general <- function(input, frequency){
  df <- tibble(input, frequency)
  return (df)
}