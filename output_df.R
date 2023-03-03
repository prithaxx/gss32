source("var_x.R")
source("var_y.R")

df_pops <- tibble(pop_name, pop_freq) 
  # rename("Respondent Group" = pop_name, "Count" = pop_freq)

df_primary_sex <- tibble(
  sex = c(primary_sex, primary_sex),
  freq = c(primary_giver_sex_freq, primary_receiver_sex_freq),
  type = c(rep("Sex of primary caregiver of respondents who are older adult care receivers", 2), rep("Sex of primary care receiver of respondents who provided care to older adults", 2))
)

