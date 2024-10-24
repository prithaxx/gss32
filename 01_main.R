GSS_PUMF_2018 <- read_sas("data/c32pumfm.sas7bdat")

# Care Receiving ---
# Care receivers 65+ and have a primary caregiver
df_receiver <- GSS_PUMF_2018 %>%
  filter(CAR_110 == 1 | CAR_115 == 1)%>%
    filter(AGEGR10 == 6 | AGEGR10 == 7)

# TODO: unused
df_receiver_65_74 <- df_receiver %>%
  filter(AGEGR10 == 6)

# TODO: unused
df_receiver_75 <- df_receiver %>%
  filter(AGEGR10 == 7)


# Care giving ---
# Care givers with primary care receiver 65+
df_giver <- GSS_PUMF_2018 %>%
  filter(ICG_110 == 1 | ICG_115 == 1) %>%
    filter(CRRCPAGR >= 14 & CRRCPAGR <= 20)

# People who are both Care giver and care receiver
df_giver_receiver <- intersect(df_receiver, df_giver)

# People who are either Care receiver (65+) or care giver of someone (65+)
df_union <- union(df_receiver, df_giver)

# TODO: Need help (did not receive) -- [Asked for help?]
df_need_help <- GSS_PUMF_2018 %>%
  filter(NFA_10 == 1 & (AGEGR10 == 6 | AGEGR10 == 7))

# Alzheimer's
df_giver_alzheimers <- df_giver %>% filter(PRP10GR == 8)
df_giver_no_alzheimers <- df_giver %>% filter(PRP10GR != 8)

# Giver-Receiver Relationship
df_caree_relations <- df_receiver |>
  filter(PGG10GR >= 1 & PGG10GR <= 6)


# The four main populations in the dataset ------
# Care-Receiver charts
# Male care-receivers who have male carers
df_male_caree_male_carer <- df_receiver |>
  filter(SEX == 1 & PGN_25 == 1)
# Male care-receivers who have female carers
df_male_caree_female_carer <- df_receiver |>
  filter(SEX == 1 & PGN_25 == 2)
# Female care-receivers who have male carers
df_female_caree_male_carer <- df_receiver |>
  filter(SEX == 2 & PGN_25 == 1)
# Female care-receivers who have female carers
df_female_caree_female_carer <- df_receiver |>
  filter(SEX == 2 & PGN_25 == 2)

# Caregiver charts
# Male caregivers who care for older males
df_male_carer_male_caree <- df_giver |>
  filter(SEX == 1 & PRN_25 == 1)
# Male caregivers who care for older females
df_male_carer_female_caree <- df_giver |>
  filter(SEX == 1 & PRN_25 == 2)
# Female caregivers who care for older males
df_female_carer_male_caree <- df_giver |>
  filter(SEX == 2 & PRN_25 == 1)
# Female caregivers who care for older females
df_female_carer_female_caree <- df_giver |>
  filter(SEX == 2 & PRN_25 == 2)