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