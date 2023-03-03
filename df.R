library(tidyverse)
library(haven)

GSS_PUMF_2018 <- read_sas("c32pumfm.sas7bdat")

# Care Receiving ---

# Care receivers 65+ and have a primary caregiver
df_receiver <- GSS_PUMF_2018 %>%
  filter((CAR_110 == 1 | CAR_115 == 1) & (
    ARE_10 == 1 |
      ARE_20 == 1 |
      ARE_30 == 1 |
      ARE_40 == 1 |
      ARE_50 == 1 |
      ARE_60 == 1 |
      ARE_70 == 1 |
      ARE_80 == 1 |
      PAA_05 == 1
  )) %>%
  filter(AGEGR10 == 6 | AGEGR10 == 7)

df_receiver_65_74 <- df_receiver %>%
  filter(AGEGR10 == 6)

df_receiver_75 <- df_receiver %>%
  filter(AGEGR10 == 7)

# Care giving ---

# Care givers with primary care receiver 65+
df_giver <- GSS_PUMF_2018 %>%
  filter((ICG_110 == 1 | ICG_115 == 1) & (
    APR_10 == 1 |
      APR_20 == 1 |
      APR_30 == 1 |
      APR_40 == 1 |
      APR_50 == 1 |
      APR_60 == 1 |
      APR_70 == 1 |
      APR_80 == 1
  )) %>%
  filter(CRRCPAGR >= 14 & CRRCPAGR <= 20)

# Care giver and care receiver
df_giver_receiver <- intersect(df_receiver, df_giver)

# Need help (did not receive)
df_need_help <- GSS_PUMF_2018 %>%
  filter(NFA_10 == 1 & (AGEGR10 == 6 | AGEGR10 == 7))