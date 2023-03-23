library(tidyverse)

# create sample data
data <- tibble(
  category = rep(c("A", "B", "C"), each = 3), # x
  group = rep(c("X", "Y", "Z"), 3),
  value = c(10, 20, 30, 15, 25, 35, 12, 22, 32)
)

# create dodged bar plot
ggplot(data, aes(x = category, y = value, fill = group)) +
  geom_col(position = position_dodge()) +
  labs(x = "Category", y = "Value", fill = "Group")


# gb <- tab_health_conditions(df_receiver) %>%
#   mutate(
#     male = nrow(filter(SEX == 1))
#   )
# 
# gb