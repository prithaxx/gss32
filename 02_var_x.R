pop_name <- c(
  "Giver",
  "Receiver",
  "Receiver - 65 to 74 years",
  "Receiver - 75 years and over",
  "Receiver and Giver",
  "Unmet Needs"
)

primary_sex <- c("male", "female")

###


health_conditions <- c(
  "Arthritis",
  "Cardiovascular disease",
  "Back problems",
  "Cancer",
  "Mental illness",
  "Alzheimer's disease or dementia",
  "Injury resulting from an accident",
  "Aging or frailty",
  "Other"
)

help_activities <- c(
  "transportation",
  "household chores",
  "house maintenance",
  "personal care",
  "medical treatment",
  "scheduling",
  "banking",
  "other"
)

giver_age_group <- c(
  "<20",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70+"
)

primary_receiver_age_group <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-80", "80-84", 
  "85-89", "90-94", "95+"
)

help_hours <- c("0", "1-9", "10-19", "20-29", "30-39", "40+")

# primary_giver_distance <- c(
#   "same household",
#   "same building",
#   "<10 min",
#   "10 to <30 min",
#   "30 min to <1 hour",
#   "1 to <3 hours",
#   ">3 hours"
# )

dwelling_distances <- c(
  "same household",
  "same building",
  "<10 min",
  "10 to <30 min",
  "30 min to <1 hour",
  "1 to <3 hours",
  ">3 hours"
)

primary_help_banking_freq <- c(
  "daily",
  "at least once a week",
  "at least once a month",
  "less than once a month"
)

primary_help_banking_hours <- c("<1", "1 to <3", "3 to <5", "5+")

out_of_pocket_expenses <- c(
  "home modifications", 
  "professional service", 
  "hiring people to help", 
  "transportation, accommodation", 
  "specialized aids/devices", 
  "prescription/non-pres. drugs", 
  "other"
)

financial_hardship <- c(
  "borrowed money from family or friends", 
  "loans from a bank or financial institution", 
  "use or defer savings", 
  "modify spending", 
  "sell off assets", 
  "file for bankruptcy", 
  "other"
)

# Filter lists ####

no_filter = -1

filter_sex <- list(
  "Both sexes" = no_filter,
  "Male" = 1,
  "Female" = 2
)

filter_age_group <- list(
  "65 years and over" = no_filter,
  "65 to 74 years" = 6,
  "75 years and over" = 7
)

filter_pop_centre <- list(
  "All" = no_filter,
  "Larger ubran population centres" = 1,
  "Rural areas and small population" = 2,
  "Prince Edward Island" = 3
)

filter_partner_in_household <- list(
  "All" = no_filter,
  "Yes" = 1,
  "No" = 2
)

filter_living_arrangement_senior_household <- list(
  "All" = no_filter,
  "Living alone" = 1,
  "living with spouse or partner only" = 2,
  "Living with spouse or partner and child(ren)" = 3,
  "Living with child(ren) only" = 4,
  "Living with other(s)" = 5
)

filter_indigenous_status <- list(
  "All" = no_filter,
  "Indigenous" = 2,
  "Non-indigenous" = 1
)

filter_visible_minority_status <- list(
  "All" = no_filter,
  "Visible minority" = 1,
  "Not a visible minority" = 2
)

filter_group_religious_participation <- list(
  "All" = no_filter,
  "At least once a week" = 1,
  "At least once a month" = 2,
  "At least 3 times a year" = 3,
  "Once or twice a year" = 4,
  "Not at all" = 5
)

# filter_province <- list(
#   "All Provinces",
#   "Newfoundland and Labrador",
#   "Prince Edward Island",
#   "Nova Scotia",
#   "New Brunswick",
#   "Quebec",
#   "Ontario",
#   "Manitoba",
#   "Saskatchewan",
#   "Alberta",
#   "British Columbia"
#
# )
