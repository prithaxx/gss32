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
  "Arthritis" = 1,
  "Cardiovascular disease" = 2,
  "Back problems" = 3,
  "Cancer" = 4,
  "Mental illness" = 5,
  "Alzheimer's disease or dementia" = 6,
  "Injury resulting from an accident" = 7,
  "Aging or frailty" = 8,
  "Other" = 9 
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
  "<20" = 1,
  "20-24" = 2,
  "25-29" = 3,
  "30-34" = 4,
  "35-39" = 5,
  "40-44" = 6,
  "45-49" = 7,
  "50-54" = 8,
  "55-59" = 9,
  "60-64" = 10,
  "65-69" = 11,
  "70+" = 12
)

primary_receiver_age_group <- c(
  "0-4" = 1, 
  "5-9" = 2, 
  "10-14" = 3, 
  "15-19" = 4, 
  "20-24" = 5, 
  "25-29" = 6, 
  "30-34" = 7, 
  "35-39" = 8, 
  "40-44" = 9, 
  "45-49" = 10, 
  "50-54" = 11, 
  "55-59" = 12, 
  "60-64" = 13, 
  "65-69" = 14,
  "70-74" = 15, 
  "75-80" = 16, 
  "80-84" = 17, 
  "85-89" = 18, 
  "90-94" = 19, 
  "95+" = 20
)

help_hours <- c(
  "0" = 0,
  "1-9" = 1,
  "10-19" = 2,
  "20-29" = 3,
  "30-39" = 4,
  "40+" = 5
)

dwelling_distances <- c(
  "Same household" = 1,
  "Same building" = 2,
  "<10 min by car" = 3,
  "10 to <30 minutes by car" = 4,
  "30 min to <1 hour by car" = 5,
  "1 hour to <3 hours by car" = 6,
  "3+ hours by car" = 7
)

primary_help_banking_freq <- c(
  "daily" = 1,
  "at least once a week" = 2,
  "at least once a month" = 3,
  "less than once a month" = 4
)

primary_help_banking_hours <- c(
  "<1" = 1, 
  "1 to <3" = 2,
  "3 to <5" = 3, 
  "5+" = 4
)

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

no_filter <- -1

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
