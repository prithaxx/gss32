pop_name <-
  c(
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

activity_receive_help <- c(
  "transportation",
  "household chorse",
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

hours_help_received <- c("0", "1-9", "10-19", "20-29", "30-39", "40+")

primary_giver_distance <- c(
  "same household",
  "same building",
  "<10 min",
  "10 to <30 min",
  "30 min to <1 hour",
  "1 to <3 hours",
  ">3 hours"
)

primary_help_banking <- c("daily", "at least once a week", "at least once a month", "less than once a month")


# Filter lists ####

filter_sex <- list(
  "Both sexes",
  "Male",
  "Female"
)

filter_age_group <- list(
  "65 years and over",
  "65 to 74 years",
  "75 years and over"
)

filter_pop_centre <- list(
  "All",
  "Larger ubran population centres",
  "Rural areas and small population",
  "Prince Edward Island"
)

filter_partner_in_household <- list(
  "All",
  "Yes",
  "No"
)

filter_indigenous_status <- list(
  "All",
  "Indigenous",
  "Non-indigenous"
)

filter_living_arrangement_senior_household <- list(
  "All",
  "Living alone",
  "living with spouse or partner only",
  "Living with spouse or partner and child(ren)",
  "Living with child(ren) only",
  "Living with other(s)"
)

filter_visible_minority_status <- list(
  "All",
  "Visible minority",
  "Not a visible minority"
)

filter_group_religious_participation <- list(
  "All",
  "At least once a week",
  "At least once a month",
  "At least 3 times a year",
  "Once or twice a year",
  "Not at all"
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