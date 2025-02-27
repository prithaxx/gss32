#The main Shiny application.
# Defines the interface for the application, and ties the UI elements to the
# various charts that are defined across the other modules.

library(shiny)
library(shinyjs)
source("global.R")
source("01_main.R")
source("02_var_x.R")
source("03_var_y.R")

general_charts <- list(
  "Respondent Groups",
  "Sex of Care Receiving Respondents",
  "Sex of Care Giver Respondents",
  "Relationship between Caree and Primary Carer",
  "Number of Disability Types in all Respondents"
)

receiver_ui_config <- list(
  "Main Heath Condition of Respondent" = list(
    index = 1,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = health_conditions,
    y = NULL,
    code = "PRA_10GR",
    caption = "Count for main health conditions for which respondents considered to be a care receiver and 65 years of age or older received help.",
    title = "Main Health Condition due to which Respondent receives care",
    x_axis = "Health Condition",
    y_axis = "Count",
    caption_pct = "Proportion of care receiver respondents reporting item as their main health condition.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_maker(df_receiver, health_conditions, "PRA_10GR"),
    title_fragment = "of Care Receiving Respondents with Health Conditions"
  ),
  "Activities Respondent Gets Help With" = list(
    index = 2,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = help_activities,
    code = help_activity_codes,
    y = y_activity_receive_help,
    caption = "Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received with from family, friends or neighbours in the past 12 months.",
    title = "Types of Activities Respondent Gets Help With - Past 12 months",
    x_axis = "Activty",
    y_axis = "Count",
    caption_pct = "Proportion of the type of activities for which respondents considered to be a care receiver and 65 years o fage or older received with family, friends or neighbours in the past 12 months.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_multi_var_maker(df_receiver, help_activities, help_activity_codes, y_activity_receive_help),
    title_fragment = "of Care Receiving Respondents who received Help with an Activity"
  ),
  "Age of Respondent's Primary Caregiver" = list(
    index = 3,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = giver_age_group,
    code = "CRGVAGGR",
    y = NULL,
    caption = "Count of the age (groups of 5) of primary caregivers for respondents considered to be a care receiver and 65 years of age or older.",
    title = "Age of Respondent's Primary Caregiver",
    x_axis = "Age (years)",
    y_axis = "Count",
    caption_pct = "Proportion of care receiver respondents reporting their primary caregiver's age.",
    y_axis_pct = "Proportion",
    table = tab_maker(df_receiver, giver_age_group, "CRGVAGGR"),
    title_fragment = "of Care Receiving Respondents and the Age of their Primary Caregiver"
  ),
  "Activities Assisted by Professionals" = list(
    index = 4,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = help_activities,
    code = help_activity_pro_codes,
    y = y_activity_receive_help_pro,
    caption = "Count for the type of activities for which respondents considered to be a care receiver and 65 years of age or older received help from a professional in the past 12 months.",
    title = "Types of Activities that Respondent needs Professional Help with - Past 12 months",
    x_axis = "Activity",
    y_axis = "Count",
    caption_pct = "Proportion of care receiver respondents reporting they received help from a professional in the past 12 months with each activity",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_multi_var_maker(df_receiver, help_activities, help_activity_pro_codes, y_activity_receive_help_pro),
    title_fragment = "of Care Receiving Respondents who Received Professional Help with an Activity"
  ),
  "Hours of Help Received per Week" = list(
    index = 5,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = help_hours,
    code = "HAR_10C",
    y = NULL,
    caption = "Count for the number of hours of help received, average per week, from family, friends or neighbours in the past 12 months.",
    title = "Hours of Help Received per Week by the Respondent - Past 12 months",
    x_axis = "Time (hour)",
    y_axis = "Count",
    caption_pct = "Proportion of care receiever repsondents reporting avergae number of hours of help received, per week from family, friends or neighbours.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_maker(df_receiver, help_hours, "HAR_10C"),
    title_fragment = "of Care Receiving Respondents and the Number of Hours of Help Received - Per Average Week"
  ),
  "Primary Caregiver Distance Away" = list(
    index = 6,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = dwelling_distances,
    code = "PGD_10",
    y = NULL,
    caption = "Counts for the distance by car between respondents considered to be a care receiver and 65 years of age or older, amd their primary caregiver during the time they were receiving help in the past 12 months.",
    title = "Distance between the dwellings of Respondent and Primary Caregiver - Past 12 months",
    x_axis = "Distane (time)",
    y_axis = "Count",
    caption_pct = "Proportion of care receiver respondents reporting distance by care between themselves and their primary caregiver.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_maker(df_receiver, dwelling_distances, "PGD_10"),
    title_fragment ="of Care Receiving Respondents and the Distance Between them and their Caregiver's Dwellings"
  ),
  "Banking Help Received - frequency" = list(
    index = 7,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = primary_help_banking_freq,
    code = "ARB_20",
    y = NULL,
    caption = "Count for how often respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.",
    title = "How many times did the Respondent Receive help for Banking - Past 12 months",
    x_axis = "Help Frequency",
    y_axis = "Count",
    caption_pct = "Of care receiver respondents reporting that they did receive help with managing their finances in the past 12 months, proportion reporting each freqeuncy level of help.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_maker(df_receiver, primary_help_banking_freq, "ARB_20"),
    title_fragment = "of Care Receiving Respondents and the Frequency Their Primary Caregiver Helped with Banking"
  ),
  "Banking Help Received - hours" = list(
    index = 8,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = primary_help_banking_hours,
    code = "ARB_30C",
    y = NULL,
    caption = "Count for the number of hours respondents considered to be a care receiver and 65 years of age or older received help with managing their finances in the past 12 months.",
    title = "How many Hours did the Respondent Receive help for Banking - Past 12 months",
    x_axis = "Time (hours)",
    y_axis = "Count",
    caption_pct = "Of care receiver respondents reporting that they did receive help with managing their finances in the past 12 months, proportion reporting numbers of hours of banking assistance achieved.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_maker(df_receiver, primary_help_banking_hours, "ARB_30C"),
    title_fragment = "of Care Receiving Respondents and Number of Hours their Primary Caregiver Helped with Banking"
  ),
  "Respondent Didn't Receive Care" = list(
    index = 9,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = received_nohelp_reasons,
    code = "DVCNR20",
    y = NULL,
    caption = "Count for the main reasons why respondents considered to be a care receiver and 65 years of age or older did not receive the care they need.",
    title = "Reasons why Respondent did not Receive Care",
    x_axis = "Reasons",
    y_axis = "Count",
    caption_pct = "Proportion for the main reasons why respondents considered to be a carereciver and 65 years of age or older did not receive the care they need.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_maker(df_receiver, received_nohelp_reasons, "DVCNR20"),
    title_fragment = "of Care Receiving Respondents and the reasons why they did not receive help"
  ),
  "Respondent has a Disability Indicator" = list(
    index = 10,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = disability_indicators,
    code = disability_codes,
    y = y_disability_indicator,
    caption = "Frequency of the type of disability indicators within respondents considered to be a care receiver and 65 years of age or older.",
    title = "Classes of Disability Indicators Reported by Respondent",
    x_axis = "Types of Disability Indicators",
    y_axis = "Count",
    caption_pct = "Proportion of the type of disability indicators within respondents considered to be a care receiver and 65 years if age or older.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_multi_var_maker(df_receiver, disability_indicators, disability_codes, y_disability_indicator),
    title_fragment = "of Care Receiving Respondents who have a Disability indicator"
  ),
  "Services/ People who cared for Respondent" = list(
    index = 11,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = caree_type,
    code = caree_codes,
    y = y_caree_type,
    caption = "Frequency of the type of caree (Friends/ Family, Professional or Both) for the respondent considered to be a care receiver and 65 years of age or older.",
    title = "Services/ People who Cared for Respondent",
    x_axis = "Types of Caree",
    y_axis = "Count",
    caption_pct = "Proportion of the type of caree (Friends/Family, Professional or Both) for the respondent considered to be a care receiver and 65 years of age or older.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_multi_var_maker(df_receiver, caree_type, caree_codes, y_caree_type),
    title_fragment = "of Care Receiving Respondents who have a type of Caree"
  )
)

giver_ui_config <- list(
  "Activities Respondent Assists Caree With" = list(
    index = 1,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = help_activities,
    code = activity_give_help_codes,
    y = y_activity_give_help,
    caption = "Count for the type of activities for which respondents considered to be a care giver assisted with in the past 12 months.",
    title = "Types of activities respondents provided help with - Past 12 months",
    x_axis = "Activity",
    y_axis = "Count",
    caption_pct = "Proportion of caregiver respondents who report providing the help to caree in the past 12 months with each type of activity.",
    y_axis_pct = "Proportion",
    table = tab_multi_var_maker(df_giver, help_activities, activity_give_help_codes, y_activity_give_help),
    title_fragment = "of Caregiving Respondents who Provided Help with an Activity"
  ),
  "Age of Primary Caree" = list(
    index = 2,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = primary_receiver_age_group,
    code = "CRRCPAGR",
    y = NULL,
    caption = "Count of groups of ages of respondents considered to be a care giver",
    title = "Age of primary care receiver",
    x_axis = "Age Group (years)",
    y_axis = "Count",
    caption_pct = "Proportion of groups of ages of respondents considered to be a care receiver.",
    y_axis_pct = "Proportion",
    table = tab_maker(df_giver, primary_receiver_age_group, "CRRCPAGR"),
    title_fragment = "of Caregiving Respondents and the Age of Respondent's Primary Care Receiver"
  ),
  "Hours of Help Respondent Provides to Caree" = list(
    index = 3,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = help_hours,
    code = "HAP_10C",
    y = NULL,
    caption = "Count of the number of hours of help provided per activity per week by respondent considered to be a care giver",
    title = "Number of hours of help provided - Per average week per activity.",
    x_axis = "Time (hours)",
    y_axis = "Count",
    caption_pct = "Proportion of the number of hours of help provided per activity per week by respondent considered to be a care giver.",
    y_axis_pct = "Proportion",
    table = tab_maker(df_giver, help_hours, "HAP_10C"),
    title_fragment = "of Caregiving Respondents and the Number of Hours of Help Provided - Per Average Week"
  ),
  "Distance to Caree" = list(
    index = 4,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = dwelling_distances,
    code = "PRD_10",
    y = NULL,
    caption = "Counts for the distance by car between respondents considered to be a care giver, and their caree's dwelling.",
    title = "Distance between the respondents's and care-receiver's dwellings.",
    x_axis = "Distance by car",
    y_axis = "Count",
    caption_pct = "Proportion for the distance by car between respondents considered to be a care giver, and their caree's dwelling.",
    y_axis_pct = "Proportion",
    table = tab_maker(df_giver, dwelling_distances, "PRD_10"),
    title_fragment = "of Caregiving Respondents and the Distance Between them and the Care Receiver's Dwellings"
  ),
  "Banking Help Provided to Caree - frequency" = list(
    index = 5,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = primary_help_banking_freq,
    code = "ARB_20",
    y = NULL,
    caption = "Count of the number of times the respondent considered to be a care giver assisted caree with banking",
    title = "Helped primary care receiver with banking - Frequency.",
    x_axis = "Help Frequency",
    y_axis = "Count",
    caption_pct = "Proportion of the number of times the respondent considered to be a care giver assisted caree with banking.",
    y_axis_pct = "Proportion",
    table = tab_maker(df_giver, primary_help_banking_freq, "ARB_20"),
    title_fragment = "of Caregiving Respondents and the Frequency they Provided Help to Their Primary Care Receiver with Banking"
  ),
  "Banking Help Provided to Caree - hours" = list(
    index = 6,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = primary_help_banking_hours,
    code = "ARB_30C",
    y = NULL,
    caption = "Count of the number of hours the respondent considered to be a care giver assisted caree with banking",
    title = "Primary caregiver helped with banking - Number of hours.",
    x_axis = "Hours helped",
    y_axis = "Count",
    caption_pct = "Proportion of the number of hours the respondent considered to be care giver assisted caree with banking",
    y_axis_pct = "Proportion",
    table = tab_maker(df_giver, primary_help_banking_hours, "ARB_30C"),
    title_fragment = "of Caregiving Respondents and Number of Hours they Provided Help with Banking"
  ),
  "Out of Pocket Caregiving Expenses" = list(
    index = 11,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = out_of_pocket_expenses,
    code = out_of_pocket_codes,
    y = y_out_of_pocket,
    caption = "Count of out-of-pocket expenses of the respondent considered to be a care giver for a caree's caregiving responsibilities",
    title = "Out-of-pocket expenses because of caregiving responsibilities.",
    x_axis = "Expense categories",
    y_axis = "Count",
    caption_pct = "Proportion of out-of-pocket expenses of the respondent considered to be care giver for a caree's caregiving responsibilites.",
    y_axis_pct = "Proportion",
    table = tab_multi_var_maker(df_giver, out_of_pocket_expenses, out_of_pocket_codes, y_out_of_pocket),
    title_fragment = "of Caregiving Respondents who had out-of-pocket Expenses From Caregiving - Past 12 months"
  ),
  "Respondent has a Diability Indicator" = list(
    index = 12,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = disability_indicators,
    code = disability_codes,
    y = y_disability_indicator,
    caption = "Frequency of the type of disability indicators within respondents considered to be a care giver",
    title = "Classes of Disability Indicators Reported by Respondent",
    x_axis = "Types of Disability Indicators",
    y_axis = "Count",
    caption_pct = "Proportion of the type of disability indicators within respondents considered to be a care giver.",
    y_axis_pct = "Proportion",
    table = tab_multi_var_maker(df_giver, disability_indicators, disability_codes, y_disability_indicator),
    title_fragment = "of Caregiving Respondents who have a Disability indicator"
  ),
  "Conditions that would enable Respondent to provide end-of-life care in their own home" = list(
    index = 13,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = end_of_life_care,
    code = end_of_life_care_codes,
    y = y_end_of_life_care,
    caption = "Frequency of the types of conditions given by respondents considered to be a care giver",
    title = "Conditions that would enable Respondent to provide end-of-life care in their own home",
    x_axis = "Conditions",
    y_axis = "Count",
    caption_pct = "Proportion of the types of conditions given by respondents considered to be a care giver",
    y_axis_pct = "Proportion",
    table = tab_multi_var_maker(df_giver, end_of_life_care, end_of_life_care_codes, y_end_of_life_care),
    title_fragment = "of Caregiving Respondents who prefer providing end-of-life care at home"
  ),
  "Social Consequences of Respondent's Caregiving Responsibilities" = list(
    index = 14,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = caregiving_social_consequences,
    code = caregiving_social_consequences_codes,
    y = y_caregiving_social_consequences,
    caption = "Frequency of the types of consequences faced by respondents considered to be a care giver",
    title = "Social Consequences of the Respondent's Caregiving Responsibilities",
    x_axis = "Social Conseqeunces",
    y_axis = "Count",
    caption_pct = "Proportion of the types of consequences faced by respondents considered to be care giver",
    y_axis_pct = "Proportion",
    table = tab_multi_var_maker(df_giver, end_of_life_care, end_of_life_care_codes, y_end_of_life_care),
    title_fragment = "of Caregiving Respondents facing different types of social consequences due to caregiving"
  ),
  "Financial Hardship due to Caregiving" = list(
    index = 15,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = financial_hardship,
    code = financial_hardship_codes,
    y = y_financial_hardship,
    caption = "Count of the types of Financial Hardships faced by respondents considered to be a care giver",
    title = "Financial Hardships due to Caregiving",
    x_axis = "Types of Financial Harships",
    y_axis = "Count",
    caption_pct = "Proportion of the types of Financial Hardships faced by respondents considered to be care giver",
    y_axis_pct = "Proportion",
    table = tab_multi_var_maker(df_giver, financial_hardship, financial_hardship_codes, y_financial_hardship),
    title_fragment = "who Experienced Financial Hardship Because of Caregiving Responsibilities"
  )
)

group_by_options <- list(
  "None" = 1,
  "Sex" = 2,
  "Age group" = 3,
  "Alzheimers's/Dementia" = 4
  # "Living arrangement" = 4,
  # "Visible minority status" = 5
)
show_group <- list("false", "true")
default <- 0

ui <- function(request) {
  print(request)
  
  fluidPage(
    useShinyjs(),
    includeCSS("www/app.css"),
    includeCSS("www/app.css"),
    titlePanel("Explore the 2018 General Social Survey on Caregiving and Care
      Receiving"),
    withTags(
      fluidRow(
        p("This data represents a subset of information from the survey. The
            information here is data from respondents who are:"),
        ol(
          li("Care receivers who are 65 years old or older, or"),
          li("Caregivers who provide assistance to individuals who are 65
              years old or older")
          # removing this from the text header
          # li("Both care receivers who are 65 years old or older while
          # simultaneously acting as caregivers to other care receivers who are
          # 65 years old or older"),
          #li("People 65 or older who need help but are not currently
          #    receiving care.")
        ),
        p("These groups of respondents are likely providing insights into the
            experiences and challenges related to receiving or providing care for
            older adults. The data may include information about their health,
            financial situation, quality of life, and other factors relevant to
            caregiving and care receiving."),
        width = 10
      )
    ),
    tabsetPanel(
      id = "chart_panel",
      tabPanel(
        "General Charts",
        fluidRow(
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "general_selected_box",
                "General info:",
                choices = general_charts,
                selected = default
              )
            ),
            mainPanel(
              tabsetPanel(
                id = "general_chart_type",
                tabPanel(
                  "Counts",
                  plotOutput("general_selected_chart"),
                  uiOutput("conditional_additional_plot")
                ),
                tabPanel(
                  "Percentages",
                  plotOutput("general_percentage"),
                  uiOutput("conditional_additional_pct_plot")
                ),
                tabPanel(
                  "Tables",
                  tableOutput("general_table"),
                  uiOutput("conditional_additional_table")
                ) 
              )
            )
          )
        )
      ),
      tabPanel(
        "Receiver Response Charts",
        fluidRow(
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "receiver_select_box",
                "Questions asked to older adults who received care:",
                choices = names(receiver_ui_config),
                selected = default
              ),
              selectInput(
                "receiver_select_box_sex",
                "Filter by sex of older adult:",
                choices = filter_sex,
                selected = default
              ),
              selectInput(
                "receiver_select_box_age",
                "Filter by age group of older adult:",
                choices = filter_age_group,
                selected = default
              ),
              selectInput(
                "receiver_select_box_pop_centre",
                "Filter by urban/rural status of older adult:",
                choices = filter_pop_centre,
                selected = default
              ),
              selectInput(
                "receiver_select_box_living_arrangement_senior_household",
                "Filter by living arrangement of older adult:",
                choices = filter_living_arrangement_senior_household,
                selected = default
              ),
              selectInput(
                "receiver_select_box_indigenous_status",
                "Filter by indigenous status of older adult:",
                choices = filter_indigenous_status,
                selected = default
              ),
              selectInput(
                "receiver_select_box_visible_minority",
                "Filter by visible minority status of older adult:",
                choices = filter_visible_minority_status,
                selected = default
              ),
              selectInput(
                "receiver_select_box_group_religious_participation",
                "Filter by religious participation of older adult:",
                choices = filter_group_religious_participation,
                selected = default
              ),
              radioButtons("receiver_radio",
                "Group by:",
                choices = group_by_options[1:3],
                selected = 1
              )
            ),
            mainPanel(
              tabsetPanel(
                id = "receiver_chart_type",
                tabPanel(
                  "Counts",
                  fluidRow(
                    column(width = 4, p(HTML("Do you want to save your vignette?"))),
                    column(width = 4, actionButton("savebtn_rc", "Save"))
                  ),
                  hr(),
                  plotOutput("receiver_selected_chart"),
                  br(),
                  fluidRow(
                    p(HTML("<strong>Filters Applied: </strong>")),
                    uiOutput("filters_applied_receiver")
                  ),
                  fluidRow(
                    uiOutput("group_by_applied_receiver")
                  ),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetReceiverCount", "Reset"))
                  )
                ),
                tabPanel(
                  "Percentages",
                  fluidRow(
                    column(width = 4, p(HTML("Do you want to save your vignette?"))),
                    column(width = 4, actionButton("savebtn_rp", "Save"))
                  ),
                  hr(),
                  plotOutput("receiver_percentage"),
                  br(),
                  fluidRow(
                    p(HTML("<strong>Filters Applied: </strong>")),
                    uiOutput("filters_applied_receiver_percentage")
                  ),
                  fluidRow(
                    uiOutput("group_by_applied_receiver_percentage")
                  ),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetReceiverPercentage", "Reset"))
                    )
                  ),
                tabPanel(
                  "Tables",
                  tableOutput("receiver_table"),
                  hr(),
                  fluidRow(
                    p(HTML("<strong>Filters Applied: </strong>")),
                    uiOutput("filters_applied_receiver_table")
                  ),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetReceiverTable", "Reset"))
                  )
                ) 
              )
            )
          )
        )
      ),
      tabPanel(
        "Giver Response Charts",
        fluidRow(
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "giver_select_box",
                "Questions asked to respondents who provided care to older
                  adults:",
                choices = names(giver_ui_config),
                selected = names(giver_ui_config[1])
              ),
              selectInput(
                "giver_select_box_sex",
                "Filter by sex of caree",
                choices = filter_sex,
                selected = default
              ),
              selectInput(
                "giver_select_box_age",
                "Filter by age of primary caree",
                filter_age_group,
                selected = default
              ),
              selectInput(
                "giver_select_box_own_age",
                "Filter by age respondent",
                filter_own_age_group,
                selected = default
              ),
              selectInput(
                "giver_select_box_pop_centre",
                "Filter by urban/rural status of caree",
                filter_pop_centre,
                selected = default
              ),
              selectInput(
                "giver_select_box_living_arrangement_senior_household",
                "Filter by living arrangement of caree",
                filter_living_arrangement_senior_household,
                selected = default
              ),
              selectInput(
                "giver_select_box_indigenous_status",
                "Filter by indigenous status of caree",
                filter_indigenous_status,
                selected = default
              ),
              selectInput(
                "giver_select_box_visible_minority",
                "Filter by visible minority status of caree",
                filter_visible_minority_status,
                selected = default
              ),
              selectInput(
                "giver_select_box_group_religious_participation",
                "Filter by religious participation of caree",
                filter_group_religious_participation,
                selected = default
              ),
              selectInput(
                "giver_select_box_receiver_main_health_condition",
                "Filter by caree's main health condition",
                filter_receiver_main_health_conditions,
                selected = default
              ),
              radioButtons(
                "giver_radio",
                "Group by:",
                choices = group_by_options,
                selected = 1
              )
            ),
            mainPanel(
              tabsetPanel(
                id = "giver_chart_type",
                tabPanel(
                  "Counts",
                  fluidRow(
                    column(width = 4, p(HTML("Do you want to save your vignette?"))),
                    column(width = 4, actionButton("savebtn_gc", "Save"))
                  ),
                  hr(),
                  plotOutput("giver_selected_chart"),
                  fluidRow(
                    p(HTML("<strong>Filters Applied: </strong>")),
                    uiOutput("filters_applied_giver")
                  ),
                  uiOutput("group_by_applied_giver"),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetGiverCount", "Reset"))
                  )
                ),
                tabPanel(
                  "Percentages",
                  fluidRow(
                    column(width = 4, p(HTML("Do you want to save your vignette?"))),
                    column(width = 4, actionButton("savebtn_gp", "Save"))
                  ),
                  hr(),
                  plotOutput("giver_percentage"),
                  fluidRow(
                    p(HTML("<strong>Filters Applied: </strong>")),
                    uiOutput("filters_applied_giver_percentage")
                  ),
                  uiOutput("group_by_applied_giver_percentage"),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetGiverPercentage", "Reset"))
                  )
                ), # giver percentages
                tabPanel(
                  "Tables",
                  tableOutput("giver_table"),
                  hr(),
                  fluidRow(
                    p(HTML("<strong>Filters Applied: </strong>")),
                    uiOutput("filters_applied_giver_table")
                  ),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetGiverTable", "Reset"))
                  )
                ) 
              )
            )
          )
        )
      ),
      # Data Vignettes tab for users to save their own charts.
      tabPanel(
        "Data Vignettes",
        id = "data_vignettes",
        uiOutput("saved_charts_ui")
        
      ) # end Data Vignettes
    ),
    fluidRow(
      h3("Highlighted Charts/Tables"),
      em("Below are some interesting views chosen by our research team. Click
          to take a look, and start your own investigation from that point!")
    ),
    withTags(
      fluidRow(
        style = "text-align: center;",
        div(
          class = "col-xs-6 col-md-3",
          a(
            class = "thumbnail bg-info",
            href = "/?_inputs_&chart_panel=%22Giver%20Response%20Charts%22&general_chart_type=%22Counts%22&receiver_chart_type=%22Counts%22&giver_chart_type=%22Percentages%22&general_selected_box=%22Respondent%20Groups%22&receiver_select_box=%22Health%20Conditions%20Experienced%22&receiver_select_box_sex=%22-1%22&receiver_select_box_age=%22-1%22&receiver_select_box_pop_centre=%22-1%22&receiver_select_box_partner_in_household=%22-1%22&receiver_select_box_living_arrangement_senior_household=%22-1%22&receiver_select_box_indigenous_status=%22-1%22&receiver_select_box_visible_minority=%22-1%22&receiver_select_box_group_religious_participation=%22-1%22&giver_select_box=%22Distance%20to%20Caree%22&giver_select_box_sex=%22-1%22&giver_select_box_age=%22-1%22&giver_select_box_pop_centre=%22-1%22&giver_select_box_partner_in_household=%22-1%22&giver_select_box_living_arrangement_senior_household=%22-1%22&giver_select_box_indigenous_status=%22-1%22&giver_select_box_visible_minority=%221%22&giver_select_box_group_religious_participation=%22-1%22&giver_select_box_receiver_main_health_condition=%22-1%22&receiver_radio=%221%22&giver_radio=%221%22",
            p(
              class = "h4 text-center",
              "Caregivers of visible minority are much more likely to live with
              the older adult they provide care for."
            )
          )
        ),
        div(
          class = "col-xs-6 col-md-3",
          a(
            class = "thumbnail bg-warning",
            href = "/?_inputs_&chart_panel=%22Receiver%20Response%20Charts%22&general_chart_type=%22Counts%22&receiver_chart_type=%22Percentages%22&giver_chart_type=%22Counts%22&general_selected_box=%22Sex%20of%20Primary%20Caregivers%20and%20Care%20Receivers%22&receiver_select_box=%22Banking%20Help%20Received%20-%20frequency%22&receiver_select_box_sex=%22-1%22&receiver_select_box_age=%22-1%22&receiver_select_box_pop_centre=%22-1%22&receiver_select_box_partner_in_household=%22-1%22&receiver_select_box_living_arrangement_senior_household=%22-1%22&receiver_select_box_indigenous_status=%22-1%22&receiver_select_box_visible_minority=%22-1%22&receiver_select_box_group_religious_participation=%22-1%22&giver_select_box=%22Activities%20Respondent%20Assists%20Caree%20With%22&giver_select_box_sex=%22-1%22&giver_select_box_age=%22-1%22&giver_select_box_pop_centre=%22-1%22&giver_select_box_partner_in_household=%22-1%22&giver_select_box_living_arrangement_senior_household=%22-1%22&giver_select_box_indigenous_status=%22-1%22&giver_select_box_visible_minority=%22-1%22&giver_select_box_group_religious_participation=%22-1%22&giver_select_box_receiver_main_health_condition=%22-1%22&receiver_radio=%222%22&giver_radio=%221%22",
            p(
              class = "h4 text-center",
              "Male care receivers were much more likely to receive help with
              banking daily than female care receivers."
            )
          )
        ),
        div(
          class = "col-xs-6 col-md-3",
          a(
            class = "thumbnail bg-danger",
            href = "/?_inputs_&chart_panel=%22Giver%20Response%20Charts%22&general_chart_type=%22Counts%22&receiver_chart_type=%22Percentages%22&giver_chart_type=%22Counts%22&general_selected_box=%22Sex%20of%20Primary%20Caregivers%20and%20Care%20Receivers%22&receiver_select_box=%22Banking%20Help%20Received%20-%20frequency%22&receiver_select_box_sex=%22-1%22&receiver_select_box_age=%22-1%22&receiver_select_box_pop_centre=%22-1%22&receiver_select_box_partner_in_household=%22-1%22&receiver_select_box_living_arrangement_senior_household=%22-1%22&receiver_select_box_indigenous_status=%22-1%22&receiver_select_box_visible_minority=%22-1%22&receiver_select_box_group_religious_participation=%22-1%22&giver_select_box=%22Banking%20Help%20Provided%20to%20Caree%20-%20frequency%22&giver_select_box_sex=%22-1%22&giver_select_box_age=%22-1%22&giver_select_box_pop_centre=%22-1%22&giver_select_box_partner_in_household=%22-1%22&giver_select_box_living_arrangement_senior_household=%22-1%22&giver_select_box_indigenous_status=%22-1%22&giver_select_box_visible_minority=%22-1%22&giver_select_box_group_religious_participation=%22-1%22&giver_select_box_receiver_main_health_condition=%22-1%22&receiver_radio=%221%22&giver_radio=%224%22",
            p(
              class = "h4 text-center",
              "Participants who cared for older adults with Alzheimer's or
              dementia were more likely to provide banking help more
              frequently."
            )
          )
        ),
        div(
          class = "col-xs-6 col-md-3",
          a(
            class = "thumbnail bg-success",
            href = "/?_inputs_&chart_panel=%22Receiver%20Response%20Charts%22&general_chart_type=%22Counts%22&receiver_chart_type=%22Counts%22&giver_chart_type=%22Counts%22&general_selected_box=%22Sex%20of%20Primary%20Caregivers%20and%20Care%20Receivers%22&receiver_select_box=%22Hours%20of%20Help%20Received%20per%20Week%22&receiver_select_box_sex=%22-1%22&receiver_select_box_age=%22-1%22&receiver_select_box_pop_centre=%22-1%22&receiver_select_box_partner_in_household=%22-1%22&receiver_select_box_living_arrangement_senior_household=%22-1%22&receiver_select_box_indigenous_status=%22-1%22&receiver_select_box_visible_minority=%22-1%22&receiver_select_box_group_religious_participation=%22-1%22&giver_select_box=%22Banking%20Help%20Provided%20to%20Caree%20-%20frequency%22&giver_select_box_sex=%22-1%22&giver_select_box_age=%22-1%22&giver_select_box_pop_centre=%22-1%22&giver_select_box_partner_in_household=%22-1%22&giver_select_box_living_arrangement_senior_household=%22-1%22&giver_select_box_indigenous_status=%22-1%22&giver_select_box_visible_minority=%22-1%22&giver_select_box_group_religious_participation=%22-1%22&giver_select_box_receiver_main_health_condition=%22-1%22&receiver_radio=%221%22&giver_radio=%224%22",
            p(
              class = "h4 text-center",
              "The majority of participants receive less than 10 hours of care
              per week, although it could be even less than that!"
            )
          )
        )
      )
    )
  )
}

server <- function(input, output, session) { # nolint: cyclocomp_linter.
  
  observeEvent(input$receiver_radio, {
    if (input$receiver_radio != 1) {
      disable("radio_select_box")
    } else {
      enable("radio_select_box")
    }
  })
  
  observe({
    # This observer triggers every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  output_receiver_df <- df_receiver
  output_giver_df <- df_giver
  # general counts tab
  output$general_selected_chart <- renderPlot({
    if (input$general_selected_box == general_charts[1]) {
      chart_general(pop_name, pop_freq, "GSS 2018 - Respondent groups", "Count of respondents in each grouping: caregivers, care receivers, and persons with unmet caregiving needs.", "Respondent groups", "Count")
    } else if (input$general_selected_box == general_charts[2]) {
      chart_general(primary_sex, receiver_sex_freq, "GSS 2018 - Care receiver Respondents by sex", "Count of care receiver respondents by sex", "Sex", "Count")
    } else if (input$general_selected_box == general_charts[3]) {
      chart_general(primary_sex, giver_sex_freq, "GSS 2018 - Caregiver Respondents by sex", "Count of caregiver respondents by sex", "Sex", "Count")
    } else if (input$general_selected_box == general_charts[4]) {
      chart_general(caree_relationship, caree_freq, "GSS 2018 - Relationship of Primary Caregiver with Respondent (Care Receiver)", "Count of relationships in each grouping: Spouse/Partner, Son, Daughter, Parent, Other Family Members, Other.", "Caree Relationships", "Count")
    } else if (input$general_selected_box == general_charts[5]) {
      chart_general(disability_counter, disability_freq, "GSS 2018 - Number of Disability Types in both Caree and Carer - Grouped", "Count of respondents (both caree and caregiver) in each grouping: None, 1, 2 or 3, >3.", "Groups of Disability Counts (None, 1, 2 or 3, >3.", "Counts")
    }
  })
 
  output$conditional_additional_plot <- renderUI({
    if (input$general_selected_box == general_charts[2] || input$general_selected_box == general_charts[3]) {
      plotOutput("additional_plot")
    }
  })
  
  output$additional_plot <- renderPlot({
    if (input$general_selected_box == general_charts[2]) {
     chart_general(primary_receiver_sex, primary_receiver_sex_freq, "Care receivers Respondents and their primary caregivers by Sex", "Count of respondents in each grouping: male carees with male carers, male carees with female cares,
                   female carees with male carers, female carees with female carers", "Sex", "Count")
    } else if(input$general_selected_box == general_charts[3]){
      chart_general(primary_giver_sex, primary_giver_sex_freq, "Caregiver Respondents and their primary care receivers by Sex", 'Count of respondents in each grouping: male carers with male carees, male carers with female carees,
                    female carers with male carees, female carers with female carees', "Sex", "Count")
    }
  })
  
  # general percentage
  output$general_percentage <- renderPlot({
    if (input$general_selected_box == general_charts[1]) {
      chart_general_pct(pop_name, pop_freq, "GSS 2018 - Respondent groups", "Proportion of respondents in each grouping: caregivers, care receivers, and persons with unmet caregiving needs", "Respondent groups", "Proportion")
    } else if(input$general_selected_box == general_charts[2]){
      chart_general_pct(primary_sex, receiver_sex_freq, "GSS 2018 - Care receiver Respondents by sex", "Proportion of care receiver respondents by sex", "Sex", "Proportion")
    } else if(input$general_selected_box == general_charts[3]){
      chart_general_pct(primary_sex, giver_sex_freq, "GSS 2018 - Caregiver Respondents by sex", "Proportion of caregiver respondents by sex", "Sex", "Proportion")
    } else if(input$general_selected_box == general_charts[4]){
      chart_general_pct(caree_relationship, caree_freq, "GSS 2018 - Relationship of Primary Caregiver with Respondent (Care Receiver)", "Proportion of relationships in each grouping: Spouse/Partner, Son, Daughter, Parent, Other Family Members, Others.", "Caree Relationships", "Proportion")
    } else if(input$general_selected_box == general_charts[5]){
      chart_general_pct(disability_counter, disability_freq, "GSS 2018 - Number of Disability Types in both Caree and Carer - Grouped", "Proportion of respondents (both caree and caregiver) in each grouping: None, 1, 2 or 3, >3.", "Groups of Disability Counts(None, 1, 2 or 3, >3", "Proportion")
    }
  })
  
  output$conditional_additional_pct_plot <- renderUI({
    if (input$general_selected_box == general_charts[2] || input$general_selected_box == general_charts[3]) {
      plotOutput("additional_pct_plot")
    }
  })
  
  output$additional_pct_plot <- renderPlot({
    if (input$general_selected_box == general_charts[2]) {
      chart_general_pct(primary_receiver_sex, primary_receiver_sex_freq, "Care receivers Respondents and their primary caregivers by Sex", "Propotion of respondents in each grouping: male carees with male carers, male carees with female cares,
                   female carees with male carers, female carees with female carers", "Sex", "Proportion")
    } else if(input$general_selected_box == general_charts[3]){
      chart_general_pct(primary_giver_sex, primary_giver_sex_freq, "Caregiver Respondents and their primary care receivers by Sex", 'Propotion of respondents in each grouping: male carers with male carees, male carers with female carees,
                    female carers with male carees, female carers with female carees', "Sex", "Propotion")
    }
  })
  
  # general table
  output$general_table <- renderTable({
    if (input$general_selected_box == general_charts[1]) {
      tab_general(pop_name, pop_freq)
    } else if(input$general_selected_box == general_charts[2]){
      tab_general(primary_sex, receiver_sex_freq)
    } else if(input$general_selected_box == general_charts[3]){
      tab_general(primary_sex, giver_sex_freq)
    } else if(input$general_selected_box == general_charts[4]){
      tab_general(caree_relationship, caree_freq)
    } else if(input$general_selected_box == general_charts[5]){
      tab_general(disability_counter, disability_freq)
    }
  })
  
  output$conditional_additional_table <- renderUI({
    if (input$general_selected_box == general_charts[2] || input$general_selected_box == general_charts[3]) {
      tableOutput("additional_table")
    }
  })
  
  output$additional_table <- renderTable({
    if (input$general_selected_box == general_charts[2]) {
      tab_general(primary_receiver_sex, primary_receiver_sex_freq)
    } else if(input$general_selected_box == general_charts[3]){
      tab_general(primary_giver_sex, primary_giver_sex_freq)
    }
  })
  
  ### Receiver filters and charts
  update_receiver_df <- reactive({
    filtered_df <- apply_filter(
      df_receiver,
      strtoi(input$receiver_select_box_sex), "SEX"
    )
    filtered_df <- apply_filter(
      filtered_df,
      strtoi(input$receiver_select_box_age), "AGEGR10"
    )
    filtered_df <- apply_filter(
      filtered_df,
      strtoi(input$receiver_select_box_pop_centre), "LUC_RST"
    )
    filtered_df <- apply_filter(
      filtered_df,
      strtoi(input$receiver_select_box_living_arrangement_senior_household),
      "LIVARRSN"
    )
    filtered_df <- apply_filter(
      filtered_df,
      strtoi(input$receiver_select_box_indigenous_status), "AMB_01_1"
    )
    filtered_df <- apply_filter(
      filtered_df,
      strtoi(input$receiver_select_box_visible_minority), "VISMIN"
    )
    filtered_df <- apply_filter(
      filtered_df,
      strtoi(input$receiver_select_box_group_religious_participation), "REE_02"
    )
    
    output_receiver_df <<- filtered_df
    
    male_pop <-  sum(output_receiver_df$SEX == 1)
    female_pop <-  sum(output_receiver_df$SEX == 2)
    age_65_74_pop <- sum(output_receiver_df$AGEGR10 == 6)
    age_75_pop <- sum(output_receiver_df$AGEGR10 == 7)
    
    group_temp <- output$group_by_applied_receiver <- renderUI({
      if(input$receiver_radio == 2){
        HTML(paste("<strong>Filtered Population: </strong>", male_pop+female_pop, "<br>Male Care receiver respondents: ", male_pop,
                   "<br>Female Care receiver respondents: ", female_pop))
      }
      else if(input$receiver_radio == 3){
        HTML(paste("<strong>Filtered Population: </strong>", age_65_74_pop+age_75_pop, "<br>Care Receiver respondents aged 65-74: ", age_65_74_pop,
                   "<br>Care Receiver respondents aged 75+: ", age_75_pop))
      }
    })
    
    output$group_by_applied_receiver_percentage <- renderUI({group_temp})
  })
  
  # receiver counts tab
  output$receiver_selected_chart <- renderPlot({
    update_receiver_df()
    
    dataset_name <- input$receiver_select_box
    config <- receiver_ui_config[[dataset_name]]
    
    if (input$receiver_radio == 2) {
      group_by_sex(
        output_receiver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else if (input$receiver_radio == 3) {
      group_by_age(
        output_receiver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else {
      config$count_chart(output_receiver_df, config$input_vector, config$code, config$y, config$title, config$caption, config$x_axis, config$y_axis)
    }
  })
  
  # TODO : FIX receiver percentage tab
  output$receiver_percentage <- renderPlot({
    update_receiver_df()
    
    dataset_name <- input$receiver_select_box
    config <- receiver_ui_config[[dataset_name]]
    
    if (input$receiver_radio == 2) {
      group_by_sex_percent(
        output_receiver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else if (input$receiver_radio == 3) {
      group_by_age_percent(
        output_receiver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else {
      config$pct_chart(output_receiver_df, config$input_vector, config$code, config$y, config$title, config$caption_pct, config$x_axis, config$y_axis_pct)
    }
  })
  
  
  # receiver table tab
  output$receiver_table <- renderTable({
    update_receiver_df()
    dataset_name <- input$receiver_select_box
    config <- receiver_ui_config[[dataset_name]]
    filtered_table <- tab_chooser(output_receiver_df, config$input_vector, config$code, config$y)
    
    if(input$receiver_radio == 2){
      final_table <- filtered_table |>
        select(x_options, count, percentage, Male, male_percentage, Female, female_percentage) 
    } else if(input$receiver_radio == 3){
      final_table <- filtered_table |>
        select(x_options, count, percentage, age_65_74, age_65_74_percentage, age_75, age_75_percentage)
    } else{
      final_table <- filtered_table |>
        select(x_options, count, percentage)
    }
    final_table <- final_table |>
      rename(!!dataset_name := x_options)
    
    final_table
  })
  
  
  resetReceiverSelections <- function(session) {
    updateRadioButtons(session, "receiver_radio", selected = 1)
    updateSelectInput(session, "receiver_select_box_sex", selected = -1)
    updateSelectInput(session, "receiver_select_box_age", selected = -1)
    updateSelectInput(session, "receiver_select_box_pop_centre", selected = -1)
    updateSelectInput(session, "receiver_select_box_living_arrangement_senior_household", selected = -1)
    updateSelectInput(session, "receiver_select_box_indigenous_status", selected = -1)
    updateSelectInput(session, "receiver_select_box_visible_minority", selected = -1)
    updateSelectInput(session, "receiver_select_box_group_religious_participation", selected = -1)
    update_receiver_df()
    update_giver_df()
  }
  
  observeEvent(c(input$resetReceiverCount, input$resetReceiverPercentage, input$resetReceiverTable), {
    resetReceiverSelections(session)
  }, ignoreInit = TRUE)
  
  
  # Live filter updates- Receiver charts
  temp <- output$filters_applied_receiver <- renderUI({
    applied_filters <- list()
    
    if(input$receiver_select_box_sex != "-1"){
      applied_filters <- c(applied_filters, paste("Sex: ", names(filter_sex)[which(filter_sex == input$receiver_select_box_sex)]))
    }
    if(input$receiver_select_box_age != "-1"){
      applied_filters <- c(applied_filters, paste("Primary Carer's Age group: ", names(filter_age_group)[which(filter_age_group == input$receiver_select_box_age)]))
    }
    if(input$receiver_select_box_pop_centre != "-1"){
      applied_filters <- c(applied_filters, paste("Urban/ Rural status: ", names(filter_pop_centre)[which(filter_pop_centre == input$receiver_select_box_pop_centre)]))
    }
    if(input$receiver_select_box_living_arrangement_senior_household != "-1"){
      applied_filters <- c(applied_filters, paste("Living Arrangement: ", names(filter_living_arrangement_senior_household)[which(filter_living_arrangement_senior_household == input$receiver_select_box_living_arrangement_senior_household)]))
    }
    if(input$receiver_select_box_indigenous_status != "-1"){
      applied_filters <- c(applied_filters, paste("Indigenous status: ", names(filter_indigenous_status)[which(filter_indigenous_status == input$receiver_select_box_indigenous_status)]))
    }
    if(input$receiver_select_box_visible_minority != "-1"){
      applied_filters <- c(applied_filters, paste("Visible minority status: ", names(filter_visible_minority_status)[which(filter_visible_minority_status == input$receiver_select_box_visible_minority)]))
    }
    if(input$receiver_select_box_group_religious_participation != "-1"){
      applied_filters <- c(applied_filters, paste("Religious Participation: ", names(filter_group_religious_participation)[which(filter_group_religious_participation == input$receiver_select_box_group_religious_participation)]))
    }
    
    if(length(applied_filters)==0){
      "None"
    } else {
      tags$ul(
        lapply(applied_filters, function(filter){
          tags$li(filter)
        })
      )
    }
  })
  
  output$filters_applied_receiver_percentage <- renderUI({temp})
  output$filters_applied_receiver_table <- renderUI({temp})
  
  ### Giver filters and charts
  update_giver_df <- reactive({
    # filter by sex
    df_filtered <- apply_filter(
      df_giver,
      strtoi(input$giver_select_box_sex), "SEX"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_age), "CRRCPAGR"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_own_age), "AGEGR10"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_pop_centre), "LUC_RST"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_living_arrangement_senior_household),
      "LIVARRSN"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_indigenous_status), "AMB_01_1"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_visible_minority), "VISMIN"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_group_religious_participation), "REE_02"
    )
    df_filtered <- apply_filter(
      df_filtered,
      input$giver_select_box_receiver_main_health_condition, "PRP10GR"
    )
    
    output_giver_df <<- df_filtered
    
    male_pop <-  sum(output_giver_df$SEX == 1)
    female_pop <-  sum(output_giver_df$SEX == 2)
    age_65_74_pop <- sum(output_giver_df$AGEGR10 == 6)
    age_75_pop <- sum(output_giver_df$AGEGR10 == 7)
    alzheimer_pop <- sum(output_giver_df$PRP10GR == 8)
    non_alzheimer_pop <- sum(output_giver_df$PRP10GR != 8)
    
    group_temp2 <- output$group_by_applied_giver <- renderUI({
      if(input$giver_radio == 2){
        HTML(paste("<strong>Filtered Population: </strong>", male_pop+female_pop, "<br>Male Caregiver Respondents: ", male_pop,
                   "<br>Female Caregiver Respondents: ", female_pop))
      }
      else if(input$giver_radio == 3){
        HTML(paste("<strong>Filtered Population: </strong>", age_65_74_pop+age_75_pop, "<br>Caregiver respondents aged 65-74: ", age_65_74_pop,
                   "<br>Caregiver respondents aged 75+: ", age_75_pop))
      }
      else if(input$giver_radio == 4){
        HTML(paste("<strong>Filtered Population: </strong>", alzheimer_pop+non_alzheimer_pop, "<br>Caregiver cares for a caree with alzheimer's: ", alzheimer_pop,
                   "<br>Caregiver cares for a caree with other health conditions: ", non_alzheimer_pop))
      }
    })
    
    output$group_by_applied_giver_percentage <- renderUI({group_temp2})
  })
  
  # giver counts tab
  output$giver_selected_chart <- renderPlot({
    update_giver_df()
    
    dataset_name <- input$giver_select_box
    config <- giver_ui_config[[dataset_name]]
    
    if (input$giver_radio == 2) {
      group_by_sex(
        output_giver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else if (input$giver_radio == 3) {
      group_by_age(
        output_giver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else if (input$giver_radio == 4) {
      group_by_alzheimers(
        output_giver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else {
      config$count_chart(output_giver_df, config$input_vector, config$code, config$y, config$title, config$caption, config$x_axis, config$y_axis)
    }
  })
  
  # giver percentage tab
  output$giver_percentage <- renderPlot({
    update_giver_df()
    
    dataset_name <- input$giver_select_box
    config <- giver_ui_config[[dataset_name]]
    
    if (input$giver_radio == 2) {
      group_by_sex_percent(
        output_giver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else if (input$giver_radio == 3) {
      group_by_age_percent(
        output_giver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else if (input$giver_radio == 4) {
      group_by_alzheimers_percent(
        output_giver_df, config$y, config$input_vector, config$code, dataset_name, config$title_fragment
      )
    } else {
      config$pct_chart(output_giver_df, config$input_vector, config$code, config$y, config$title, config$caption_pct, config$x_axis, config$y_axis_pct)
    }
  })
  
  # giver table tab
  output$giver_table <- renderTable({
    update_giver_df()
    dataset_name <- input$giver_select_box
    config <- giver_ui_config[[dataset_name]]
    filtered_table <- tab_chooser(output_giver_df, config$input_vector, config$code, config$y)
    
    if(input$giver_radio == 2){
      final_table <- filtered_table |>
        select(x_options, count, percentage, Male, male_percentage, Female, female_percentage) 
    } else if(input$giver_radio == 3){
      final_table <- filtered_table |>
        select(x_options, count, percentage, age_65_74, age_65_74_percentage, age_75, age_75_percentage)
    } else if(input$giver_radio == 4){
      final_table <- filtered_table |>
        select(x_options, count, percentage, alzheimers, alzheimers_percentage, non_alzheimers, non_alzheimers_percentage)
    } 
    else{
      final_table <- filtered_table |>
        select(x_options, count, percentage)
    }
    final_table <- final_table |>
      rename(!!dataset_name := x_options)
    
    final_table
  })
  
  # Update which filters are being applied live
  temp2 <- output$filters_applied_giver <- renderUI({
    applied_filters <- list()
    
    if (input$giver_select_box_sex != "-1") {
      applied_filters <- c(applied_filters, paste("Sex:", names(filter_sex)[which(filter_sex == input$giver_select_box_sex)]))
    }
    if (input$giver_select_box_age != "-1") {
      applied_filters <- c(applied_filters, paste("Age group:", names(filter_age_group[which(filter_age_group == input$giver_select_box_age)])))
    }
    if(input$giver_select_box_own_age != "-1"){
      applied_filters <- c(applied_filters, paste("Respondent's Age Group: ", names(filter_own_age_group)[which(filter_own_age_group == input$giver_select_box_own_age)]))
    }
    if (input$giver_select_box_pop_centre != "-1") {
      applied_filters <- c(applied_filters, paste("Urban/Rural status:", names(filter_pop_centre[which(filter_pop_centre == input$giver_select_box_pop_centre)])))
    }
    if (input$giver_select_box_living_arrangement_senior_household != "-1") {
      applied_filters <- c(applied_filters, paste("Living arrangement:", names(filter_living_arrangement_senior_household[which(filter_living_arrangement_senior_household == input$giver_select_box_living_arrangement_senior_household)])))
    }
    if (input$giver_select_box_indigenous_status != "-1") {
      applied_filters <- c(applied_filters, paste("Indigenous status:",  names(filter_indigenous_status[which(filter_indigenous_status == input$giver_select_box_indigenous_status)])))
    }
    if (input$giver_select_box_visible_minority != "-1") {
      applied_filters <- c(applied_filters, paste("Visible minority status:", names(filter_visible_minority_status[which(filter_visible_minority_status == input$giver_select_box_visible_minority)])))
    }
    if (input$giver_select_box_group_religious_participation != "-1") {
      applied_filters <- c(applied_filters, paste("Religious participation:",  names(filter_group_religious_participation[which(filter_group_religious_participation == input$giver_select_box_group_religious_participation)])))
    }
    if (input$giver_select_box_receiver_main_health_condition != "-1"){
      applied_filters <- c(applied_filters, paste("Main Health Condition:", names(filter_receiver_main_health_conditions[which(filter_receiver_main_health_conditions == input$giver_select_box_receiver_main_health_condition)])))
    }
    
    
    if (length(applied_filters) == 0) {
      "None"
    } else {
      tags$ul(
        lapply(applied_filters, function(filter) {
          tags$li(filter)
        })
      )
    }
  })
  output$filters_applied_giver_percentage <- renderUI({temp2})
  output$filters_applied_giver_table <- renderUI({temp2})
  
  resetGiverSelections <- function(session) {
    updateRadioButtons(session, "giver_radio", selected = 1)
    updateSelectInput(session, "giver_select_box_sex", selected = -1)
    updateSelectInput(session, "giver_select_box_age", selected = -1)
    updateSelectInput(session, "giver_select_box_own_age", selected = -1)
    updateSelectInput(session, "giver_select_box_pop_centre", selected = -1)
    updateSelectInput(session, "giver_select_box_living_arrangement_senior_household", selected = -1)
    updateSelectInput(session, "giver_select_box_indigenous_status", selected = -1)
    updateSelectInput(session, "giver_select_box_visible_minority", selected = -1)
    updateSelectInput(session, "giver_select_box_group_religious_participation", selected = -1)
    updateSelectInput(session, "giver_select_box_receiver_main_health_condition", selected = -1)
    update_receiver_df()
    update_giver_df()
  }
  
  observeEvent(c(input$resetGiverCount, input$resetGiverPercentage, input$resetGiverTable), {
    resetGiverSelections(session)
  }, ignoreInit = TRUE)
  
  
  
  # Saving data vignettes ----------------------------------------------------------
  input_selected <- reactiveVal(NULL)
  
  observeEvent(c(input$savebtn_rc, input$savebtn_rp, input$savebtn_gc, input$savebtn_gp), {
     
    if (input$savebtn_rc > 0 && input$savebtn_rc == max(input$savebtn_rc, input$savebtn_rp, input$savebtn_gc, input$savebtn_gp, na.rm = TRUE)) {
      clicked_button <- "savebtn_rc"
    } else if (input$savebtn_rp > 0 && input$savebtn_rp == max(input$savebtn_rc, input$savebtn_rp, input$savebtn_gc, input$savebtn_gp, na.rm = TRUE)) {
      clicked_button <- "savebtn_rp"
    } else if (input$savebtn_gc > 0 && input$savebtn_gc == max(input$savebtn_rc, input$savebtn_rp, input$savebtn_gc, input$savebtn_gp, na.rm = TRUE)) {
      clicked_button <- "savebtn_gc"
    } else if (input$savebtn_gp > 0 && input$savebtn_gp == max(input$savebtn_rc, input$savebtn_rp, input$savebtn_gc, input$savebtn_gp, na.rm = TRUE)) {
      clicked_button <- "savebtn_gp"
    }
    
    print(paste("Clicked button:", clicked_button))  # Debugging output
     
    input_selected(clicked_button)
     
     showModal(modalDialog(
       title = "Save Data Vignette",
       textInput("vignette_description", "Enter a short description for your vignette", placeholder = "Vignette description"),
       footer = tagList(
         modalButton("Cancel"),
         actionButton("confirm_save", "Save")
       )
     ))

   }, ignoreInit = TRUE)
  
  
  
  # Function to load saved charts from file
  load_saved_charts <- function() {
    if (file.exists("saved_charts.rds")) {
      return(readRDS("saved_charts.rds"))
    }
    return(list())  # Return empty list if no file exists
  }
  
  # Reactive value to store saved charts
  saved_charts <- reactiveVal(load_saved_charts())
  
  observeEvent(input$confirm_save, {
    clicked_button <- input_selected()
    removeModal()
    
    vignette_name <- input$vignette_name
    vignette_description <- input$vignette_description
    
  
    # Generate dynamic link based on selected chart type
    if (clicked_button %in% c("savebtn_rc", "savebtn_rp")) {
      # Receiver chart
      chart_link <- paste0(
        "/?_inputs_",
        "&chart_panel=%22Receiver%20Response%20Charts%22",
        "&general_chart_type=%22", input$general_selected_box, "%22",
        "&receiver_chart_type=%22", input$receiver_chart_type, "%22",
        "&receiver_select_box=%22", input$receiver_select_box, "%22",
        "&receiver_select_box_sex=%22", input$receiver_select_box_sex, "%22",
        "&receiver_select_box_age=%22", input$receiver_select_box_age, "%22",
        "&receiver_select_box_pop_centre=%22", input$receiver_select_box_pop_centre, "%22",
        "&receiver_select_box_living_arrangement_senior_household=%22", input$receiver_select_box_living_arrangement_senior_household, "%22",
        "&receiver_select_box_indigenous_status=%22", input$receiver_select_box_indigenous_status, "%22",
        "&receiver_select_box_visible_minority=%22", input$receiver_select_box_visible_minority, "%22",
        "&receiver_select_box_group_religious_participation=%22", input$receiver_select_box_group_religious_participation, "%22",
        "&receiver_radio=%22", input$receiver_radio, "%22"
      )
    } else if (clicked_button %in% c("savebtn_gc", "savebtn_gp")) {
      # Giver chart
      chart_link <- paste0(
        "/?_inputs_",
        "&chart_panel=%22Giver%20Response%20Charts%22",
        "&general_chart_type=%22", input$general_selected_box, "%22",
        "&giver_chart_type=%22", input$giver_chart_type, "%22",
        "&giver_select_box=%22", input$giver_select_box, "%22",
        "&giver_select_box_sex=%22", input$giver_select_box_sex, "%22",
        "&giver_select_box_age=%22", input$giver_select_box_age, "%22",
        "&giver_select_box_own_age=%22", input$giver_select_box_own_age, "%22",
        "&giver_select_box_pop_centre=%22", input$giver_select_box_pop_centre, "%22",
        "&giver_select_box_living_arrangement_senior_household=%22", input$giver_select_box_living_arrangement_senior_household, "%22",
        "&giver_select_box_indigenous_status=%22", input$giver_select_box_indigenous_status, "%22",
        "&giver_select_box_visible_minority=%22", input$giver_select_box_visible_minority, "%22",
        "&giver_select_box_group_religious_participation=%22", input$giver_select_box_group_religious_participation, "%22",
        "&giver_select_box_receiver_main_health_condition=%22", input$giver_select_box_receiver_main_health_condition, "%22",
        "&giver_radio=%22", input$giver_radio, "%22"
      )
    }
    
    
    # Define image filename and path2
    image_filename <- paste0("chart_", as.numeric(Sys.time()), ".png")
    image_path <- paste0("www/", image_filename)
    
    # Call Puppeteer script to capture screenshot
    system(paste("node generate_image.js", chart_link, image_path))
    
    current_chart <- list(
      vignette_description = vignette_description,
      chart_link = chart_link,
      chart_title = paste(vignette_description),
      chart_img = image_filename
    )
    
    chart_list <- saved_charts()
    chart_list[[length(chart_list) + 1]] <- current_chart
    saved_charts(chart_list)
    
    # Save to file for persistence
    saveRDS(chart_list, "saved_charts.rds")
  })
  
  # Render saved charts as clickable thumbnails
  output$saved_charts_ui <- renderUI({
    chart_list <- saved_charts()
    if (length(chart_list) == 0) return(tags$p("No saved charts yet."))
    
    div(
      class = "row",
      lapply(chart_list, function(chart) {
        div(
          class = "col-xs-6 col-md-3",
          a(
            class = "thumbnail bg-warning",
            href = chart$chart_link,
            img(src = paste0("/", chart$chart_img), width = "100%"),  # Show saved screenshot
            p(class = "h4 text-center", chart$chart_title)
          )
        )
      })
    )
  })
  
  
}
  

options <- list()

if (Sys.getenv("GSS32_PORT") != "") {
  options <- c(options, port = as.integer(Sys.getenv("GSS32_PORT")))
}
if (Sys.getenv("GSS32_HOST") != "") {
  options <- c(options, host = Sys.getenv("GSS32_HOST"))
}
if (toupper(Sys.getenv("GSS32_DEVMODE")) == "TRUE") {
  options <- c(options, list(
    shiny.autoreload = TRUE,
    shiny.autoreload.pattern = glob2rx("*.R"),
    shiny.fullstacktrace = FALSE
  ))
  devmode(TRUE)
}
enableBookmarking("url")
# Can re-add this if/when static images are added to the app. For the time being
# having this in here means the directory needs to be created in order to run
# the app without crashing.
# addResourcePath("img", "www/images")

shinyApp(ui = ui, server = server, options = options)
