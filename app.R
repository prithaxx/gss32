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
  "Sex of Primary Caregivers and Care Receivers",
  "Relationship between Caree and Receiver",
  "Number of Disability Types in Respondents"
)

receiver_ui_config <- list(
  "Health Conditions Experienced" = list(
    index = 1,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = health_conditions,
    y = NULL,
    code = "PRA_10GR",
    caption = "Count for main health conditions for which respondents considered to be a care receiver and 65 years of age or older received help.",
    title = "Health Conditions Experienced by Respondents",
    x_axis = "Health Condition",
    y_axis = "Count",
    caption_pct = "Proportion of care receiver respondents reporting item as their main health condition.",
    y_axis_pct = "Proportion of Care Receiver Respondents (65+)",
    table = tab_maker(df_receiver, health_conditions, "PRA_10GR"),
    title_fragment = "of People with Health Conditions"
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
    title_fragment = "of People who Received Help with an Activity"
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
    title_fragment = "of people and the Age of Respondent's Primary Caregiver"
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
    title_fragment = "of People who Received Professional Help with an
      Activity"
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
    title_fragment = "of People and the Number of Hours of Help Received
      - Per Average Week"
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
    title_fragment ="of People and the Distance Between the Respondent and the Caregiver's
      Dwellings"
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
    title_fragment = "of People and the Frequency Their Primary Caregiver Helped
      with Banking"
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
    title_fragment = "of People and Number of Hours their Primary Caregiver
      Helped with Banking"
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
    title_fragment = "of Respondents and the reasons why they did not receive help"
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
    title_fragment = "of Respondents who have a Disability indicator"
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
    title_fragment = "of Respondents who have a type of Caree"
  )
)

giver_ui_config <- list(
  "Activities Respondent Assists Caree With" = list(
    index = 1,
    count_chart = chart,
    pct_chart = chart_pct,
    input_vector = help_activities,
    code = help_activity_codes,
    y = y_activity_give_help,
    caption = "Count for the type of activities for which respondents considered to be a care giver assisted with in the past 12 months.",
    title = "Types of activities respondents provided help with - Past 12 months",
    x_axis = "Activity",
    y_axis = "Count",
    caption_pct = "Proportion of caregiver respondents who report providing the help to caree in the past 12 months with each type of activity.",
    y_axis_pct = "Proportion",
    table = tab_multi_var_maker(df_giver, help_activities, help_activity_codes, y_activity_give_help),
    title_fragment = "of People who Provided Help with an Activity"
  ),
  "Age of Caree" = list(
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
    title_fragment = "of people and the Age of Respondent's Primary Care
        Receiver"
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
    title_fragment = "of People and the Number of Hours of Help Provided -
        Per Average Week"
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
    title_fragment = "of People and the Distance Between them and the Care
        Receiver's Dwellings"
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
    title_fragment = "of People and the Frequency they Provided Help to Their
        Primary Care Receiver with Banking"
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
    title_fragment = "of People and Number of Hours they Provided Help with
        Banking"
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
    title_fragment = "of People who had out-of-pocket Expenses From Caregiving
        - Past 12 months"
  ),
  "Financial Hardship due to Caregiving" = list(
    index = 12,
    count_chart = chart_financial_hardship,
    pct_chart = chart_financial_hardship_percent,
    table = tab_financial_hardship,
    title_fragment = "who Experienced Financial Hardship Because of Caregiving
        Responsibilities"
  ),
  "Respondent has a Diability Indicator" = list(
    index = 13,
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
    title_fragment = "of Respondents who have a Disability indicator"
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
                  plotOutput("general_selected_chart")
                ),
                tabPanel(
                  "Percentages",
                  plotOutput("general_percentage")
                ),
                tabPanel(
                  "Tables",
                  tableOutput("general_table")
                ) # table id
                # tabPanel(
                #   "Statistical Significance",
                #   "Statisical significance of data will be displayed here"
                # )
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
                "receiver_select_box_partner_in_household",
                "Filter by spouse/partner living with older adult:",
                choices = filter_partner_in_household,
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
                  plotOutput("receiver_selected_chart"),
                  uiOutput("filters_applied"),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetReceiverCount", "Reset"))
                  )
                ),
                tabPanel(
                  "Percentages", 
                  plotOutput("receiver_percentage"),
                  hr(),
                  
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetReceiverPercentage", "Reset"))
                    )
                  ),
                tabPanel(
                  "Tables",
                  tableOutput("receiver_table")
                ) # receiver table id
                # tabPanel(
                #   "Statistical Significance",
                #   "Statisical significance of data will be displayed here"
                # )
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
                "Filter by age of caree",
                filter_age_group,
                selected = default
              ),
              selectInput(
                "giver_select_box_pop_centre",
                "Filter by urban/rural status of caree",
                filter_pop_centre,
                selected = default
              ),
              selectInput(
                "giver_select_box_partner_in_household",
                "Filter by Spouse/Partner living with caree",
                filter_partner_in_household,
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
                  plotOutput("giver_selected_chart"),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetGiverCount", "Reset"))
                  )
                ),
                tabPanel(
                  "Percentages",
                  plotOutput("giver_percentage"),
                  hr(),
                  fluidRow(
                    column(4, p("Reset all filters to default settings?")),
                    column(2, actionButton("resetGiverPercentage", "Reset"))
                  )
                ), # giver percentages
                tabPanel(
                  "Tables",
                  tableOutput("giver_table")
                ) # giver table
                # tabPanel(
                #   "Statistical Significance",
                #   "Statisical significance of data will be displayed here"
                # )
              )
            )
          )
        )
      )
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
      chart_general(pop_name, pop_freq, "GSS 2018 - Respondent groups", "Count of respondents in each grouping: caregivers, care receivers, and persons with unmet caregiving needs.","Respondent groups", "Count")
    } else if(input$general_selected_box == general_charts[2]) {
      c_primary_sex
    } else if(input$general_selected_box == general_charts[3]){
      chart_general(caree_relationship, caree_freq, "GSS 2018 - Relationship between Caree and Receiver", "Count of respondents in each grouping: Spouse/Partner, Son, Daughter, Parent, Other Family Members, Other.","Caree Relationships", "Count")
    } else if (input$general_selected_box == general_charts[4]){
      chart_general(disability_counter, disability_freq, "GSS 2018 - Number of Disability Types - Grouped", "Count of respondents in each grouping: None, 1, 2 or 3, >3.","Groups of Disability Counts (None, 1, 2 or 3, >3.", "Counts")
    }
  })
  
  # general percentage
  output$general_percentage <- renderPlot({
    if (input$general_selected_box == general_charts[1]) {
      chart_general_pct(pop_name, pop_freq, "GSS 2018 - Respondent groups", "Proportion of respondents in each grouping: caregivers, care receivers, and persons with unmet caregiving needs", "Respondent groups", "Proportion")
    } else if(input$general_selected_box == general_charts[2]){
      # TODO: create primary sex percent chart
    } else if(input$general_selected_box == general_charts[3]){
      chart_general_pct(caree_relationship, caree_freq, "GSS 2018 - Relationship between Caree and Receiver", "Proportion of respondents in each grouping: Spuse/Partner, Son, Daughter, Parent, Other Family Members, Others.", "Caree Relationships", "Proportion")
    } else if(input$general_selected_box == general_charts[4]){
      chart_general_pct(disability_counter, disability_freq, "GSS 2018 - Number of Disability Types - Grouped", "Proportion of respondents in each grouping: None, 1, 2 or 3, >3.", "Groups of Disability Counts(None, 1, 2 or 3, >3", "Proportion")
    }
  })
  
  # general table
  output$general_table <- renderTable({
    if (input$general_selected_box == general_charts[1]) {
      tab_general(pop_name, pop_freq)
    } else if(input$general_selected_box == general_charts[2]){
      df_primary_sex %>% rename("Sex" = sex, "Count" = freq)
    } else if(input$general_selected_box == general_charts[3]){
      tab_general(caree_relationship, caree_freq)
    } else if(input$general_selected_box == general_charts[4]){
      tab_general(disability_counter, disability_freq)
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
      strtoi(input$receiver_select_box_partner_in_household), "PHSDFLG"
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
    
    config <- receiver_ui_config[[input$receiver_select_box]]
    final_table <- config$table  
    final_table <- final_table %>%
      rename(!!input$receiver_select_box := 1, "count" := 2)
    
    final_table
  })
  
  observeEvent(input$resetReceiverCount, {
    reset("receiver_select_box_sex")
    reset("receiver_select_box_age")
    reset("receiver_select_box_pop_centre")
    reset("receiver_select_box_partner_in_household")
    reset("receiver_select_box_living_arrangement_senior_household")
    reset("receiver_select_box_indigenous_status")
    reset("receiver_select_box_visible_minority")
    reset("receiver_select_box_group_religious_participation")
    update_receiver_df()
    update_giver_df()
  })
  
  observeEvent(input$resetReceiverPercentage, {
    reset("receiver_select_box_sex")
    reset("receiver_select_box_age")
    reset("receiver_select_box_pop_centre")
    reset("receiver_select_box_partner_in_household")
    reset("receiver_select_box_living_arrangement_senior_household")
    reset("receiver_select_box_indigenous_status")
    reset("receiver_select_box_visible_minority")
    reset("receiver_select_box_group_religious_participation")
    update_receiver_df()
    update_giver_df()
  })
  
  
  ### Giver filters and charts
  update_giver_df <- reactive({
    # filter by sex
    df_filtered <- apply_filter(
      df_giver,
      strtoi(input$giver_select_box_sex), "SEX"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_age), "AGEGR10"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_pop_centre), "LUC_RST"
    )
    df_filtered <- apply_filter(
      df_filtered,
      strtoi(input$giver_select_box_partner_in_household), "PHSDFLG"
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
    
    config <- giver_ui_config[[input$giver_select_box]]
    final_table <- config$table  # This only works for charts that run with tab_maker
    #final_table <- config$table(output_giver_df) # This only works for charts that run withour tab_maker
    
    final_table <- final_table %>%
      rename(!!input$giver_select_box := 1, "count" := 2)
    
    final_table
  })
  
  observeEvent(input$resetGiverCount, {
    reset("giver_select_box_sex")
    reset("giver_select_box_age")
    reset("giver_select_box_pop_centre")
    reset("giver_select_box_partner_in_household")
    reset("giver_select_box_living_arrangement_senior_household")
    reset("giver_select_box_indigenous_status")
    reset("giver_select_box_visible_minority")
    reset("giver_select_box_religious_participation")
    reset("giver_select_box_receiver_main_health_condition")
    update_receiver_df()
    update_giver_df()
  })
  
  observeEvent(input$resetGiverPercentage, {
    reset("giver_select_box_sex")
    reset("giver_select_box_age")
    reset("giver_select_box_pop_centre")
    reset("giver_select_box_partner_in_household")
    reset("giver_select_box_living_arrangement_senior_household")
    reset("giver_select_box_indigenous_status")
    reset("giver_select_box_visible_minority")
    reset("giver_select_box_religious_participation")
    reset("giver_select_box_receiver_main_health_condition")
    update_receiver_df()
    update_giver_df()
  })
  
  output$filters_applied <- renderUI({
    applied_filters <- list(
      c(paste("Sex: ", 
              names(filter_sex)[which(filter_sex == input$receiver_select_box_sex)])),
      c(paste("Age group: ", 
              names(filter_age_group[which(filter_age_group == input$receiver_select_box_age)]))),
      c(paste("Urban/Rural status: ", 
              names(filter_pop_centre[which(filter_pop_centre == input$receiver_select_box_pop_centre)]))),
      c(paste("Partner in household: ", 
              names(filter_partner_in_household[which(filter_partner_in_household == input$receiver_select_box_partner_in_household)]))),
      c(paste("Living Arrangement: ", 
              names(filter_living_arrangement_senior_household[which(filter_living_arrangement_senior_household == input$receiver_select_box_living_arrangement_senior_household)]))),
      c(paste("Indigenous Status: ", 
              names(filter_indigenous_status[which(filter_indigenous_status == input$receiver_select_box_indigenous_status)]))),
      c(paste("Visible Minority Status: ",
              names(filter_visible_minority_status[which(filter_visible_minority_status == input$receiver_select_box_visible_minority)]))),
      c(paste("Religious Participation: ",
              names(filter_group_religious_participation[which(filter_group_religious_participation == input$receiver_select_box_group_religious_participation)])))
    )
    
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
addResourcePath("img", "www/images")

shinyApp(ui = ui, server = server, options = options)
