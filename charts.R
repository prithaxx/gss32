# The main Shiny application.
#
# Defines the interface for the application, and ties the UI elements to the
# various charts that are defined across the other modules.

library(shiny)
library(shinyjs)
source("global.R")

general_charts <- list(
  "Respondent Groups",
  "Sex of Primary Caregivers and Care Receivers"
)

receiver_options <- list(
  "Health Conditions Experienced" = 1,
  "Activities Respondent Gets Help With" = 2,
  "Age of Respondent's Primary Caregiver" = 3,
  "Activities Assisted by Professionals" = 4,
  "Hours of Help Received per Week" = 5,
  "Primary Caregiver Distance Away" = 6,
  "Banking Help Received - frequency" = 7,
  "Banking Help Received - hours" = 8,
  "Help banking hours - daily" = 9,
  "Help banking hours - at least once a week" = 10,
  "Help banking hours - at least once a month" = 11,
  "Help banking hours - less than once a month" = 12
)

receiver_response_charts <- list(
  "Health Conditions Experienced" = chart_health_conditions,
  "Activities Respondent Gets Help With" = chart_activity_receive_help,
  "Age of Respondent's Primary Caregiver" = chart_age_primary_giver,
  "Activities Assisted by Professionals" = chart_activity_receive_help_pro,
  "Hours of Help Received per Week" = chart_hours_help_received,
  "Primary Caregiver Distance Away" = chart_primary_giver_distance,
  "Banking Help Received - frequency" = chart_receive_help_banking_freq,
  "Banking Help Received - hours" = chart_receive_help_banking_hours,
  "Help banking hours - daily" = chart_help_banking_hours_daily,
  "Help banking hours - at least once a week" = chart_help_banking_weekly,
  "Help banking hours - at least once a month" = chart_help_banking_monthly,
  "Help banking hours - less than once a month" =
    chart_help_banking_monthly_less
)

receiver_charts_percent <- list(
  "Health Conditions Experienced" = chart_health_conditions_percent,
  "Activities Respondent Gets Help With" = chart_activity_receive_help_percent,
  "Age of Respondent's Primary Caregiver" = chart_age_primary_giver_percent,
  "Activities Assisted by Professionals" =
    chart_activity_receive_help_pro_percent,
  "Hours of Help Received per Week" = chart_hours_help_received_percent,
  "Primary Caregiver Distance Away" = chart_primary_giver_distance_percent,
  "Banking Help Received - frequency" = chart_receive_help_banking_freq_percent,
  "Banking Help Received - hours" = chart_receive_help_banking_hours_percent,
  "Help banking hours - daily" = chart_help_banking_hours_daily_percent,
  "Help banking hours - at least once a week" =
    chart_help_banking_weekly_percent,
  "Help banking hours - at least once a month" =
    chart_help_banking_monthly_percent,
  "Help banking hours - less than once a month" =
    chart_help_banking_monthly_less_percent
)

receiver_response_tabs <- list(
  "Health Conditions Experienced" = tab_health_conditions,
  "Activities Respondent Gets Help With" = tab_activity_receive_help,
  "Age of Respondent's Primary Caregiver" = tab_age_primary_giver,
  "Activities Assisted by Professionals" = tab_activity_receive_help_pro,
  "Hours of Help Received per Week" = tab_hours_help_received,
  "Primary Caregiver Distance Away" = tab_primary_giver_distance,
  "Banking Help Received - frequency" = tab_receive_help_banking_freq,
  "Banking Help Received - hours" = tab_receive_help_banking_hours,
  "Help banking hours - daily" = tab_help_banking_hours_daily,
  "Help banking hours - at least once a week" = tab_help_banking_hours_weekly,
  "Help banking hours - at least once a month" =
    tab_help_banking_hours_monthly,
  "Help banking hours - less than once a month" =
    tab_help_banking_hours_monthly_less
)

giver_options <- list(
  "Activities Respondent Assists Caree With" = 1,
  "Age of Caree" = 2,
  "Hours of Help Respondent Provides to Caree" = 3,
  "Distance to Caree" = 4,
  "Banking Help Provided to Caree - frequency" = 5,
  "Banking Help Provided to Caree - hours" = 6,
  "Give help banking - daily" = 7,
  "Give help banking - at least once a week" = 8,
  "Give help banking - at least once a month" = 9,
  "Give help banking - less than once a month" = 10,
  "Out of Pocket Caregiving Expenses" = 11,
  "Financial Hardship due to Caregiving" = 12
)

giver_response_charts <- list(
  "Activities Respondent Assists Caree With" = chart_activity_give_help,
  "Age of Caree" = chart_age_primary_receiver,
  "Hours of Help Respondent Provides to Caree" = chart_hours_help_provided,
  "Distance to Caree" = chart_primary_receiver_distance,
  "Banking Help Provided to Caree - frequency" = chart_give_help_banking_freq,
  "Banking Help Provided to Caree - hours" = chart_give_help_banking_hours,
  "Give help banking - daily" = chart_give_help_banking_daily,
  "Give help banking - at least once a week" = chart_give_help_banking_weekly,
  "Give help banking - at least once a month" =
    chart_give_help_banking_monthly,
  "Give help banking - less than once a month" =
    chart_give_help_banking_monthly_less,
  "Out of Pocket Caregiving Expenses" = chart_out_of_pocket,
  "Financial Hardship due to Caregiving" = chart_financial_hardship
)

giver_response_percent <- list(
  "Activities Respondent Assists Caree With" = chart_activity_give_help_percent,
  "Age of Caree" = chart_age_primary_receiver_percent,
  "Hours of Help Respondent Provides to Caree" =
    chart_hours_help_provided_percent,
  "Distance to Caree" = chart_primary_receiver_distance_percent,
  "Banking Help Provided to Caree - frequency" =
    chart_give_help_banking_freq_percent,
  "Banking Help Provided to Caree - hours" =
    chart_give_help_banking_hours_percent,
  "Give help banking - daily" = chart_give_help_banking_daily_percent,
  "Give help banking - at least once a week" =
    chart_give_help_banking_weekly_percent,
  "Give help banking - at least once a month" =
    chart_give_help_banking_monthly_percent,
  "Give help banking - less than once a month" =
    chart_give_help_banking_monthly_less_percent,
  "Out of Pocket Caregiving Expenses" = chart_out_of_pocket_percent,
  "Financial Hardship due to Caregiving" = chart_financial_hardship_percent
)

giver_response_tabs <- list(
  "Activities Respondent Assists Caree With" = tab_activity_give_help,
  "Age of Caree" = tab_age_primary_receiver,
  "Hours of Help Respondent Provides to Caree" = tab_hours_help_provided,
  "Distance to Caree" = tab_primary_receiver_distance,
  "Banking Help Provided to Caree - frequency" = tab_give_help_banking_freq,
  "Banking Help Provided to Caree - hours" = tab_give_help_banking_hours,
  "Give help banking - daily" = tab_give_help_banking_daily,
  "Give help banking - at least once a week" = tab_give_help_banking_weekly,
  "Give help banking - at least once a month" = tab_give_help_banking_monthly,
  "Give help banking - less than once a month" =
    tab_give_help_banking_monthly_less,
  "Out of Pocket Caregiving Expenses" = tab_out_of_pocket,
  "Financial Hardship due to Caregiving" = tab_financial_hardship
)

group_by_options <- list(
  "None" = 1,
  "Sex" = 2,
  "Age group" = 3,
  "Living arrangement" = 4,
  "Visible minority status" = 5
)
show_group <- list("false", "true")
default <- 0

ui <- function(request) {
  request
  fluidPage(
    useShinyjs(),
    titlePanel("Explore the 2018 General Social Survey on Caregiving and Care
      Receiving"),
    fluidRow(
      sidebarLayout(
        mainPanel(
          p("This data represents a subset of information from the survey. The
          information here is data from respondents who are:"),
          tags$ol(
            tags$li("Care receivers who are 65 years old or older, or"),
            tags$li("Caregivers who provide assistance to individuals who are 65
            years old or older, or"),
            tags$li("Both care receivers who are 65 years old or older while
            simultaneously acting as caregivers to other care receivers who are
            65 years old or older"),
            tags$li("People 65 or older who need help but are not currently
            receiving care.")
          ),
          p("These groups of respondents are likely providing insights into the
          experiences and challenges related to receiving or providing care for
          older adults. The data may include information about their health,
          financial situation, quality of life, and other factors relevant to
          caregiving and care receiving."),
          width = 10
        ),
        sidebarPanel(
          bookmarkButton(
            label = "Bookmark...",
            icon = shiny::icon("link", lib = "glyphicon"),
            title = "Bookmark this application's state and get a URL for
              sharing.",
            id = "._bookmark_",
          ),
          width = 2
        )
      )
    ),
    tabsetPanel(
      id = "main_panel",
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
              ),
            ),
            mainPanel(
              tabsetPanel(
                type = "pills",
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
                ), # table id
                tabPanel(
                  "Statistical Significance",
                  "Statisical significance of data will be displayed here"
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
                choices = names(receiver_response_charts),
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
                choices = group_by_options,
                selected = 1
              ),
              selectInput("radio_select_box",
                "radio select box",
                choices = list("hello" = 1, "world" = 2),
                selected = 1
              )
            ),
            mainPanel(
              tabsetPanel(
                type = "pills",
                tabPanel(
                  "Counts",
                  plotOutput("receiver_selected_chart")
                ),
                tabPanel("Percentages", plotOutput("receiver_percentage")),
                tabPanel(
                  "Tables",
                  tableOutput("receiver_table")
                ), # receiver table id
                tabPanel(
                  "Statistical Significance",
                  "Statisical significance of data will be displayed here"
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
                choices = names(giver_response_charts),
                selected = names(giver_response_charts[1])
              ),
              selectInput(
                "giver_select_box_sex",
                "Filter by sex",
                choices = filter_sex,
                selected = default
              ),
              selectInput(
                "giver_select_box_age",
                "Age group",
                filter_age_group,
                selected = default
              ),
              selectInput(
                "giver_select_box_pop_centre",
                "Population Centre",
                filter_pop_centre,
                selected = default
              ),
              selectInput(
                "giver_select_box_partner_in_household",
                "Spouse/Partner living in household",
                filter_partner_in_household,
                selected = default
              ),
              selectInput(
                "giver_select_box_living_arrangement_senior_household",
                "Living arrangement of senior respondent's household",
                filter_living_arrangement_senior_household,
                selected = default
              ),
              selectInput(
                "giver_select_box_indigenous_status",
                "Indigenous status",
                filter_indigenous_status,
                selected = default
              ),
              selectInput(
                "giver_select_box_visible_minority",
                "Visible minority status",
                filter_visible_minority_status,
                selected = default
              ),
              selectInput(
                "giver_select_box_group_religious_participation",
                "Group religious participation",
                filter_group_religious_participation,
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
                type = "pills",
                tabPanel(
                  "Counts",
                  plotOutput("giver_selected_chart")
                ),
                tabPanel(
                  "Percentages",
                  plotOutput("giver_percentage")
                ), # giver percentages
                tabPanel(
                  "Tables",
                  tableOutput("giver_table")
                ), # giver table
                tabPanel(
                  "Statistical Significance",
                  "Statisical significance of data will be displayed here"
                )
              )
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
  }, )

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    l <- reactiveValuesToList(input)
    "trigger"
    session$doBookmark()
  })
  onBookmark(function(state) {
    state.values
  })
  onBookmarked(function(url) {
    # Can instead use `updateQueryString(url)` if we want to just update the
    # window's location. Refreshing would take to the last bookmarked page.
    updateQueryString(url)
    showBookmarkUrlModal(url)
  })

  output_receiver_df <- df_receiver
  df_output_giver <- df_giver

  # general counts tab
  output$general_selected_chart <- renderPlot({
    if (input$general_selected_box == general_charts[1]) {
      c_respondent_groups
    } else {
      c_primary_sex
    }
  })

  # general percentage
  output$general_percentage <- renderPlot({
    if (input$general_selected_box == general_charts[1]) {
      chart_respondent_groups_percent()
    } else {
      # TODO: create primary sex percent chart
    }
  })

  # general table
  output$general_table <- renderTable({
    if (input$general_selected_box == general_charts[1]) {
      tab_pop_freq()
    } else {
      df_primary_sex %>% rename("Sex" = sex, "Count" = freq)
    }
  })

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
    index <- receiver_options[[input$receiver_select_box]]

    chart <- receiver_response_charts[[index]]
    tab <- receiver_response_tabs[[index]]
    x_lab <- names(receiver_options)[[index]]
    title_lab <- receiver_group_by_titles[[index]]

    update_receiver_df()

    if (input$receiver_radio == 2) {
      group_by_sex(output_receiver_df, tab, x_lab, title_lab)
    } else if (input$receiver_radio == 3) {
      group_by_age(output_receiver_df, tab, x_lab, title_lab)
    } else {
      chart(output_receiver_df)
    }
  })

  # receiver percentage tab
  output$receiver_percentage <- renderPlot({
    index <- receiver_options[[input$receiver_select_box]]

    chart <- receiver_charts_percent[[index]]
    tab <- receiver_response_tabs[[index]]
    x_lab <- names(receiver_options)[[index]]
    title_lab <- receiver_group_by_titles[[index]]

    update_receiver_df()

    if (input$receiver_radio == 2) {
      group_by_sex_percent(output_receiver_df, tab, x_lab, title_lab)
    } else if (input$receiver_radio == 3) {
      group_by_age_percent(output_receiver_df, tab, x_lab, title_lab)
    } else {
      chart(output_receiver_df)
    }
  })

  # receiver table tab
  output$receiver_table <- renderTable({
    update_receiver_df()
    tab <- receiver_response_tabs[[input$receiver_select_box]]
    final_table <- tab(output_receiver_df)

    for (i in seq_along(receiver_response_charts)) {
      if (input$receiver_select_box == names(receiver_response_charts[i])) {
        final_table <- final_table %>%
          rename(!!names(receiver_response_charts[i]) := 1, "count" := 2)
      }
    }

    final_table
  })

  ###

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

    df_output_giver <<- df_filtered
  })

  # giver counts tab
  output$giver_selected_chart <- renderPlot({
    index <- giver_options[[input$giver_select_box]]

    chart <- giver_response_charts[[index]]
    tab <- giver_response_tabs[[index]]
    x_lab <- names(giver_options)[[index]]
    title_lab <- giver_group_by_titles[[index]]

    update_giver_df()

    if (input$giver_radio == 2) {
      group_by_sex(df_output_giver, tab, x_lab, title_lab)
    } else if (input$giver_radio == 3) {
      group_by_age(df_output_giver, tab, x_lab, title_lab)
    } else {
      chart(df_output_giver)
    }
  })

  # giver percentage tab
  output$giver_percentage <- renderPlot({
    index <- giver_options[[input$giver_select_box]]

    chart <- giver_response_percent[[index]]
    tab <- giver_response_tabs[[index]]
    x_lab <- names(giver_options)[[index]]
    title_lab <- giver_group_by_titles[[index]]

    update_giver_df()

    if (input$giver_radio == 2) {
      group_by_sex_percent(df_output_giver, tab, x_lab, title_lab)
    } else if (input$giver_radio == 3) {
      group_by_age_percent(df_output_giver, tab, x_lab, title_lab)
    } else {
      chart(df_output_giver)
    }
  })

  # giver table tab
  output$giver_table <- renderTable({
    update_giver_df()
    tab <- giver_response_tabs[[input$giver_select_box]]
    final_table <- tab(df_output_giver)

    for (i in seq_along(giver_response_charts)) {
      if (input$giver_select_box == names(giver_response_charts[i])) {
        final_table <- final_table %>%
          rename(
            !!names(giver_response_charts[i]) := 1,
            "count" := 2, "percentage" := 3
          )
      }
    }

    return(final_table)
  })
}

options <- list(
  port = as.integer(Sys.getenv("GSS32_PORT")),
  host = Sys.getenv("GSS32_HOST")
)
if (grepl("TRUE", Sys.getenv("GSS32_DEVMODE"))) {
  options <- c(options, list(
    shiny.autoreload = TRUE,
    shiny.autoreload.pattern = glob2rx("*.R")
  ))
  devmode(TRUE)
}
enableBookmarking("url")

shinyApp(ui = ui, server = server, options = options)
