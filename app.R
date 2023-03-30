library(shiny)
library(shinyjs)
source("global.R")

general_charts <- list(
  "Respondent groups",
  "Sex of primary caregivers and care receivers"
)

receiver_response_charts <- list(
  "Health conditions" = chart_health_conditions,
  "Activity receive help" = chart_activity_receive_help,
  "Age of primary giver" = chart_age_primary_giver,
  "Activity receive help from professional" = chart_activity_receive_help_pro,
  "Hours of help received" = chart_hours_help_received,
  "Primary giver distance" = chart_primary_giver_distance,
  "Receive help banking - frequency" = chart_receive_help_banking_freq,
  "Receive help banking - hours" = chart_receive_help_banking_hours,
  "Help banking hours - daily" = chart_help_banking_hours_daily,
  "Help banking hours - at least once a week" = chart_help_banking_weekly,
  "Help banking hours - at least once a month" = chart_help_banking_monthly,
  "Help banking hours - less than once a month" = chart_help_banking_monthly_less
)
receiver_charts_percent <- list(
  "Health conditions" = chart_health_conditions_percent,
  "Activity receive help" = chart_activity_receive_help_percent,
  "Age of primary giver" = chart_age_primary_giver_percent,
  "Activity receive help from professional" = chart_activity_receive_help_pro_percent,
  "Hours of help received" = chart_hours_help_received_percent,
  "Primary giver distance" = chart_primary_giver_distance_percent,
  "Receive help banking - frequency" = chart_receive_help_banking_freq_percent,
  "Receive help banking - hours" = chart_receive_help_banking_hours_percent,
  "Help banking hours - daily" = chart_help_banking_hours_daily_percent,
  "Help banking hours - at least once a week" = chart_help_banking_weekly_percent,
  "Help banking hours - at least once a month" = chart_help_banking_monthly_percent,
  "Help banking hours - less than once a month" = chart_help_banking_monthly_less_percent
)
receiver_response_tabs <- list(
  "Health conditions" = tab_health_conditions,
  "Activity receive help" = tab_activity_receive_help,
  "Age of primary giver" = tab_age_primary_giver,
  "Activity receive help from professional" = tab_activity_receive_help_pro,
  "Hours of help received" = tab_hours_help_received,
  "Primary giver distance" = tab_primary_giver_distance,
  "Receive help banking - frequency" = tab_receive_help_banking_freq,
  "Receive help banking - hours" = tab_receive_help_banking_hours,
  "Help banking hours - daily" = tab_help_banking_hours_daily,
  "Help banking hours - at least once a week" = tab_help_banking_hours_weekly,
  "Help banking hours - at least once a month" = tab_help_banking_hours_monthly,
  "Help banking hours - less than once a month" = tab_help_banking_hours_monthly_less
)

giver_response_charts <- list(
  "Activity give help" = chart_activity_give_help,
  "Age of primary receiver" = chart_age_primary_receiver,
  "Hours of help provided" = chart_hours_help_provided,
  "Primary receiver distance" = chart_primary_receiver_distance,
  "Give help banking - frequency" = chart_give_help_banking_freq,
  "Give help banking - hours" = chart_give_help_banking_hours,
  "Give help banking - daily" = chart_give_help_banking_daily,
  "Give help banking - at least once a week" = chart_give_help_banking_weekly,
  "Give help banking - at least once a month" = chart_give_help_banking_monthly,
  "Give help banking - less than once a month" = chart_give_help_banking_monthly_less,
  "Out of pocket expenses" = chart_out_of_pocket,
  "Financial hardship" = chart_financial_hardship
)

giver_response_percent <- list(
  "Activity give help" = chart_activity_give_help_percent,
  "Age of primary receiver" = chart_age_primary_receiver_percent,
  "Hours of help provided" = chart_hours_help_provided_percent,
  "Primary receiver distance" = chart_primary_receiver_distance_percent,
  "Give help banking - frequency" = chart_give_help_banking_freq_percent,
  "Give help banking - hours" = chart_give_help_banking_hours_percent,
  "Give help banking - daily" = chart_give_help_banking_daily_percent,
  "Give help banking - at least once a week" = chart_give_help_banking_weekly_percent,
  "Give help banking - at least once a month" = chart_give_help_banking_monthly_percent,
  "Give help banking - less than once a month" = chart_give_help_banking_monthly_less_percent,
  "Out of pocket expenses" = chart_out_of_pocket_percent,
  "Financial hardship" = chart_financial_hardship_percent
)

giver_response_tabs <- list(
  "Activity give help" = tab_activity_give_help,
  "Age of primary receiver" = tab_age_primary_receiver,
  "Hours of help provided" = tab_hours_help_provided,
  "Primary receiver distance" = tab_primary_receiver_distance,
  "Give help banking - frequency" = tab_give_help_banking_freq,
  "Give help banking - hours" = tab_give_help_banking_hours,
  "Give help banking - daily" = tab_give_help_banking_daily,
  "Give help banking - at least once a week" = tab_give_help_banking_weekly,
  "Give help banking - at least once a month" = tab_give_help_banking_monthly,
  "Give help banking - less than once a month" = tab_give_help_banking_monthly_less,
  "Out of pocket expenses" = tab_out_of_pocket,
  "Financial hardship" = tab_financial_hardship
)

group_by_options <- list("None" = 1, "Sex" = 2, "Age group" = 3, "Living arrangement" = 4, "Visible minority status" = 5)
show_group <- list("false", "true")
default <- 0

# apply_filter(): takes a frame and filter based on option selected
# df_input (tibble): data frame to be transformed
# select_option (integer): filter value mapped to the response category
# col_name (String): variable to filter by
apply_filter <- function(df_input, select_option, col_name) {
  filtered_df <- if (select_option != -1) {
    filtered_df <- df_input %>% filter(!!as.symbol(col_name) == select_option) # the value from the list: e.g. both sexes = -1, male = 1, female = 2
  } else {
    df_input
  }

  return(filtered_df)
}

ui <- fluidPage(
  useShinyjs(),
  actionButton("button", "Click me"),
  textInput("text", "Text"),
  
  titlePanel("Explore the 2018 General Social Survey on Caregiving and Care Receiving"),
  div(
    p("The data represents respondents who are:"),
    tags$ol(
      tags$li("care receivers who are 65 years old or older"),
      tags$li("caregivers to individuals who are 65 years old or older"),
      tags$li("Both care receivers who are 65 years old or older and caregivers to care receivers who are 65 years old or older"),
      tags$li("Needed help and are not receiving care but would like to and are 65 years old or older.")
    ),
    p("These groups of respondents are likely providing insights into the experiences and challenges
      related to receiving or providing care for elderly individuals. The data may include information about their health,
      financial situation, quality of life, and other factors relevant to caregiving and care receiving.")
  ),
  # general charts
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("general_selected_box", "General info:", choices = general_charts, selected = default),
      ),
      mainPanel(
        tabsetPanel(
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
          tabPanel("Statistical Significance", "Statisical significance of data will be displayed here")
        )
      )
    )
  ),
  # receiver response charts
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("receiver_select_box", "Questions asked to older adults who received care:", choices = names(receiver_response_charts), default),
        selectInput("receiver_select_box_sex", "Filter by sex", choices = filter_sex, default),
        selectInput("receiver_select_box_age", "Age group", filter_age_group, default),
        selectInput("receiver_select_box_pop_centre", "Population Centre", filter_pop_centre, default),
        selectInput("receiver_select_box_partner_in_household", "Spouse/Partner living in household", filter_partner_in_household, default),
        selectInput("receiver_select_box_living_arrangement_senior_household", "Living arrangement of senior respondent's household", filter_living_arrangement_senior_household, default),
        selectInput("receiver_select_box_indigenous_status", "Indigenous status", filter_indigenous_status, selected = default),
        selectInput("receiver_select_box_visible_minority", "Visible minority status", filter_visible_minority_status, selected = default),
        selectInput("receiver_select_box_group_religious_participation", "Group religious participation", filter_group_religious_participation, selected = default),
        radioButtons("receiver_radio", "Group by:", choices = group_by_options, selected = 1),
        selectInput("radio_select_box", "radio select box", list("hello" = 1, "world" = 2), selected = 1)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Counts",
            plotOutput("receiver_selected_chart")
          ),
          tabPanel("Percentages", plotOutput("receiver_percentage")),
          tabPanel(
            "Tables",
            tableOutput("receiver_table")
          ), # receiver table id
          tabPanel("Statistical Significance", "Statisical significance of data will be displayed here")
        )
      )
    )
  ),
  # giver response charts
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("giver_select_box", "Questions asked to respondents who provided care to older adults:", choices = names(giver_response_charts), selected = names(giver_response_charts[1])),
        selectInput("giver_select_box_sex", "Filter by sex", choices = filter_sex, selected = default),
        selectInput("giver_select_box_age", "Age group", filter_age_group, selected = default),
        selectInput("giver_select_box_pop_centre", "Population Centre", filter_pop_centre, selected = default),
        selectInput("giver_select_box_partner_in_household", "Spouse/Partner living in household", filter_partner_in_household, selected = default),
        selectInput("giver_select_box_living_arrangement_senior_household", "Living arrangement of senior respondent's household", filter_living_arrangement_senior_household, selected = default),
        selectInput("giver_select_box_indigenous_status", "Indigenous status", filter_indigenous_status, selected = default),
        selectInput("giver_select_box_visible_minority", "Visible minority status", filter_visible_minority_status, selected = default),
        selectInput("giver_select_box_group_religious_participation", "Group religious participation", filter_group_religious_participation, selected = default)
      ),
      mainPanel(
        tabsetPanel(
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
          tabPanel("Statistical Significance", "Statisical significance of data will be displayed here")
        )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$button, {
    toggle("text")  # toggle is a shinyjs function
  })
  
  observeEvent(input$receiver_radio, {
    if (input$receiver_radio != 1) {
      disable("radio_select_box")
    } else {
      enable("radio_select_box")
    }
  },)
  
  output_receiver_df <- df_receiver
  df_output_giver <- df_giver

  # general counts tab
  output$general_selected_chart <- renderPlot({
    # chart_function <- general_charts[[input$general_selected_box]]
    # general_charts[[input$general_selected_box]]
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
      # print("else")
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
    # filter by sex
    # print(input$receiver_select_box_sex )
    # print(names(filter_sex[2]))
    # print(filter_sex[2])

    filtered_df <- apply_filter(df_receiver, strtoi(input$receiver_select_box_sex), "SEX")
    filtered_df <- apply_filter(filtered_df, strtoi(input$receiver_select_box_age), "AGEGR10")
    filtered_df <- apply_filter(filtered_df, strtoi(input$receiver_select_box_pop_centre), "LUC_RST")
    filtered_df <- apply_filter(filtered_df, strtoi(input$receiver_select_box_partner_in_household), "PHSDFLG")
    filtered_df <- apply_filter(filtered_df, strtoi(input$receiver_select_box_living_arrangement_senior_household), "LIVARRSN")
    filtered_df <- apply_filter(filtered_df, strtoi(input$receiver_select_box_indigenous_status), "AMB_01_1")
    filtered_df <- apply_filter(filtered_df, strtoi(input$receiver_select_box_visible_minority), "VISMIN")
    filtered_df <- apply_filter(filtered_df, strtoi(input$receiver_select_box_group_religious_participation), "REE_02")

    output_receiver_df <<- filtered_df
  })

  # receiver counts tab
  output$receiver_selected_chart <- renderPlot({
    chart <- receiver_response_charts[[input$receiver_select_box]]
    update_receiver_df()
    
    if (input$receiver_radio == 2) {
      group_by_sex(output_receiver_df)
    } else {
      chart(output_receiver_df)
    }
    # chart(output_receiver_df)
  })

  # receiver percentage tab
  output$receiver_percentage <- renderPlot({
    chart <- receiver_charts_percent[[input$receiver_select_box]]
    update_receiver_df()

    if (input$receiver_radio == 2) {
      group_by_sex(output_receiver_df)
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
        final_table <- final_table %>% rename(!!names(receiver_response_charts[i]) := 1, "Count" := 2)
      }
    }

    final_table
  })

  ###

  update_giver_df <- reactive({
    # filter by sex
    df_filtered <- apply_filter(df_giver, strtoi(input$giver_select_box_sex), "SEX")
    df_filtered <- apply_filter(df_filtered, strtoi(input$giver_select_box_age), "AGEGR10")
    df_filtered <- apply_filter(df_filtered, strtoi(input$giver_select_box_pop_centre), "LUC_RST")
    df_filtered <- apply_filter(df_filtered, strtoi(input$giver_select_box_partner_in_household), "PHSDFLG")
    df_filtered <- apply_filter(df_filtered, strtoi(input$giver_select_box_living_arrangement_senior_household), "LIVARRSN")
    df_filtered <- apply_filter(df_filtered, strtoi(input$giver_select_box_indigenous_status), "AMB_01_1")
    df_filtered <- apply_filter(df_filtered, strtoi(input$giver_select_box_visible_minority), "VISMIN")
    df_filtered <- apply_filter(df_filtered, strtoi(input$giver_select_box_group_religious_participation), "REE_02")

    df_output_giver <<- df_filtered
  })

  # giver counts tab
  output$giver_selected_chart <- renderPlot({
    chart <- giver_response_charts[[input$giver_select_box]]
    update_giver_df()
    chart(df_output_giver)

    # giver_response_charts[[input$giver_selected_box]]
  })

  # giver percentage tab
  output$giver_percentage <- renderPlot({
    chart <- giver_response_percent[[input$giver_select_box]]
    update_giver_df()
    chart(df_output_giver)
  })

  # giver table tab
  output$giver_table <- renderTable({
    update_giver_df()
    tab <- giver_response_tabs[[input$giver_select_box]]
    final_table <- tab(df_output_giver)

    for (i in seq_along(giver_response_charts)) {
      if (input$giver_select_box == names(giver_response_charts[i])) {
        final_table <- final_table %>% rename(!!names(giver_response_charts[i]) := 1, "Count" := 2, "Percentage" := 3)
      }
    }

    return(final_table)
  })
}

shinyApp(ui = ui, server = server)
