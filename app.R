library(shiny)
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
receiver_response_tabs <- list(
  "Health conditions" = tab_health_conditions,
  "Activity receive help" = tab_activity_receive_help,
  "Age of primary giver" = tab_age_primary_giver,
  "Activity receive help from professional" = tab_activity_receive_help_pro,
  "Hours of help received" = tab_hours_help_received,
  "Primary giver distance" = tab_primary_giver_distance,
  "Receive help banking - frequency" = tab_receive_help_banking_freq
  # "Receive help banking - hours" = chart_receive_help_banking_hours,
  # "Help banking hours - daily" = chart_help_banking_hours_daily,
  # "Help banking hours - at least once a week" = chart_help_banking_weekly,
  # "Help banking hours - at least once a month" = chart_help_banking_monthly,
  # "Help banking hours - less than once a month" = chart_help_banking_monthly_less
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

ui <- fluidPage(
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
        selectInput("general_selected_box",
          "General info:",
          choices = general_charts,
          selected = general_charts[1]
        ),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Counts",
            plotOutput("general_selected_chart")
          ),
          tabPanel("Percentages", "Data shown as percentages will be displayed here"),
          tabPanel("Tables", 
                    tableOutput("general_table")), #table id
          tabPanel("Statistical Significance", "Statisical significance of data will be displayed here")
        )
      )
    )
  ),
  # receiver response charts
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("receiver_select_box", "Questions asked to older adults who received care:", choices = names(receiver_response_charts),selected = names(receiver_response_charts[1])),
        selectInput("receiver_select_box_sex", "Filter by sex", choices = filter_sex, selected = filter_sex[1]),
        selectInput("receiver_select_box_age", "Age group", filter_age_group, selected = filter_age_group[1]),
        selectInput("receiver_select_box_pop_centre", "Population Centre", filter_pop_centre, selected = filter_pop_centre[1]),
        selectInput("receiver_select_box_partner_in_household", "Spouse/Partner living in household", filter_partner_in_household, selected = filter_partner_in_household[1]),
        selectInput("receiver_select_box_living_arrangement_senior_household", "Living arrangement of senior respondent's household", filter_living_arrangement_senior_household, selected = filter_partner_in_household[1]),
        selectInput("receiver_select_box_indigenous_status", "Indigenous status", filter_indigenous_status, selected = filter_indigenous_status[1]),
        selectInput("receiver_select_box_visible_minority", "Visible minority status", filter_visible_minority_status, selected = filter_visible_minority_status[1]),
        selectInput("receiver_select_box_group_religious_participation", "Group religious participation", filter_group_religious_participation, selected = filter_group_religious_participation[1])
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Counts",
            plotOutput("receiver_selected_chart")
          ),
          tabPanel("Percentages", "Data shown as percentages will be displayed here"),
          tabPanel("Tables", 
                   tableOutput("receiver_table")), # receiver table id
          tabPanel("Statistical Significance", "Statisical significance of data will be displayed here")
        )
      )
    )
  ),
  # giver response charts
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("giver_selected_box", "Questions asked to respondents who provided care to older adults:", choices = names(giver_response_charts), selected = names(giver_response_charts[1])),
        selectInput("giver_selected_box_sex", "Filter by sex", choices = filter_sex, selected = filter_sex[1])
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Counts",
            plotOutput("giver_selected_chart")
          ),
          tabPanel("Percentages", "Data shown as percentages will be displayed here"),
          tabPanel("Tables", "Tabluar data will be displayed here"),
          tabPanel("Statistical Significance", "Statisical significance of data will be displayed here")
        )
      )
    )
  )
)

server <- function(input, output) {
  output_receiver_df <- df_receiver
  
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
  
  # general table tab
  output$general_table <- renderTable({
    if (input$general_selected_box == general_charts[1]) {
      df_pops %>% rename("Respondent Group" = pop_name, "Count" = pop_freq)
    } else {
      df_primary_sex %>% rename("Sex" = sex, "Count" = freq, )
    }
  })
  
  update_receiver_df <- reactive({
    # filter by sex
    filtered_df <- if (input$receiver_select_box_sex == filter_sex[2]) {
      filtered_df <- df_receiver %>% filter(SEX == 1)
    } else if (input$receiver_select_box_sex == filter_sex[3]) {
      filtered_df <- df_receiver %>% filter(SEX == 2)
    } else {
      df_receiver
    }
    
    # filter 65+, 65-74, 75+
    filtered_df <- if (input$receiver_select_box_age == filter_age_group[2]) {
      filtered_df <- filtered_df %>% filter(AGEGR10 == 6)
    } else if (input$receiver_select_box_age == filter_age_group[3]) {
      filtered_df <- filtered_df %>% filter(AGEGR10 == 7)
    } else {
      filtered_df
    }
    
    # filter population centre
    filtered_df <- if (input$receiver_select_box_pop_centre == filter_pop_centre[2]) {
      filtered_df <- filtered_df %>% filter(LUC_RST == 1)
    } else if (input$receiver_select_box_pop_centre == filter_pop_centre[3]) {
      filtered_df <- filtered_df %>% filter(LUC_RST == 2)
    } else if (input$receiver_select_box_pop_centre == filter_pop_centre[4]) {
      filtered_df <- filtered_df %>% filter(LUC_RST == 3)
    } else {
      filtered_df
    }
    
    filtered_df <- if (input$receiver_select_box_partner_in_household == filter_partner_in_household[2]) {
      filtered_df <- filtered_df %>% filter(PHSDFLG == 1)
    } else if (input$receiver_select_box_partner_in_household == filter_partner_in_household[3]) {
      filtered_df <- filtered_df %>% filter(PHSDFLG == 2)
    } else {
      filtered_df
    }
    
    filtered_df <- if (input$receiver_select_box_living_arrangement_senior_household ==
                       filter_living_arrangement_senior_household[2]) {
      filtered_df <- filtered_df %>% filter(LIVARRSN == 1)
    } else if (input$receiver_select_box_living_arrangement_senior_household ==
               filter_living_arrangement_senior_household[3]) {
      filtered_df <- filtered_df %>% filter(LIVARRSN == 2)
    } else if (input$receiver_select_box_living_arrangement_senior_household ==
               filter_living_arrangement_senior_household[4]) {
      filtered_df <- filtered_df %>% filter(LIVARRSN == 3)
    } else if (input$receiver_select_box_living_arrangement_senior_household ==
               filter_living_arrangement_senior_household[5]) {
      filtered_df <- filtered_df %>% filter(LIVARRSN == 4)
    } else if (input$receiver_select_box_living_arrangement_senior_household ==
               filter_living_arrangement_senior_household[6]) {
      filtered_df <- filtered_df %>% filter(LIVARRSN == 5)
    }else {
      filtered_df
    }
    
    filtered_df <- if (input$receiver_select_box_indigenous_status == filter_indigenous_status[2]) {
      filtered_df <- filtered_df %>% filter(AMB_01_1 == 1)
    } else if (input$receiver_select_box_indigenous_status == filter_indigenous_status[3]) {
      filtered_df <- filtered_df %>% filter(AMB_01_1 == 2)
    } else {
      filtered_df
    }
    
    filtered_df <- if (input$receiver_select_box_visible_minority == filter_visible_minority_status[2]) {
      filtered_df <- filtered_df %>% filter(VISMIN == 1)
    } else if (input$receiver_select_box_visible_minority == filter_visible_minority_status[3]) {
      filtered_df <- filtered_df %>% filter(VISMIN == 2)
    } else {
      filtered_df
    }
    
    filtered_df <- if (input$receiver_select_box_group_religious_participation == filter_group_religious_participation[2]) {
      filtered_df <- filtered_df %>% filter(REE_02 == 1)
    } else if (input$receiver_select_box_group_religious_participation == filter_group_religious_participation[3]) {
      filtered_df <- filtered_df %>% filter(REE_02 == 2)
    } else {
      filtered_df
    }
    
    output_receiver_df <<- filtered_df
  })
  
  # receiver counts tab
  output$receiver_selected_chart <- renderPlot({
    chart <- receiver_response_charts[[input$receiver_select_box]]
    update_receiver_df()
    chart(output_receiver_df)
  })

  # receiver table tab
  output$receiver_table <- renderTable({
    update_receiver_df()
    tab <- receiver_response_tabs[[input$receiver_select_box]]
    tab(output_receiver_df)
  })
  
  
  # giver counts tab
  output$giver_selected_chart <- renderPlot({
    chart_function <- giver_response_charts[[input$giver_selected_box]]

    # filter by sex
    filtered_df <- if (input$giver_selected_box_sex == filter_sex[2]) {
      filtered_df <- df_giver %>% filter(SEX == 1)
      # df_giver_male
    } else if (input$giver_selected_box_sex == filter_sex[3]) {
      filtered_df <- df_giver %>% filter(SEX == 2)
      # df_giver_female
    } else {
      df_giver
    }

    chart_function(filtered_df)
    # giver_response_charts[[input$giver_selected_box]]
  })
}

shinyApp(ui = ui, server = server)