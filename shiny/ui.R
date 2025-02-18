shiny::shinyUI(
  shinydashboardPlus::dashboardPage(
    skin = "black",
    title = "Rhabdomyolysis characterization",
    shinydashboard::dashboardHeader(
      title = "Characterization"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "menu1",
        shinydashboard::menuItem(
          tabName = "overall",
          text = "Overall",
          icon = shiny::icon("chart-simple")
        ),
        shinydashboard::menuItem(
          tabName = "subgroup_analysis",
          text = "Subgroup analysis",
          icon = shiny::icon("magnifying-glass-chart")
        )
      ),
      shinydashboard::sidebarMenu(
        id = "menu2",
        shiny::selectInput(
          inputId = "analysis_name",
          label = "Analysis",
          choices = c("short_term", "medium_term", "any_time_prior"),
          selected = "short_term"
        ),
        shiny::conditionalPanel(
          condition = "input.menu1 == 'subgroup_analysis'",
          shiny::selectInput(
            inputId = "subgroup_variable",
            label = "Subgroup variable",
            choices = c("stroke_type", "stroke_type_by_gender"),
            selected = "absent"
          )
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "overall",
          shiny::tabsetPanel(
            id = "overall_results",
            shiny::tabPanel(
              title = "Overview",
              DT::dataTableOutput("overall_overview")
            ),
            shiny::tabPanel(
              title = "Age groups",
              DT::dataTableOutput("overall_age_groups")
            ),
            shiny::tabPanel(
              title = "Drugs",
              DT::dataTableOutput("overall_drugs")
            ),
            shiny::tabPanel(
              title = "Conditions",
              DT::dataTableOutput("overall_conditions")
            ),
            shiny::tabPanel(
              title = "Measurement",
              DT::dataTableOutput("overall_measurement")
            ),
            shiny::tabPanel(
              title = "Procedures",
              DT::dataTableOutput("overall_procedures")
            ),
            shiny::tabPanel(
              title = "Drug groups",
              DT::dataTableOutput("overall_drug_groups")
            ),
            shiny::tabPanel(
              title = "Condition groups",
              DT::dataTableOutput("overall_condition_groups")
            ),
            shiny::tabPanel(
              title = "Measurement range",
              DT::dataTableOutput("overall_measurement_range")
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "subgroup_analysis",
          shiny::tabsetPanel(
            id = "subgroup_results",
            shiny::tabPanel(
              title = "Overview",
              DT::dataTableOutput("subgroup_analysis_overview")
            ),
            shiny::tabPanel(
              title = "Age groups",
              DT::dataTableOutput("subgroup_analysis_age_groups")
            ),
            shiny::tabPanel(
              title = "Drugs",
              DT::dataTableOutput("subgroup_analysis_drugs")
            ),
            shiny::tabPanel(
              title = "Conditions",
              DT::dataTableOutput("subgroup_analysis_conditions")
            ),
            shiny::tabPanel(
              title = "Procedures",
              DT::dataTableOutput("subgroup_analysis_procedures")
            ),
            shiny::tabPanel(
              title = "Drug groups",
              DT::dataTableOutput("subgroup_analysis_drug_groups")
            ),
            shiny::tabPanel(
              title = "Condition groups",
              DT::dataTableOutput("subgroup_analysis_condition_groups")
            ),
            shiny::tabPanel(
              title = "Measurement range",
              DT::dataTableOutput("subgroup_analysis_measurement_range")
            )
          )
        )
      )
    )
  )
)