rm(list = ls())

#library to read excel
library(readxl)

## @knitr shiny_action_ui

ui <- fluidPage(    # create user interface using fluidpage function

  titlePanel("Inputs"),   # title of app

  # SIDEBAR
  sidebarLayout(    # indicates layout is going to be a sidebar-layout

    sidebarPanel( # open sidebar panel
      width = 6,  #width of the sidebar panel
      
      selectInput(
        inputId = "timehorizon",          # Input ID
        label = "Select time horizon:",   # Label for the dropdown
        choices = 1:5,
        selected = 1,         # Default selected option
        width = "150px"),

      numericInput(
        inputId = "drugcost_olorofim",
        label = "olorofim drug cost per day (USD)",
        value = 907,
        min = 0,
        max = 100000,
        width = "150px"),             

      numericInput(
        inputId = "drugcost_baat",
        label = "BAAT drug cost per day (USD)",
        value = 2261.95,
        min = 0,
        max = 400,
        width = "150px"),
      
      # Slider input
      sliderInput(
        inputId = "slider",           
        label = "Choose a number:",  
        min = 1,                     
        max = 100,                  
        value = 50),
      
      # Table with inputs in the second column
      fluidRow(
        tableOutput("table_header"),
        tags$table(
          style = "width:200px;",
          tags$thead(
            tags$tr(
              tags$th("Coste per day"), tags$th("USD")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("ICU"),
              tags$td(numericInput("cost_icu", label = NULL, value = 6408, width = "100%"))
            ),
            tags$tr(
              tags$td("Ward"),
              tags$td(numericInput("cost_ward", label = NULL, value = 3204, width = "100%"))
            ),
            #tags$tr(
             # tags$td("IV at home"),
             # tags$td(numericInput("cost_ivathome", label = NULL, value = 64.72, width = "100%"))
            #)
          )
        )
    
    ), #close table inputs

    numericInput(
      inputId = "rate_mortality",
      label = "Mortality - mothly rate",
      value = -0.01636806755581,
      min = 0,
      max = 400,
      width = "150px"),
    
    # Table with inputs in the second column
    fluidRow(
      tableOutput("table_header"),
      tags$table(
        style = "width:200px;",
        tags$thead(
          tags$tr(
            tags$th("Utilities"), tags$th("")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Alive no response"),
            tags$td(numericInput("utility_noresponse", label = NULL, value = 0.59, width = "100%"))
          ),
          tags$tr(
            tags$td("Response"),
            tags$td(numericInput("utility_success", label = NULL, value = 0.63, width = "100%"))
          ),
          tags$tr(
            tags$td("death"),
            tags$td(numericInput("utility_death", label = NULL, value = 0.00, width = "100%"))
          ),
          #tags$tr(
            #tags$td("IV at home disutility"),
            #tags$td(numericInput("disutility_ivathome", label = NULL, value = -0.23, width = "100%"))
          #)
        )
      )
      
    ), #close table inputs
    
    
    actionButton(
        inputId = "run_model", # id of action button, used in server
        label   = "Run model") # action button end

   ),  # close sidebarPanel
    
   

    mainPanel(
      width = 6,

      h3("Results") ,

      textOutput(outputId = "printvalue")     # text output

    ) # close mainpanel

  ) # close sidebarlayout

) # close UI fluid page




## SERVER########################################

server <- function(input, output){
  
  sumofeverything <- reactiveVal(0)  # Create a reactive value to store the sum
  
  observeEvent(input$run_model, {
    
    #MARKOV MODEL ENGINE:
    # Rows and columns represent: [Alive No Response, Response, Death]
    tpm <- matrix(c(0.780220, 0.164835, 0.054945,   # From Alive No Response
                    0.315789, 0.684210, 0.000000,   # From Response
                    0.000000, 0.000000, 1.000000),  # From Death (absorbing state)
                  nrow = 3, byrow = TRUE)
    
    # Number of weeks (1 year = 52 weeks)
    weeks <- 52
    
    # State occupancy matrix (each row represents a week)
    health_states <- matrix(0, nrow = weeks + 1, ncol = 3)
    colnames(health_states) <- c("Alive No Response", "Response", "Death")
    
    # Initial state: all patients start in "Alive No Response"
    health_states[1, ] <- c(1, 0, 0)
    
    # Markov model simulation over time
    for (t in 2:(weeks + 1)) {
      health_states[t, ] <- health_states[t - 1, ] %*% tpm  # Update state occupancy
    }
    
    # Read value from Excel file
    excel_file_path <- "data_repository/data_from_excel.xlsx"
    excel_data <- read_excel(excel_file_path, range = "B1:B1", col_names = FALSE)
    
    # Extract the value from B1
    value_from_excel <- as.numeric(excel_data[[1]])
    
    # Calculate sum
    sum_result <- input$drugcost_olorofim + input$drugcost_baat + input$slider + 
      as.numeric(input$timehorizon) + input$cost_icu + input$cost_ward + input$rate_mortality + input$utility_success + 
      input$utility_death + input$utility_noresponse + value_from_excel
    
    sumofeverything(sum_result)  # Update reactive value
    
  })
  
  output$printvalue <- renderText({
    paste("Sum of x and y and slider =", sumofeverything())  # Use the reactive value
  })
  
}



## @knitr shiny_action_apprun

shinyApp(ui, server)
