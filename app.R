# College Fund Calculator - Shiny App
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(scales)
library(gridExtra)
library(shinyjs)
library(dplyr)
library(reshape2)

# UI definition 
ui <- dashboardPage(
  dashboardHeader(title = "College Fund Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Cost Details", tabName = "costdetails", icon = icon("table")),
      menuItem("Investment Details", tabName = "investmentdetails", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      
      # Main calculator tab
      tabItem(tabName = "calculator", 
              fluidRow(
                
                # Input parameters panel
                box(title = "Investment Parameters", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    fluidRow(column(12, actionButton("calculate", 
                                                     "Calculate Required Investment", 
                                                     class = "btn-primary", 
                                                     width = "100%"))),
                    fluidRow(column(3, numericInput("investment_startyear", 
                                                    "Year at Start of Investing", 
                                                    value = 2025, 
                                                    min = 2025, 
                                                    max = 2050, 
                                                    step = 1)),
                             column(3, numericInput("investment_startage", 
                                                    "Age at Start of Investing", 
                                                    value = 0, 
                                                    min = 0, 
                                                    max = 17, 
                                                    step = 1))),
                    fluidRow(column(3, numericInput("college_startage", 
                                                    "Age at Start of Higher Education", 
                                                    value = 18, 
                                                    min = 16, 
                                                    max = 25, 
                                                    step = 1)),
                             column(3, numericInput("college_studyyears", 
                                                    "Years of College", 
                                                    value = 4, 
                                                    min = 1, 
                                                    max = 10, 
                                                    step = 1))),
                    fluidRow(column(4, numericInput("primary_rate", 
                                                    "Expected Growth - Primary Fund (%)", 
                                                    value = 7, 
                                                    min = 0, 
                                                    max = 15, 
                                                    step = 0.1)),
                             column(4, numericInput("secondary_rate", 
                                                    "Expected Growth - Secondary Fund (%)", 
                                                    value = 10, 
                                                    min = 0, 
                                                    max = 15, 
                                                    step = 0.1))),
                    fluidRow(column(4, numericInput("contribution_rate", 
                                                    "Annual Increase in Contributions (%)", 
                                                    value = 4, 
                                                    min = 0, 
                                                    max = 10, 
                                                    step = 0.1))),
                    fluidRow(column(4, radioButtons("inflation_method", 
                                                    "Education Inflation", 
                                                    choices = c("Historical Education Inflation Rate" = "historical", "Fixed Rate of Education Inflation" = "fixed"), 
                                                    selected = "historical", 
                                                    inline = TRUE),
                                    conditionalPanel(condition = "input.inflation_method == 'fixed'",
                                                     numericInput("inflation_rate", 
                                                                  "Expected Annual Education Inflation (%)", 
                                                                  value = 4, 
                                                                  min = 0, 
                                                                  max = 10, 
                                                                  step = 0.1)),
                                    conditionalPanel(condition = "input.inflation_method == 'historical'",
                                                     uiOutput("historical_inflation_display"
                                                     ))
                    ))
                )),
              
              # Costs data entry panel
              fluidRow(
                
                box(title = "College Cost Data Entry", 
                    status = "warning", 
                    solidHeader = TRUE,
                    width = 12,
                    p("Enter known costs for future college expenses. Additional rows will be automatically added as needed."),
                    DTOutput("costs_table"),
                    actionButton("add_year", "Add Another Year", class = "btn-success"))),
              
              # Summary metrics panel
              fluidRow(valueBoxOutput("total_cost_box", width = 4),
                       valueBoxOutput("primary_contribution_box", width = 4),
                       valueBoxOutput("secondary_contribution_box", width = 4)),
              
              # Results panel with tabs
              fluidRow(tabBox(title = "Results", 
                              id = "results_tabs", 
                              width = 12,
                              tabPanel("Summary", 
                                       fluidRow(column(4, box(title = "Summary of Projections", 
                                                              width = NULL, 
                                                              status = "primary", 
                                                              verbatimTextOutput("summary_text"))),
                                                column(8, box(title = "Investment Growth Projection", 
                                                              width = NULL, 
                                                              status = "info", 
                                                              plotOutput("investment_growth_plot", height = "300px")))),
                                       fluidRow(box(title = "College Cost Projection", 
                                                    width = 12, 
                                                    status = "warning", 
                                                    plotOutput("cost_breakdown_plot", height = "300px")))),
                              tabPanel("Projections",
                                       fluidRow(box(title = "Cost Projections", 
                                                    width = 6, 
                                                    status = "warning", 
                                                    plotOutput("yearly_cost_breakdown", height = "300px")),
                                                box(title = "Investment Growth", 
                                                    width = 6, 
                                                    status = "info", 
                                                    plotOutput("yearly_contributions", height = "300px"))),
                                       fluidRow(box(title = "Projected College Costs by Year", 
                                                    width = 12, 
                                                    status = "warning", 
                                                    DTOutput("college_costs_table")))
                              )))
      ),
      
      # Cost details tab
      tabItem(tabName = "costdetails",
              
              fluidRow(
                box(title = "College Cost Details", 
                    width = 12, 
                    status = "warning",
                    p("Detailed breakdown of estimated college costs by year and category."),
                    DTOutput("cost_details_table"))),
              fluidRow(
                box(title = "Cost Breakdown", 
                    width = 12, 
                    status = "warning",
                    plotOutput("cost_category_breakdown", height = "400px")))
      ),
      
      # Investment details tab
      tabItem(tabName = "investmentdetails",
              
              fluidRow(
                box(title = "Investment Growth Details", 
                    width = 12, 
                    status = "info",
                    p("Detailed breakdown of investment growth by year."),
                    DTOutput("investment_details_table"))),
              fluidRow(
                box(title = "Investment Contribution by Year", 
                    width = 12, 
                    status = "info",
                    DTOutput("investment_contribution_table"))),
              fluidRow(
                box(title = "Annual Investment Contribution", 
                    width = 12, 
                    status = "info",
                    plotOutput("annual_contribution_chart", height = "400px")))
      ),
      
      # About tab
      tabItem(tabName = "about",
              box(title = "About This Calculator", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  p("This college fund calculator helps you determine how much you need to invest now to cover future college expenses."),
                  p("The calculator takes into account:"),
                  tags$ul(tags$li("Actual and projected college costs including tuition, fees, housing, meals, books, transportation, and personal expenses"),
                          tags$li("Expected inflation rates for higher education"),
                          tags$li("Investment growth rates for different investment funds"),
                          tags$li("Annual increases in your contributions")
                  ),
                  p("Key Concepts:"),
                  tags$ul(tags$li(strong("Primary Expenses:"), "Tuition, fees, housing, meals, and books"),
                          tags$li(strong("Secondary Expenses:"), "Transportation and personal expenses"),
                          tags$li(strong("Primary Fund:"), "Typically a more conservative investment approach for essential expenses"),
                          tags$li(strong("Secondary Fund:"), "Often allows for more aggressive growth strategy for discretionary expenses")
                  ),
                  p("Inflation Methods:"),
                  tags$ul(tags$li(strong("Historical Inflation:"), "Calculated as the compound annual growth rate (CAGR) from the cost data you enter. Requires at least 2 years of data. Formula: ((ending cost / starting cost) ^ (1 / years)) - 1"),
                          tags$li(strong("Fixed Rate:"), "A manually specified annual inflation rate applied uniformly to project future costs")
                  ),
                  p("Instructions:"),
                  tags$ol(tags$li("Fill in the parameters in the 'Investment Parameters' section"),
                          tags$li("Choose an inflation method: Historical (default) uses your entered cost data; Fixed Rate uses a manually specified percentage"),
                          tags$li("Enter any known college costs in the 'College Cost Data Entry' table"),
                          tags$li("Click 'Calculate Required Investment' to see the results"),
                          tags$li("Review the summary tab for an overview of projected costs and required investments"),
                          tags$li("Explore the projections tab for detailed year-by-year breakdowns"),
                          tags$li("Use the navigation menu to view detailed cost and investment information")
                  ))
      )
    )
  )
)

# Server logic 
server <- function(input, output, session) {
  
  if (FALSE) {
    
    # Guarded clause for future dev or troubleshooting
    input <- list(
      college_studyyears = 4, 
      investment_startage = 0, 
      college_startage = 18, 
      investment_startyear = 2025, 
      inflation_method = "historical",
      inflation_rate = 4, 
      contribution_rate = 4, 
      primary_rate = 7, 
      secondary_rate = 10
    )
    
  }
  
  # Create a lookup table to match cost_varvals to cost_varlabs
  # Define the cost_varvals and cost_varlabs globally to ensure consistency
  # Define cost types
  primary_cost_dictionary <- c(
    "Tuition" = "cost_tuition", 
    "Fees" = "cost_fees", 
    "Housing" = "cost_housing", 
    "Meals" = "cost_meals", 
    "Books" = "cost_books"
  )
  
  secondary_cost_dictionary <- c(
    "Transportation" = "cost_transport", 
    "Personal" = "cost_personal"
  )
  
  cost_dictionary <- c(primary_cost_dictionary, secondary_cost_dictionary)
  
  cost_varlabs <- names(cost_dictionary)
  primary_cost_varlabs <- names(primary_cost_dictionary)
  secondary_cost_varlabs <- names(secondary_cost_dictionary)

  primary_cost_varvals <- unname(primary_cost_dictionary)
  secondary_cost_varvals <- unname(secondary_cost_dictionary)
  cost_varvals <- unname(cost_dictionary)

  if (FALSE) {
    # Guarded clause for future dev or troubleshooting
    costs_df <- function(reacting_costs = NULL) {
      if (!is.null(reacting_costs)) {
        current_costs
      } else {
        tibble::tibble(
          year = c(2025, 2026),
          age = c(0, 1),
          cost_tuition = c(10086, 10908),
          cost_fees = c(1772, 1800),
          cost_housing = c(9562, 10290),
          cost_meals = c(6396, 7180),
          cost_books = c(1250, 1250),
          cost_transport = c(1514, 1514),
          cost_personal = c(1200, 1200)
        )
      }
    } 
  }
  
  # Initial costs data frame
  costs_df <- reactiveVal(tibble::tibble(
    year = c(2025, 2026),
    age = c(0, 1),
    cost_tuition = c(10086, 10908),
    cost_fees = c(1772, 1800),
    cost_housing = c(9562, 10290),
    cost_meals = c(6396, 7180),
    cost_books = c(1250, 1250),
    cost_transport = c(1514, 1514),
    cost_personal = c(1200, 1200)
  ))
  
  # Update year and age in costs table when parameters change
  observe({
    req(input$investment_startyear)
    req(input$investment_startage)
    
    reacting_costs <- costs_df()
    
    # Update years and ages
    investment_startyear <- input$investment_startyear
    investment_startage <- input$investment_startage
    
    new_years <- seq(from = investment_startyear, length.out = nrow(reacting_costs))
    new_ages <- seq(from = investment_startage, length.out = nrow(reacting_costs))
    
    reacting_costs$year <- new_years
    reacting_costs$age <- new_ages
    
    costs_df(reacting_costs)
  })
  
  # --------------------------------------------------------------------------
  # Historical inflation calculation (CAGR from entered cost data)
  # --------------------------------------------------------------------------
  
  if (FALSE) {
    
    calculated_historical_inflation <- function() {
      reacting_costs <- costs_df()
      
      # Need at least 2 rows to compute historical inflation
      if (nrow(reacting_costs) < 2) {
        return(NULL)
      }
      
      # Compute total cost of attendance for each row
      row_costs <- rowSums(reacting_costs[, cost_varvals, drop = FALSE])
      
      first_cost <- row_costs[1]
      last_cost   <- row_costs[length(row_costs)]
      n_years       <- nrow(reacting_costs) - 1
      
      # Guard against zero or negative starting cost
      if (is.na(first_cost) || first_cost <= 0) {
        return(NULL)
      }
      
      # CAGR formula: ((ending / starting) ^ (1/n)) - 1
      cagr <- (last_cost / first_cost) ^ (1 / n_years) - 1
      return(cagr)
    }
    
  }
  
  calculated_historical_inflation <- reactive({
    reacting_costs <- costs_df()
    
    # Need at least 2 rows to compute historical inflation
    if (nrow(reacting_costs) < 2) {
      return(NULL)
    }
    
    # Compute total cost of attendance for each row
    row_costs <- rowSums(reacting_costs[, cost_varvals, drop = FALSE])
    
    first_cost <- row_costs[1]
    last_cost   <- row_costs[length(row_costs)]
    n_years       <- nrow(reacting_costs) - 1
    
    # Guard against zero or negative starting cost
    if (is.na(first_cost) || first_cost <= 0) {
      return(NULL)
    }
    
    # CAGR formula: ((ending / starting) ^ (1/n)) - 1
    cagr <- (last_cost / first_cost) ^ (1 / n_years) - 1
    return(cagr)
  })
  
  # --------------------------------------------------------------------------
  # Effective inflation rate: resolves which rate to actually use
  # --------------------------------------------------------------------------
  
  if (FALSE) {
    # Guarded clause for future dev or troubleshooting
    effective_inflation <- function() {
      if (input$inflation_method == "historical") {
        inflation_historical <- calculated_historical_inflation()
        if (!is.null(inflation_historical)) {
          return(inflation_historical)
        } else {
          # Fall back to fixed rate when historical is unavailable
          return(input$inflation_rate / 100)
        }
      } else {
        return(input$inflation_rate / 100)
      }
    }
  }
  
  effective_inflation <- reactive({
    if (input$inflation_method == "historical") {
      inflation_historical <- calculated_historical_inflation()
      if (!is.null(inflation_historical)) {
        return(inflation_historical)
      } else {
        # Fall back to fixed rate when historical is unavailable
        return(input$inflation_rate / 100)
      }
    } else {
      return(input$inflation_rate / 100)
    }
  })
  
  # --------------------------------------------------------------------------
  # UI output: display the historical inflation rate or fallback message
  # --------------------------------------------------------------------------
  output$historical_inflation_display <- renderUI({
    inflation_historical <- calculated_historical_inflation()
    reacting_costs <- costs_df()

    if (!is.null(inflation_historical)) {
      tags$div(
        style = "padding: 8px 12px; background-color: #d5edda; border: 1px solid #c3e6cb; border-radius: 4px; margin-top: 5px;",
        tags$strong(
          style = "color: #155724;",
          icon("chart-line"),
          sprintf(" Historical inflation rate: %.1f%%", inflation_historical * 100)
        ),
        tags$br(),
        tags$small(
          style = "color: #155724;",
          sprintf("Computed as CAGR from %d year(s) of entered cost data", nrow(reacting_costs))
        )
      )
    } else {
      tags$div(
        style = "padding: 8px 12px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; margin-top: 5px;",
        tags$strong(
          style = "color: #856404;",
          icon("exclamation-triangle"),
          sprintf(" Not enough data \u2014 using fixed rate: %.1f%%", input$inflation_rate)
        ),
        tags$br(),
        tags$small(
          style = "color: #856404;",
          "Enter at least 2 years of cost data to enable historical inflation"
        )
      )
    }
  })
  
  # Display costs table
  output$costs_table <- renderDT({
    datatable(
      costs_df(),
      editable = TRUE,
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE
      )
    )
  })
  
  # Update costs data frame when cells are edited
  observeEvent(input$costs_table_cell_edit, {
    info <- input$costs_table_cell_edit
    reacting_costs <- costs_df()
    reacting_costs[info$row, info$col] <- info$value
    costs_df(reacting_costs)
  })
  
  # Add a new year to the costs table
  observeEvent(input$add_year, {
    reacting_costs <- costs_df()
    last_row <- nrow(reacting_costs)
    new_row <- reacting_costs[last_row, ]
    
    # Increment year and age
    new_row$year <- new_row$year + 1
    new_row$age <- new_row$age + 1
    
    # Apply inflation to costs (use effective inflation for the new row projection)
    inflation_factor <- 1 + effective_inflation()
    for(col in cost_varvals) {
      new_row[[col]] <- new_row[[col]] * inflation_factor
    }
    
    costs_df(rbind(reacting_costs, new_row))
  })
  
  # Calculate results when the button is clicked
  results <- eventReactive(input$calculate, {
    
    # Get inputs
    college_studyyears <- input$college_studyyears
    investment_startage <- input$investment_startage
    college_startage <- input$college_startage
    investment_startyear <- input$investment_startyear
    inflation_rate <- effective_inflation()
    contribution_rate <- input$contribution_rate / 100
    primary_rate <- input$primary_rate / 100
    secondary_rate <- input$secondary_rate / 100
    
    # Determine which inflation method was actually used
    inflation_method <- input$inflation_method
    inflation_historical <- calculated_historical_inflation()
    if (inflation_method == "historical" && is.null(inflation_historical)) {
      inflation_method <- "fixed (fallback)"
    }
    
    # Notify user if historical was requested but fell back to fixed
    if (input$inflation_method == "historical" && is.null(inflation_historical)) {
      showNotification(
        "Historical inflation unavailable (need 2+ years of cost data). Using fixed rate instead.",
        type = "warning",
        duration = 8
      )
    }
    
    # Extract costs from the table
    reacting_costs <- costs_df()
    
    # Create named lists for each cost type
    reacted_costs <- reacting_costs
    
    # Calculate age and year values
    college_endage <- college_startage + college_studyyears - 1
    college_endyear <- investment_startyear + (college_endage - investment_startage)
    
    # Calculate age and year sequences
    analysis_ages <- seq(from = investment_startage, to = college_endage)
    analysis_years <- seq(from = investment_startyear, to = college_endyear)
    college_years <- analysis_years[(length(analysis_years) - college_studyyears + 1):length(analysis_years)]
    college_startyear <- min(college_years)
    college_endyear <- max(college_years)
    
    investment_skips <- length(analysis_years) - college_studyyears
    investment_years <- investment_skips + 1
    
    # Working data frame
    working_00 <- tibble::tibble(
      year = analysis_years, 
      age = analysis_ages
    ) %>% 
      dplyr::mutate(
        college_studyyears = college_studyyears,
        investment_startage = investment_startage,
        college_startage = college_startage,
        investment_startyear = investment_startyear,
        college_endage = college_endage,
        college_endyear = college_endyear,
        college_startyear = college_startyear,
        college_endyear = college_endyear,
        investment_years = investment_years
      )
    
    working_01 <- working_00 %>% 
      # Indicate if in in_college during eligible range of years/ages
      dplyr::mutate(in_college = analysis_years %in% college_years) %>% 
      # Join reacted_costs
      dplyr::left_join(reacted_costs) %>% 
      dplyr::mutate(primary_cost = rowSums(select({.}, tidyselect::all_of(unname(primary_cost_dictionary))))) %>% 
      dplyr::mutate(secondary_cost = rowSums(select({.}, tidyselect::all_of(unname(secondary_cost_dictionary))))) %>% 
      dplyr::mutate(cost_total = primary_cost + secondary_cost) %>% 
      # Add constants
      dplyr::mutate(inflation_rate = inflation_rate) %>% 
      dplyr::mutate(contribution_rate = contribution_rate) %>% 
      dplyr::mutate(primary_rate = primary_rate) %>% 
      dplyr::mutate(secondary_rate = secondary_rate) %>% 
      # Add and update multipliers 
      dplyr::mutate(inflation_factor = 1 + inflation_rate) %>% 
      dplyr::mutate(contribution_factor = 1 + contribution_rate) %>% 
      dplyr::mutate(primary_factor = 1 + primary_rate) %>% 
      dplyr::mutate(secondary_factor = 1 + secondary_rate) %>% 
      # Update with is.na
      dplyr::mutate(inflation_factor = ifelse(!is.na(cost_tuition), 1, inflation_factor)) %>% 
      # Update with row_number
      dplyr::mutate(primary_factor = ifelse(dplyr::row_number() == 1, 1, primary_factor)) %>% 
      dplyr::mutate(contribution_factor = ifelse(dplyr::row_number() == 1, 1, contribution_factor)) %>% 
      dplyr::mutate(secondary_factor = ifelse(dplyr::row_number() == 1, 1, secondary_factor)) %>% 
      # Calculate cumprod to facilitate calculation of inflated values
      dplyr::mutate(inflation_index = round(cumprod(inflation_factor), 6)) %>% 
      dplyr::mutate(contribution_index = round(cumprod(contribution_factor), 6)) %>% 
      dplyr::mutate(primary_index = round(cumprod(primary_factor), 6)) %>%
      dplyr::mutate(secondary_index = round(cumprod(secondary_factor), 6)) %>%
      # Fill (down) columns that start with "cost_" and then calculate remaining inflated values
      tidyr::fill(tidyselect::contains("cost"), .direction = "down") %>% 
      dplyr::mutate(dplyr::across(tidyselect::contains("cost"), ~ inflation_index * .x)) %>% 
      # Calculate cumprod_growth
      dplyr::mutate(primary_revindex = primary_factor ^ pmax(rev(dplyr::row_number()) - college_studyyears, 0)) %>% 
      dplyr::mutate(secondary_revindex = secondary_factor ^ pmax(rev(dplyr::row_number()) - college_studyyears, 0))
    
    college_category_costs <- working_01 %>% 
      dplyr::filter(year %in% college_years) %>% 
      dplyr::select(tidyselect::all_of(cost_varvals)) %>% 
      colSums()
    
    # Calculate primary, secondary, total costs
    college_primary_cost <- working_01 %>% 
      dplyr::filter(year %in% college_years) %>% 
      dplyr::pull(primary_cost) %>% 
      sum()
    
    college_secondary_cost <- working_01 %>% 
      dplyr::filter(year %in% college_years) %>% 
      dplyr::pull(secondary_cost) %>% 
      sum()
    
    # Rename to clarify
    college_cost <- working_01 %>% 
      dplyr::filter(year %in% college_years) %>% 
      dplyr::pull(cost_total) %>% 
      sum()
    
    # Create a function that calculate investment growth
    calculate_growth <- function(
    df, 
    col_index,
    contribution_seed, 
    col_contribution, 
    col_age, 
    col_college_startage, 
    col_cost,
    col_rate, 
    col_growth, 
    col_balance
    ) {
      
      # One value in col
      college_startage <- df[[col_college_startage]]
      
      # Many values in col
      index <- df[[col_index]]
      age <- df[[col_age]]
      cost <- df[[col_cost]]
      
      rate <- df[[col_rate]]
      contribution <- ifelse(age < college_startage, contribution_seed * index, 0)
      df[[col_contribution]] <- contribution
      
      # Initialize vectors for calculated values
      growth <- vector(mode = "numeric", length = nrow(df)) 
      balance_current <- vector(mode = "numeric", length = nrow(df)) 
      
      i <- NULL
      # Calculate fund growth and balance
      for (i in seq_len(nrow(df))) {
        
        contribution_current <- if (age[i] < college_startage[i]) { contribution[i] } else { ( (-1) * (cost[i])) }
        # Get previous balance (0 for first year)
        balance_previous <- if (i > 1) { balance_current[i-1] } else { 0 }
        # Calculate fund growth based on formula
        growth[i] <- (contribution_current + balance_previous) * rate[i]
        # Update balance
        balance_current[i] <- balance_previous + contribution_current + growth[i]
        
      }
      
      df[[col_growth]] <- growth
      df[[col_balance]] <- balance_current
      return(df)
    }
    
    {
      
      # Calculate required initial investment for expenses
      primary_contribution_seed <- 1
      primary_fund_balance <- 0
      df_loop <- working_01
      
      if (FALSE) {
        # Guarded clause for future dev or troubleshooting
        df = df_loop
        col_index = "contribution_index"
        contribution_seed = primary_contribution_seed
        col_contribution = "primary_contribution"
        col_age = "age"
        col_college_startage = "college_startage"
        col_cost = "primary_cost"
        col_rate = "primary_rate"
        col_growth = "primary_growth"
        col_balance = "primary_balance"
      }
      
      while ((college_primary_cost - primary_fund_balance) > 0) {
        df_loop <- calculate_growth(
          df = df_loop, 
          col_index = "contribution_index",
          contribution_seed = primary_contribution_seed, 
          col_contribution = "primary_contribution", 
          col_age = "age", 
          col_college_startage = "college_startage", 
          col_cost = "primary_cost",
          col_rate = "primary_rate",
          col_growth = "primary_growth",
          col_balance = "primary_balance"
        )
        
        primary_fund_balance <- max(df_loop[["primary_balance"]])
        
        if (!(college_primary_cost - primary_fund_balance) > 0) {
          break
        }
        
        primary_contribution_seed <- primary_contribution_seed + 1
      }
      
      print(primary_contribution_seed)
      
    }
    
    {
      
      # Calculate required initial investment for expenses
      secondary_contribution_seed <- 1
      secondary_fund_balance <- 0
      df_loop <- working_01
      
      if (FALSE) {
        # Guarded clause for future dev or troubleshooting
        df = df_loop
        col_index = "contribution_index"
        contribution_seed = secondary_contribution_seed
        col_contribution = "secondary_contribution"
        col_age = "age"
        col_college_startage = "college_startage"
        col_cost = "secondary_cost"
        col_rate = "secondary_rate"
        col_growth = "secondary_growth"
        col_balance = "secondary_balance"
      }
      
      while ((college_secondary_cost - secondary_fund_balance) > 0) {
        df_loop <- calculate_growth(
          df = df_loop, 
          col_index = "contribution_index",
          contribution_seed = secondary_contribution_seed, 
          col_contribution = "secondary_contribution", 
          col_age = "age", 
          col_college_startage = "college_startage", 
          col_cost = "secondary_cost",
          col_rate = "secondary_rate",
          col_growth = "secondary_growth",
          col_balance = "secondary_balance"
        )
        
        secondary_fund_balance <- max(df_loop[["secondary_balance"]])
        
        if (!(college_secondary_cost - secondary_fund_balance) > 0) {
          break
        }
        
        secondary_contribution_seed <- secondary_contribution_seed + 1
      }
      
      print(secondary_contribution_seed)
      
    }
    
    working_02 <- working_01 %>% 
      calculate_growth(
        col_index = "contribution_index",
        contribution_seed = primary_contribution_seed, 
        col_contribution = "primary_contribution", 
        col_age = "age", 
        col_college_startage = "college_startage", 
        col_cost = "primary_cost",
        col_rate = "primary_rate",
        col_growth = "primary_growth",
        col_balance = "primary_balance"
      ) %>% 
      calculate_growth(
        col_index = "contribution_index",
        contribution_seed = secondary_contribution_seed, 
        col_contribution = "secondary_contribution", 
        col_age = "age", 
        col_college_startage = "college_startage", 
        col_cost = "secondary_cost",
        col_rate = "secondary_rate",
        col_growth = "secondary_growth",
        col_balance = "secondary_balance"
      ) %>% 
      dplyr::mutate(total_contribution = primary_contribution + secondary_contribution) %>% 
      dplyr::mutate(total_growth = primary_growth + secondary_growth) %>% 
      dplyr::mutate(total_balance = primary_balance + secondary_balance) 
    
    working_final <- working_02
    
    # Return a list of all calculated values - make sure this is COMPLETE
    return(list(
      working_final = working_final,
      college_cost = college_cost,
      college_primary_cost = college_primary_cost,
      college_secondary_cost = college_secondary_cost,
      primary_contribution_seed = primary_contribution_seed,
      secondary_contribution_seed = secondary_contribution_seed,
      college_startyear = college_startyear,
      college_endyear = college_endyear,
      analysis_years = analysis_years,
      college_years = college_years,
      college_category_costs = college_category_costs,
      inflation_rate = inflation_rate,
      inflation_method = inflation_method
    ))
    
    if (FALSE) {
      results <- function() {
        list(
          working_final = working_final,
          college_cost = college_cost,
          college_primary_cost = college_primary_cost,
          college_secondary_cost = college_secondary_cost,
          primary_contribution_seed = primary_contribution_seed,
          secondary_contribution_seed = secondary_contribution_seed,
          college_startyear = college_startyear,
          college_endyear = college_endyear,
          analysis_years = analysis_years,
          college_years = college_years,
          college_category_costs = college_category_costs,
          inflation_rate = inflation_rate,
          inflation_method = inflation_method
        )
      }
      r <- results
    }
    
  })
  
  # Value box outputs
  output$total_cost_box <- renderValueBox({
    req(results())
    valueBox(
      value = dollar(results()$college_cost),
      subtitle = "Total College Costs",
      icon = icon("university"),
      color = "red"
    )
  })
  
  output$primary_contribution_box <- renderValueBox({
    req(results())
    valueBox(
      value = dollar(results()$primary_contribution_seed),
      subtitle = "Initial Primary Investment",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$secondary_contribution_box <- renderValueBox({
    req(results())
    valueBox(
      value = dollar(results()$secondary_contribution_seed),
      subtitle = "Initial Secondary Investment",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  # Summary text output
  output$summary_text <- renderText({
    req(results())
    
    # Format the inflation method description
    inflation_desc <- if (results()$inflation_method == "historical") {
      sprintf("Historical (CAGR): %.1f%%", results()$inflation_rate * 100)
    } else if (results()$inflation_method == "fixed (fallback)") {
      sprintf("Fixed (fallback): %.1f%%", results()$inflation_rate * 100)
    } else {
      sprintf("Fixed: %.1f%%", results()$inflation_rate * 100)
    }
    
    paste0(
      "College Years: ", results()$college_startyear, " - ", results()$college_endyear, "\n",
      "Inflation Method: ", inflation_desc, "\n\n",
      "Total College Cost: ", dollar(results()$college_cost), "\n",
      "  - Primary Expenses: ", dollar(results()$college_primary_cost), "\n",
      "  - Secondary Expenses: ", dollar(results()$college_secondary_cost), "\n\n",
      "Required Investment:\n",
      "  - Primary Fund: ", dollar(results()$primary_contribution_seed), "\n",
      "  - Secondary Fund: ", dollar(results()$secondary_contribution_seed), "\n",
      "  - Total Initial: ", dollar(results()$primary_contribution_seed + results()$secondary_contribution_seed)
    )
  })
  
  # Investment growth plot
  output$investment_growth_plot <- renderPlot({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, total_contribution, total_growth, total_balance)
    
    investment_data_long <- melt(investment_data, id.vars = "year", 
                                 measure.vars = c("total_contribution", "total_growth"))
    
    ggplot(investment_data_long, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("total_contribution" = "#3498db", "total_growth" = "#2ecc71"),
                        labels = c("Contribution", "Growth")) +
      labs(x = "Year", y = "Amount ($)", fill = "Type") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Cost breakdown plot
  output$cost_breakdown_plot <- renderPlot({
    req(results())
    
    cost_data <- results()$working_final %>%
      filter(in_college) %>%
      select(tidyselect::all_of(c(cost_varvals, "year")))
    
    cost_data_long <- melt(cost_data, id.vars = "year", measure.vars = cost_varvals)
    
    # Map variable names to readable labels
    cost_data_long$variable <- factor(cost_data_long$variable,
                                      levels = cost_varvals,
                                      labels = cost_varlabs)
    
    ggplot(cost_data_long, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3") +
      labs(x = "Year", y = "Amount ($)", fill = "Category") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Yearly cost breakdown
  output$yearly_cost_breakdown <- renderPlot({
    req(results())
    
    cost_data <- results()$working_final %>%
      filter(in_college) %>%
      select(year, tidyselect::all_of(cost_varvals))
    
    # Calculate primary and secondary costs per year
    cost_data <- cost_data %>%
      mutate(
        primary_costs = rowSums(across(all_of(primary_cost_dictionary))),
        secondary_costs = rowSums(across(all_of(secondary_cost_dictionary)))
      ) %>%
      select(year, primary_costs, secondary_costs)
    
    cost_data_long <- melt(cost_data, id.vars = "year", 
                           measure.vars = c("primary_costs", "secondary_costs"))
    
    cost_data_long$variable <- factor(cost_data_long$variable,
                                      levels = c("primary_costs", "secondary_costs"),
                                      labels = c("Primary Costs", "Secondary Costs"))
    
    ggplot(cost_data_long, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Primary Costs" = "#e74c3c", "Secondary Costs" = "#f39c12")) +
      labs(x = "Year", y = "Amount ($)", fill = "Category") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Yearly contributions plot
  output$yearly_contributions <- renderPlot({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, primary_contribution, secondary_contribution)
    
    investment_data_long <- melt(investment_data, id.vars = "year", 
                                 measure.vars = c("primary_contribution", "secondary_contribution"))
    
    investment_data_long$variable <- factor(investment_data_long$variable,
                                            levels = c("primary_contribution", "secondary_contribution"),
                                            labels = c("Primary Fund", "Secondary Fund"))
    
    ggplot(investment_data_long, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Primary Fund" = "#3498db", "Secondary Fund" = "#9b59b6")) +
      labs(x = "Year", y = "Amount ($)", fill = "Fund") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # College costs table
  output$college_costs_table <- renderDT({
    req(results())
    
    cost_data <- results()$working_final %>%
      filter(in_college) %>%
      select(year, age, tidyselect::all_of(cost_dictionary))
    
    # Rename columns for display
    names(cost_data) <- c(cost_varlabs, "Total")
    
    datatable(cost_data, options = list(
      pageLength = 5,
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c(cost_varlabs, "Total"))
  })
  
  # Cost details table
  output$cost_details_table <- renderDT({
    req(results())
    
    cost_data <- results()$working_final %>%
      filter(in_college) %>%
      select(year, age, tidyselect::all_of(cost_dictionary))
    
    # Rename columns for display
    names(cost_data) <- c(cost_varlabs, "Total")
    
    datatable(cost_data, options = list(
      pageLength = nrow(cost_data),
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c(cost_varlabs, "Total"))
  })
  
  # Cost category breakdown chart
  output$cost_category_breakdown <- renderPlot({
    req(results())
    
    cost_data <- results()$working_final %>%
      filter(in_college) %>%
      select(tidyselect::all_of(cost_varvals))
    
    # Sum costs by category
    cost_summary <- data.frame(
      Category = cost_varlabs,
      Amount = c(
        sum(cost_data$cost_tuition),
        sum(cost_data$cost_fees),
        sum(cost_data$cost_housing),
        sum(cost_data$cost_meals),
        sum(cost_data$cost_books),
        sum(cost_data$cost_transport),
        sum(cost_data$cost_personal)
      )
    )
    
    # Add type (primary or secondary)
    cost_summary$Type <- c(rep("Primary", 5), rep("Secondary", 2))
    
    # Calculate percentages
    cost_summary$Percentage <- cost_summary$Amount / sum(cost_summary$Amount) * 100
    
    # Create pie chart
    ggplot(cost_summary, aes(x = "", y = Amount, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Total College Cost Breakdown", x = NULL, y = NULL, fill = "Category") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank()) +
      geom_text(aes(label = paste0(Category, "\n$", format(round(Amount), big.mark = ","))), 
                position = position_stack(vjust = 0.5))
  })
  
  # Investment details table
  output$investment_details_table <- renderDT({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, age, primary_contribution, secondary_contribution, total_contribution,
             primary_growth, secondary_growth, total_growth,
             primary_balance, secondary_balance, total_balance)
    
    # Rename columns for display
    names(investment_data) <- c("Year", "Age", 
                                "Primary Contribution", "Secondary Contribution", "Total Contribution",
                                "Primary Growth", "Secondary Growth", "Total Growth",
                                "Primary Balance", "Secondary Balance", "Total Balance")
    
    datatable(investment_data, options = list(
      pageLength = nrow(investment_data),
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c("Primary Contribution", "Secondary Contribution", "Total Contribution",
                                 "Primary Growth", "Secondary Growth", "Total Growth",
                                 "Primary Balance", "Secondary Balance", "Total Balance"))
  })
  
  # Investment contribution table
  output$investment_contribution_table <- renderDT({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, age, primary_contribution, secondary_contribution, total_contribution)
    
    # Calculate cumulative sums
    investment_data <- investment_data %>%
      mutate(
        cumulative_primary = cumsum(primary_contribution),
        cumulative_secondary = cumsum(secondary_contribution),
        cumulative_total = cumsum(total_contribution)
      )
    
    # Rename columns for display
    names(investment_data) <- c("Year", "Age", 
                                "Primary Annual", "Secondary Annual", "Total Annual",
                                "Primary Cumulative", "Secondary Cumulative", "Total Cumulative")
    
    datatable(investment_data, options = list(
      pageLength = nrow(investment_data),
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c("Primary Annual", "Secondary Annual", "Total Annual",
                                 "Primary Cumulative", "Secondary Cumulative", "Total Cumulative"))
  })
  
  # Annual contribution chart
  output$annual_contribution_chart <- renderPlot({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, primary_contribution, secondary_contribution)
    
    # Calculate cumulative sums
    investment_data <- investment_data %>%
      mutate(
        cumulative_primary = cumsum(primary_contribution),
        cumulative_secondary = cumsum(secondary_contribution)
      )
    
    # Prepare data for plot
    cumulative_data <- investment_data %>%
      select(year, cumulative_primary, cumulative_secondary) %>%
      rename(
        "Primary Fund" = cumulative_primary,
        "Secondary Fund" = cumulative_secondary
      ) %>%
      melt(id.vars = "year")
    
    annual_data <- investment_data %>%
      select(year, primary_contribution, secondary_contribution) %>%
      rename(
        "Primary Fund" = primary_contribution,
        "Secondary Fund" = secondary_contribution
      ) %>%
      melt(id.vars = "year")
    
    # Create two plots
    p1 <- ggplot(annual_data, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Primary Fund" = "#3498db", "Secondary Fund" = "#9b59b6")) +
      labs(title = "Annual Contributions", x = "Year", y = "Amount ($)", fill = "Fund") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    p2 <- ggplot(cumulative_data, aes(x = year, y = value, fill = variable)) +
      geom_area(alpha = 0.7) +
      scale_fill_manual(values = c("Primary Fund" = "#3498db", "Secondary Fund" = "#9b59b6")) +
      labs(title = "Cumulative Contributions", x = "Year", y = "Amount ($)", fill = "Fund") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Arrange both plots
    grid.arrange(p1, p2, ncol = 2)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
