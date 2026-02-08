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
                box(
                  title = "Investment Parameters", status = "primary", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(3,
                           numericInput("years_of_college", "Years of College", value = 4, min = 1, max = 10, step = 1)
                    ),
                    column(3,
                           numericInput("age_initial", "Age at Start of Investing", 
                                        value = 0, min = 0, max = 17, step = 1)
                    ),
                    column(3,
                           numericInput("age_college_start", 
                                        "Age at Start of Higher Education", value = 18, min = 16, max = 25, step = 1)
                    ),
                    column(3,
                           numericInput("year_initial", "Year at Start of Investing", 
                                        value = 2025, min = 2025, max = 2050, step = 1)
                    )
                  ),
                  fluidRow(
                    column(4,
                           radioButtons("inflation_method", "Inflation Method",
                                        choices = c("Historical (from entered costs)" = "historical",
                                                    "Fixed Rate" = "fixed"),
                                        selected = "historical", inline = TRUE),
                           conditionalPanel(
                             condition = "input.inflation_method == 'fixed'",
                             numericInput("pctpoint_inflation", 
                                          "Expected Annual Education Inflation (%)", 
                                          value = 4, min = 0, max = 10, step = 0.1)
                           ),
                           conditionalPanel(
                             condition = "input.inflation_method == 'historical'",
                             uiOutput("historical_inflation_display")
                           )
                    ),
                    column(4,
                           numericInput("pctpoint_principal", 
                                        "Annual Increase in Contributions (%)", 
                                        value = 4, min = 0, max = 10, step = 0.1)
                    ),
                    column(4,
                           numericInput("pctpoint_primary", 
                                        "Expected Growth - Primary Fund (%)", 
                                        value = 7, min = 0, max = 15, step = 0.1)
                    )
                  ),
                  fluidRow(
                    column(4,
                           numericInput("pctpoint_secondary", 
                                        "Expected Growth - Secondary Fund (%)", 
                                        value = 10, min = 0, max = 15, step = 0.1)
                    ),
                    column(8,
                           actionButton("calculate", "Calculate Required Investment", 
                                        class = "btn-primary", width = "100%")
                    )
                  )
                )
              ),
              
              # Costs data entry panel
              fluidRow(
                box(
                  title = "College Cost Data Entry", status = "warning", solidHeader = TRUE,
                  width = 12,
                  p("Enter known costs for future college expenses. Additional rows will be automatically added as needed."),
                  DTOutput("costs_table"),
                  actionButton("add_year", "Add Another Year", class = "btn-success")
                )
              ),
              
              # Summary metrics panel
              fluidRow(
                valueBoxOutput("total_cost_box", width = 4),
                valueBoxOutput("primary_principal_box", width = 4),
                valueBoxOutput("secondary_principal_box", width = 4)
              ),
              
              # Results panel with tabs
              fluidRow(
                tabBox(
                  title = "Results", 
                  id = "results_tabs",
                  width = 12,
                  tabPanel(
                    "Summary",
                    fluidRow(
                      column(4, 
                             box(
                               title = "Summary of Projections", width = NULL, status = "primary",
                               verbatimTextOutput("summary_text")
                             )
                      ),
                      column(8,
                             box(
                               title = "Investment Growth Projection", width = NULL, status = "info",
                               plotOutput("investment_growth_plot", height = "300px")
                             )
                      )
                    ),
                    fluidRow(
                      box(
                        title = "College Cost Projection", width = 12, status = "warning",
                        plotOutput("cost_breakdown_plot", height = "300px")
                      )
                    )
                  ),
                  tabPanel(
                    "Projections",
                    fluidRow(
                      box(
                        title = "Cost Projections", width = 6, status = "warning",
                        plotOutput("yearly_cost_breakdown", height = "300px")
                      ),
                      box(
                        title = "Investment Growth", width = 6, status = "info",
                        plotOutput("yearly_contributions", height = "300px")
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Projected College Costs by Year", width = 12, status = "warning",
                        DTOutput("college_costs_table")
                      )
                    )
                  )
                )
              )
      ),
      
      # Cost details tab
      tabItem(tabName = "costdetails",
              fluidRow(
                box(
                  title = "College Cost Details", width = 12, status = "warning",
                  p("Detailed breakdown of estimated college costs by year and category."),
                  DTOutput("cost_details_table")
                )
              ),
              fluidRow(
                box(
                  title = "Cost Breakdown", width = 12, status = "warning",
                  plotOutput("cost_category_breakdown", height = "400px")
                )
              )
      ),
      
      # Investment details tab
      tabItem(tabName = "investmentdetails",
              fluidRow(
                box(
                  title = "Investment Growth Details", width = 12, status = "info",
                  p("Detailed breakdown of investment growth by year."),
                  DTOutput("investment_details_table")
                )
              ),
              fluidRow(
                box(
                  title = "Investment Allocations by Year", width = 12, status = "info",
                  DTOutput("investment_principals_table")
                )
              ),
              fluidRow(
                box(
                  title = "Annual Investment Allocations", width = 12, status = "info",
                  plotOutput("annual_principals_chart", height = "400px")
                )
              )
      ),
      
      # About tab
      tabItem(tabName = "about",
              box(
                title = "About This Calculator", status = "info", solidHeader = TRUE,
                width = 12,
                p("This college fund calculator helps you determine how much you need to invest now to cover future college expenses."),
                p("The calculator takes into account:"),
                tags$ul(
                  tags$li("Actual and projected college costs including tuition, fees, housing, meals, books, transportation, and personal expenses"),
                  tags$li("Expected inflation rates for higher education"),
                  tags$li("Investment growth rates for different investment funds"),
                  tags$li("Annual increases in your contributions")
                ),
                p("Key Concepts:"),
                tags$ul(
                  tags$li(strong("Primary Expenses:"), "Tuition, fees, housing, meals, and books"),
                  tags$li(strong("Secondary Expenses:"), "Transportation and personal expenses"),
                  tags$li(strong("Primary Fund:"), "Typically a more conservative investment approach for essential expenses"),
                  tags$li(strong("Secondary Fund:"), "Often allows for more aggressive growth strategy for discretionary expenses")
                ),
                p("Inflation Methods:"),
                tags$ul(
                  tags$li(strong("Historical Inflation:"), "Calculated as the compound annual growth rate (CAGR) from the cost data you enter. Requires at least 2 years of data. Formula: ((ending cost / starting cost) ^ (1 / years)) - 1"),
                  tags$li(strong("Fixed Rate:"), "A manually specified annual inflation rate applied uniformly to project future costs")
                ),
                p("Instructions:"),
                tags$ol(
                  tags$li("Fill in the parameters in the 'Investment Parameters' section"),
                  tags$li("Choose an inflation method: Historical (default) uses your entered cost data; Fixed Rate uses a manually specified percentage"),
                  tags$li("Enter any known college costs in the 'College Cost Data Entry' table"),
                  tags$li("Click 'Calculate Required Investment' to see the results"),
                  tags$li("Review the summary tab for an overview of projected costs and required investments"),
                  tags$li("Explore the projections tab for detailed year-by-year breakdowns"),
                  tags$li("Use the navigation menu to view detailed cost and investment information")
                )
              )
      )
    )
  )
)

# Server logic 
server <- function(input, output, session) {
  
  if (FALSE) {
    
    # For troubleshooting
    input <- list(
      years_of_college = 4, 
      age_initial = 0, 
      age_college_start = 18, 
      year_initial = 2025, 
      inflation_method = "historical",
      pctpoint_inflation = 4, 
      pctpoint_principal = 4, 
      pctpoint_primary = 7, 
      pctpoint_secondary = 10
    )
    
    # For troubleshooting
    costs_df <- function(costs_entered = NULL) {
      if (!is.null(costs_entered)) {
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
  
  # Create a lookup table to match cost_values to cost_labels
  # Define the cost_values and cost_labels globally to ensure consistency
  category_lookup <- c(
    "Tuition" = "cost_tuition", 
    "Fees" = "cost_fees", 
    "Housing" = "cost_housing", 
    "Meals" = "cost_meals", 
    "Books" = "cost_books", 
    "Transportation" = "cost_transport", 
    "Personal" = "cost_personal"
  )
  
  cost_labels <- names(category_lookup)
  cost_values <- unname(category_lookup)
  
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
    req(input$year_initial)
    req(input$age_initial)
    
    costs_entered <- costs_df()
    years_needed <- nrow(costs_entered)
    
    # Update years and ages
    start_year <- input$year_initial
    start_age <- input$age_initial
    
    new_years <- seq(from = start_year, length.out = years_needed)
    new_ages <- seq(from = start_age, length.out = years_needed)
    
    costs_entered$year <- new_years
    costs_entered$age <- new_ages
    
    costs_df(costs_entered)
  })
  
  # --------------------------------------------------------------------------
  # Historical inflation calculation (CAGR from entered cost data)
  # --------------------------------------------------------------------------
  calculated_inflation <- reactive({
    costs_entered <- costs_df()
    
    # Need at least 2 rows to compute historical inflation
    if (nrow(costs_entered) < 2) {
      return(NULL)
    }
    
    # Compute total cost of attendance for each row
    total_costs <- rowSums(costs_entered[, cost_values, drop = FALSE])
    
    starting_cost <- total_costs[1]
    ending_cost   <- total_costs[length(total_costs)]
    n_years       <- nrow(costs_entered) - 1
    
    # Guard against zero or negative starting cost
    if (is.na(starting_cost) || starting_cost <= 0) {
      return(NULL)
    }
    
    # CAGR formula: ((ending / starting) ^ (1/n)) - 1
    cagr <- (ending_cost / starting_cost) ^ (1 / n_years) - 1
    return(cagr)
  })
  
  # --------------------------------------------------------------------------
  # Effective inflation rate: resolves which rate to actually use
  # --------------------------------------------------------------------------
  effective_inflation <- reactive({
    if (input$inflation_method == "historical") {
      hist_rate <- calculated_inflation()
      if (!is.null(hist_rate)) {
        return(hist_rate)
      } else {
        # Fall back to fixed rate when historical is unavailable
        return(input$pctpoint_inflation / 100)
      }
    } else {
      return(input$pctpoint_inflation / 100)
    }
  })
  
  # --------------------------------------------------------------------------
  # UI output: display the historical inflation rate or fallback message
  # --------------------------------------------------------------------------
  output$historical_inflation_display <- renderUI({
    hist_rate <- calculated_inflation()
    costs_entered <- costs_df()
    n_rows <- nrow(costs_entered)
    
    if (!is.null(hist_rate)) {
      tags$div(
        style = "padding: 8px 12px; background-color: #d5edda; border: 1px solid #c3e6cb; border-radius: 4px; margin-top: 5px;",
        tags$strong(
          style = "color: #155724;",
          icon("chart-line"),
          sprintf(" Historical inflation rate: %.1f%%", hist_rate * 100)
        ),
        tags$br(),
        tags$small(
          style = "color: #155724;",
          sprintf("Computed as CAGR from %d year(s) of entered cost data", n_rows)
        )
      )
    } else {
      tags$div(
        style = "padding: 8px 12px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; margin-top: 5px;",
        tags$strong(
          style = "color: #856404;",
          icon("exclamation-triangle"),
          sprintf(" Not enough data \u2014 using fixed rate: %.1f%%", input$pctpoint_inflation)
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
    costs_entered <- costs_df()
    costs_entered[info$row, info$col] <- info$value
    costs_df(costs_entered)
  })
  
  # Add a new year to the costs table
  observeEvent(input$add_year, {
    costs_entered <- costs_df()
    last_row <- nrow(costs_entered)
    new_row <- costs_entered[last_row, ]
    
    # Increment year and age
    new_row$year <- new_row$year + 1
    new_row$age <- new_row$age + 1
    
    # Apply inflation to costs (use effective inflation for the new row projection)
    inflation_factor <- 1 + effective_inflation()
    for(col in cost_values) {
      new_row[[col]] <- new_row[[col]] * inflation_factor
    }
    
    costs_df(rbind(costs_entered, new_row))
  })
  
  # Calculate results when the button is clicked
  results <- eventReactive(input$calculate, {
    
    # Define cost types
    cost_type_primary <- c("cost_tuition", "cost_fees", "cost_housing", "cost_meals", "cost_books")
    cost_type_secondary <- c("cost_transport", "cost_personal")
    
    # Get inputs
    years_of_college <- input$years_of_college
    age_initial <- input$age_initial
    age_college_start <- input$age_college_start
    year_initial <- input$year_initial
    percent_inflation <- effective_inflation()
    percent_principal <- input$pctpoint_principal / 100
    percent_primary <- input$pctpoint_primary / 100
    percent_secondary <- input$pctpoint_secondary / 100
    
    # Determine which inflation method was actually used
    inflation_method_used <- input$inflation_method
    hist_rate <- calculated_inflation()
    if (inflation_method_used == "historical" && is.null(hist_rate)) {
      inflation_method_used <- "fixed (fallback)"
    }
    
    # Notify user if historical was requested but fell back to fixed
    if (input$inflation_method == "historical" && is.null(hist_rate)) {
      showNotification(
        "Historical inflation unavailable (need 2+ years of cost data). Using fixed rate instead.",
        type = "warning",
        duration = 8
      )
    }
    
    # Extract costs from the table
    costs_entered <- costs_df()
    
    # Create named lists for each cost type
    costs_actual <- costs_entered
    
    # Calculate age and year values
    age_at_end_of_higher_education <- age_college_start + years_of_college - 1
    year_at_end_of_higher_education <- year_initial + (age_at_end_of_higher_education - age_initial)
    
    # Calculate age and year sequences
    ages_eligible <- seq(from = age_initial, to = age_at_end_of_higher_education)
    years_eligible <- seq(from = year_initial, to = year_at_end_of_higher_education)
    years_during_college <- years_eligible[(length(years_eligible) - years_of_college + 1):length(years_eligible)]
    college_start_year <- min(years_during_college)
    college_end_year <- max(years_during_college)
    
    years_to_invest <- length(years_eligible) - years_of_college
    total_years_investing <- years_to_invest + 1
    
    # Working data frame
    working_00 <- tibble::tibble(
      year = years_eligible, 
      age = ages_eligible
    ) %>% 
      dplyr::mutate(
        years_of_college = years_of_college,
        age_initial = age_initial,
        age_college_start = age_college_start,
        year_initial = year_initial,
        age_at_end_of_higher_education = age_at_end_of_higher_education,
        year_at_end_of_higher_education = year_at_end_of_higher_education,
        college_start_year = college_start_year,
        college_end_year = college_end_year,
        total_years_investing = total_years_investing
      )
    
    working_01 <- working_00 %>% 
      # Indicate if in in_college during eligible range of years/ages
      dplyr::mutate(in_college = years_eligible %in% years_during_college) %>% 
      # Join costs_actual
      dplyr::left_join(costs_actual) %>% 
      dplyr::mutate(cost_primary = rowSums(select({.}, tidyselect::all_of(cost_type_primary)))) %>% 
      dplyr::mutate(cost_secondary = rowSums(select({.}, tidyselect::all_of(cost_type_secondary)))) %>% 
      dplyr::mutate(cost_total = cost_primary + cost_secondary) %>% 
      # Add constants
      dplyr::mutate(percent_inflation = percent_inflation) %>% 
      dplyr::mutate(percent_principal = percent_principal) %>% 
      dplyr::mutate(percent_primary = percent_primary) %>% 
      dplyr::mutate(percent_secondary = percent_secondary) %>% 
      # Add and update multipliers 
      dplyr::mutate(multiplier_inflation = 1 + percent_inflation) %>% 
      dplyr::mutate(multiplier_principal = 1 + percent_principal) %>% 
      dplyr::mutate(multiplier_primary = 1 + percent_primary) %>% 
      dplyr::mutate(multiplier_secondary = 1 + percent_secondary) %>% 
      # Update with is.na
      dplyr::mutate(multiplier_inflation = ifelse(!is.na(cost_tuition), 1, multiplier_inflation)) %>% 
      # Update with row_number
      dplyr::mutate(multiplier_primary = ifelse(dplyr::row_number() == 1, 1, multiplier_primary)) %>% 
      dplyr::mutate(multiplier_principal = ifelse(dplyr::row_number() == 1, 1, multiplier_principal)) %>% 
      dplyr::mutate(multiplier_secondary = ifelse(dplyr::row_number() == 1, 1, multiplier_secondary)) %>% 
      # Calculate cumprod to facilitate calculation of inflated values
      dplyr::mutate(cumprod_inflation = round(cumprod(multiplier_inflation), 6)) %>% 
      dplyr::mutate(cumprod_principal = round(cumprod(multiplier_principal), 6)) %>% 
      dplyr::mutate(cumprod_primary = round(cumprod(multiplier_primary), 6)) %>%
      dplyr::mutate(cumprod_secondary = round(cumprod(multiplier_secondary), 6)) %>%
      # Fill (down) columns that start with "cost_" and then calculate remaining inflated values
      tidyr::fill(tidyselect::starts_with("cost_"), .direction = "down") %>% 
      dplyr::mutate(dplyr::across(tidyselect::starts_with("cost_"), ~ cumprod_inflation * .x)) %>% 
      # Calculate cumprod_growth
      dplyr::mutate(revprod_growth_primary = multiplier_primary ^ pmax(rev(dplyr::row_number()) - years_of_college, 0)) %>% 
      dplyr::mutate(revprod_growth_secondary = multiplier_secondary ^ pmax(rev(dplyr::row_number()) - years_of_college, 0))
    
    costs_college_each <- working_01 %>% 
      dplyr::filter(year %in% years_during_college) %>% 
      dplyr::select(tidyselect::all_of(cost_values)) %>% 
      colSums()
    
    # Calculate primary, secondary, total costs
    cost_college_primary <- working_01 %>% 
      dplyr::filter(year %in% years_during_college) %>% 
      dplyr::pull(cost_primary) %>% 
      sum()
    
    cost_college_secondary <- working_01 %>% 
      dplyr::filter(year %in% years_during_college) %>% 
      dplyr::pull(cost_secondary) %>% 
      sum()
    
    # Rename to clarify
    cost_college_total <- working_01 %>% 
      dplyr::filter(year %in% years_during_college) %>% 
      dplyr::pull(cost_total) %>% 
      sum()
    
    # Create a function that calculate investment growth
    calculate_growth <- function(
    df, 
    col_cumprod,
    initial_principal, 
    col_principal, 
    col_age, 
    col_age_college_start, 
    col_cost,
    col_growth_percent, 
    col_growth_amount, 
    col_balance_current
    ) {
      
      # One value in col
      age_college_start <- df[[col_age_college_start]]
      
      # Many values in col
      cumprod <- df[[col_cumprod]]
      age <- df[[col_age]]
      cost <- df[[col_cost]]
      
      growth_percent <- df[[col_growth_percent]]
      principal_amount <- ifelse(age < age_college_start, initial_principal * cumprod, 0)
      df[[col_principal]] <- principal_amount
      
      # Initialize vectors for calculated values
      growth_amount <- c(NULL)
      balance_current <- c(NULL)
      
      i <- NULL
      # Calculate fund growth and balance
      for (i in seq_len(nrow(df))) {
        
        contribution_current <- if (age[i] < age_college_start[i]) { principal_amount[i] } else { (-cost[i]) }
        # Get previous balance (0 for first year)
        balance_previous <- if (i > 1) { balance_current[i-1] } else { 0 }
        # Calculate fund growth based on formula
        growth_amount[i] <- (contribution_current + balance_previous) * growth_percent[i]
        # Update balance
        balance_current[i] <- balance_previous + contribution_current + growth_amount[i]
        
      }
      
      df[[col_growth_amount]] <- growth_amount
      df[[col_balance_current]] <- balance_current
      return(df)
    }
    
    if (FALSE) {
      col_cumprod = "cumprod_principal"
      initial_principal = initial_principal_primary
      col_principal = "principal_primary"
      col_age = "age"
      col_age_college_start = "age_college_start"
      col_cost = "cost_primary"
      col_growth_percent = "percent_primary"
      col_growth_amount = "growth_primary"
      col_balance_current = "balance_primary"
    }
    
    {
      
      # Calculate required initial investment for expenses
      initial_principal_primary <- 1
      fund_value_primary <- 0
      df_loop <- working_01
      
      while ((cost_college_primary - fund_value_primary) > 0) {
        df_loop <- calculate_growth(
          df = df_loop, 
          col_cumprod = "cumprod_principal",
          initial_principal = initial_principal_primary, 
          col_principal = "principal_primary", 
          col_age = "age", 
          col_age_college_start = "age_college_start", 
          col_cost = "cost_primary",
          col_growth_percent = "percent_primary",
          col_growth_amount = "growth_primary",
          col_balance_current = "balance_primary"
        )
        
        fund_value_primary <- max(df_loop[["balance_primary"]])
        
        if (!(cost_college_primary - fund_value_primary) > 0) {
          break
        }
        
        initial_principal_primary <- initial_principal_primary + 1
      }
      
      print(initial_principal_primary)
      
    }
    
    {
      
      # Calculate required initial investment for expenses
      initial_principal_secondary <- 1
      fund_value_secondary <- 0
      df_loop <- working_01
      
      while ((cost_college_secondary - fund_value_secondary) > 0) {
        df_loop <- calculate_growth(
          df = df_loop, 
          col_cumprod = "cumprod_principal",
          initial_principal = initial_principal_secondary, 
          col_principal = "principal_secondary", 
          col_age = "age", 
          col_age_college_start = "age_college_start", 
          col_cost = "cost_secondary",
          col_growth_percent = "percent_secondary",
          col_growth_amount = "growth_secondary",
          col_balance_current = "balance_secondary"
        )
        
        fund_value_secondary <- max(df_loop[["balance_secondary"]])
        
        if (!(cost_college_secondary - fund_value_secondary) > 0) {
          break
        }
        
        initial_principal_secondary <- initial_principal_secondary + 1
      }
      
      print(initial_principal_secondary)
      
    }
    
    working_02 <- working_01 %>% 
      calculate_growth(
        col_cumprod = "cumprod_principal",
        initial_principal = initial_principal_primary, 
        col_principal = "principal_primary", 
        col_age = "age", 
        col_age_college_start = "age_college_start", 
        col_cost = "cost_primary",
        col_growth_percent = "percent_primary",
        col_growth_amount = "growth_primary",
        col_balance_current = "balance_primary"
      ) %>% 
      calculate_growth(
        col_cumprod = "cumprod_principal",
        initial_principal = initial_principal_secondary, 
        col_principal = "principal_secondary", 
        col_age = "age", 
        col_age_college_start = "age_college_start", 
        col_cost = "cost_secondary",
        col_growth_percent = "percent_secondary",
        col_growth_amount = "growth_secondary",
        col_balance_current = "balance_secondary"
      ) %>% 
      dplyr::mutate(principal_total = principal_primary + principal_secondary) %>% 
      dplyr::mutate(growth_total = growth_primary + growth_secondary) %>% 
      dplyr::mutate(balance_total = balance_primary + balance_secondary) 
    
    working_final <- working_02
    
    # Return a list of all calculated values - make sure this is COMPLETE
    return(list(
      working_final = working_final,
      cost_college_total = cost_college_total,
      cost_college_primary = cost_college_primary,
      cost_college_secondary = cost_college_secondary,
      initial_principal_primary = initial_principal_primary,
      initial_principal_secondary = initial_principal_secondary,
      college_start_year = college_start_year,
      college_end_year = college_end_year,
      years_eligible = years_eligible,
      years_during_college = years_during_college,
      costs_college_each = costs_college_each,
      inflation_rate_used = percent_inflation,
      inflation_method_used = inflation_method_used
    ))
    
    if (FALSE) {
      results <- function() {
        list(
          working_final = working_final,
          cost_college_total = cost_college_total,
          cost_college_primary = cost_college_primary,
          cost_college_secondary = cost_college_secondary,
          initial_principal_primary = initial_principal_primary,
          initial_principal_secondary = initial_principal_secondary,
          college_start_year = college_start_year,
          college_end_year = college_end_year,
          years_eligible = years_eligible,
          years_during_college = years_during_college,
          costs_college_each = costs_college_each
        )
      }
      r <- results
    }
    
  })
  
  # Value box outputs
  output$total_cost_box <- renderValueBox({
    req(results())
    valueBox(
      value = dollar(results()$cost_college_total),
      subtitle = "Total College Costs",
      icon = icon("university"),
      color = "red"
    )
  })
  
  output$primary_principal_box <- renderValueBox({
    req(results())
    valueBox(
      value = dollar(results()$initial_principal_primary),
      subtitle = "Initial Primary Investment",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$secondary_principal_box <- renderValueBox({
    req(results())
    valueBox(
      value = dollar(results()$initial_principal_secondary),
      subtitle = "Initial Secondary Investment",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  # Summary text output
  output$summary_text <- renderText({
    req(results())
    
    # Format the inflation method description
    inflation_desc <- if (results()$inflation_method_used == "historical") {
      sprintf("Historical (CAGR): %.1f%%", results()$inflation_rate_used * 100)
    } else if (results()$inflation_method_used == "fixed (fallback)") {
      sprintf("Fixed (fallback): %.1f%%", results()$inflation_rate_used * 100)
    } else {
      sprintf("Fixed: %.1f%%", results()$inflation_rate_used * 100)
    }
    
    paste0(
      "College Years: ", results()$college_start_year, " - ", results()$college_end_year, "\n",
      "Inflation Method: ", inflation_desc, "\n\n",
      "Total College Cost: ", dollar(results()$cost_college_total), "\n",
      "  - Primary Expenses: ", dollar(results()$cost_college_primary), "\n",
      "  - Secondary Expenses: ", dollar(results()$cost_college_secondary), "\n\n",
      "Required Investment:\n",
      "  - Primary Fund: ", dollar(results()$initial_principal_primary), "\n",
      "  - Secondary Fund: ", dollar(results()$initial_principal_secondary), "\n",
      "  - Total Initial: ", dollar(results()$initial_principal_primary + results()$initial_principal_secondary)
    )
  })
  
  # Investment growth plot
  output$investment_growth_plot <- renderPlot({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, principal_total, growth_total, balance_total)
    
    investment_data_long <- melt(investment_data, id.vars = "year", 
                                 measure.vars = c("principal_total", "growth_total"))
    
    ggplot(investment_data_long, aes(x = year, y = value, fill = variable)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("principal_total" = "#3498db", "growth_total" = "#2ecc71"),
                        labels = c("Principal", "Growth")) +
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
      select(year, cost_tuition, cost_fees, cost_housing, cost_meals, 
             cost_books, cost_transport, cost_personal)
    
    cost_data_long <- melt(cost_data, id.vars = "year", 
                           measure.vars = c("cost_tuition", "cost_fees", "cost_housing", 
                                            "cost_meals", "cost_books", "cost_transport", "cost_personal"))
    
    # Map variable names to readable labels
    cost_data_long$variable <- factor(cost_data_long$variable,
                                      levels = c("cost_tuition", "cost_fees", "cost_housing", 
                                                 "cost_meals", "cost_books", "cost_transport", "cost_personal"),
                                      labels = c("Tuition", "Fees", "Housing", 
                                                 "Meals", "Books", "Transportation", "Personal"))
    
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
      select(year, cost_tuition, cost_fees, cost_housing, cost_meals, 
             cost_books, cost_transport, cost_personal)
    
    # Calculate primary and secondary costs per year
    cost_data <- cost_data %>%
      mutate(
        primary_costs = cost_tuition + cost_fees + cost_housing + cost_meals + cost_books,
        secondary_costs = cost_transport + cost_personal
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
      select(year, principal_primary, principal_secondary)
    
    investment_data_long <- melt(investment_data, id.vars = "year", 
                                 measure.vars = c("principal_primary", "principal_secondary"))
    
    investment_data_long$variable <- factor(investment_data_long$variable,
                                            levels = c("principal_primary", "principal_secondary"),
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
      select(year, age, cost_tuition, cost_fees, cost_housing, cost_meals, 
             cost_books, cost_transport, cost_personal, cost_total)
    
    # Rename columns for display
    names(cost_data) <- c("Year", "Age", "Tuition", "Fees", "Housing", "Meals", 
                          "Books", "Transportation", "Personal", "Total")
    
    datatable(cost_data, options = list(
      pageLength = 5,
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c("Tuition", "Fees", "Housing", "Meals", 
                                 "Books", "Transportation", "Personal", "Total"))
  })
  
  # Cost details table
  output$cost_details_table <- renderDT({
    req(results())
    
    cost_data <- results()$working_final %>%
      filter(in_college) %>%
      select(year, age, cost_tuition, cost_fees, cost_housing, cost_meals, 
             cost_books, cost_transport, cost_personal, cost_total)
    
    # Rename columns for display
    names(cost_data) <- c("Year", "Age", "Tuition", "Fees", "Housing", "Meals", 
                          "Books", "Transportation", "Personal", "Total")
    
    datatable(cost_data, options = list(
      pageLength = 10,
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c("Tuition", "Fees", "Housing", "Meals", 
                                 "Books", "Transportation", "Personal", "Total"))
  })
  
  # Cost category breakdown chart
  output$cost_category_breakdown <- renderPlot({
    req(results())
    
    cost_data <- results()$working_final %>%
      filter(in_college) %>%
      select(cost_tuition, cost_fees, cost_housing, cost_meals, 
             cost_books, cost_transport, cost_personal)
    
    # Sum costs by category
    cost_summary <- data.frame(
      Category = c("Tuition", "Fees", "Housing", "Meals", "Books", "Transportation", "Personal"),
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
      select(year, age, principal_primary, principal_secondary, principal_total,
             growth_primary, growth_secondary, growth_total,
             balance_primary, balance_secondary, balance_total)
    
    # Rename columns for display
    names(investment_data) <- c("Year", "Age", 
                                "Primary Principal", "Secondary Principal", "Total Principal",
                                "Primary Growth", "Secondary Growth", "Total Growth",
                                "Primary Balance", "Secondary Balance", "Total Balance")
    
    datatable(investment_data, options = list(
      pageLength = 10,
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c("Primary Principal", "Secondary Principal", "Total Principal",
                                 "Primary Growth", "Secondary Growth", "Total Growth",
                                 "Primary Balance", "Secondary Balance", "Total Balance"))
  })
  
  # Investment principals table
  output$investment_principals_table <- renderDT({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, age, principal_primary, principal_secondary, principal_total)
    
    # Calculate cumulative sums
    investment_data <- investment_data %>%
      mutate(
        cumulative_primary = cumsum(principal_primary),
        cumulative_secondary = cumsum(principal_secondary),
        cumulative_total = cumsum(principal_total)
      )
    
    # Rename columns for display
    names(investment_data) <- c("Year", "Age", 
                                "Primary Annual", "Secondary Annual", "Total Annual",
                                "Primary Cumulative", "Secondary Cumulative", "Total Cumulative")
    
    datatable(investment_data, options = list(
      pageLength = 10,
      searching = FALSE,
      dom = 't',
      ordering = FALSE
    )) %>%
      formatCurrency(columns = c("Primary Annual", "Secondary Annual", "Total Annual",
                                 "Primary Cumulative", "Secondary Cumulative", "Total Cumulative"))
  })
  
  # Annual principals chart
  output$annual_principals_chart <- renderPlot({
    req(results())
    
    investment_data <- results()$working_final %>%
      filter(!in_college) %>%
      select(year, principal_primary, principal_secondary)
    
    # Calculate cumulative sums
    investment_data <- investment_data %>%
      mutate(
        cumulative_primary = cumsum(principal_primary),
        cumulative_secondary = cumsum(principal_secondary)
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
      select(year, principal_primary, principal_secondary) %>%
      rename(
        "Primary Fund" = principal_primary,
        "Secondary Fund" = principal_secondary
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
