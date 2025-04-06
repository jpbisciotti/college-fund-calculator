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
                           numericInput("age_at_start_of_investing", "Age at Start of Investing", 
                                        value = 0, min = 0, max = 17, step = 1)
                    ),
                    column(3,
                           numericInput("age_at_start_of_first_year_of_higher_education", 
                                        "Age at Start of Higher Education", value = 18, min = 16, max = 25, step = 1)
                    ),
                    column(3,
                           numericInput("year_at_start_of_investing", "Year at Start of Investing", 
                                        value = 2025, min = 2025, max = 2050, step = 1)
                    )
                  ),
                  fluidRow(
                    column(4,
                           numericInput("inflation_of_higher_education_expected_annual", 
                                        "Expected Annual Education Inflation (%)", 
                                        value = 4, min = 0, max = 10, step = 0.1)
                    ),
                    column(4,
                           numericInput("multiplier_to_increase_annual_allocation", 
                                        "Annual Increase in Contributions (%)", 
                                        value = 4, min = 0, max = 10, step = 0.1)
                    ),
                    column(4,
                           numericInput("growth_of_primary_investment_vehicle_expected_annual", 
                                        "Expected Growth - Primary Vehicle (%)", 
                                        value = 7, min = 0, max = 15, step = 0.1)
                    )
                  ),
                  fluidRow(
                    column(4,
                           numericInput("growth_of_secondary_investment_vehicle_expected_annual", 
                                        "Expected Growth - Secondary Vehicle (%)", 
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
                valueBoxOutput("primary_allocation_box", width = 4),
                valueBoxOutput("secondary_allocation_box", width = 4)
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
                  DTOutput("investment_allocations_table")
                )
              ),
              fluidRow(
                box(
                  title = "Annual Investment Allocations", width = 12, status = "info",
                  plotOutput("annual_allocations_chart", height = "400px")
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
                  tags$li("Investment growth rates for different investment vehicles"),
                  tags$li("Annual increases in your contributions")
                ),
                p("Key Concepts:"),
                tags$ul(
                  tags$li(strong("Primary Expenses:"), "Tuition, fees, housing, meals, and books"),
                  tags$li(strong("Secondary Expenses:"), "Transportation and personal expenses"),
                  tags$li(strong("Primary Investment Vehicle:"), "Typically a more conservative investment approach for essential expenses"),
                  tags$li(strong("Secondary Investment Vehicle:"), "Often allows for more aggressive growth strategy for discretionary expenses")
                ),
                p("Instructions:"),
                tags$ol(
                  tags$li("Fill in the parameters in the 'Investment Parameters' section"),
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
  # Define the cost types globally to ensure consistency
  cost_types <- c("tuition", "fees", "housing", "meals", "books", "transport", "personal")
  
  # Initial costs data frame
  costs_df <- reactiveVal(data.frame(
    year = c(2025, 2026),
    age = c(0, 1),
    tuition = c(10086, 10490),
    fees = c(1772, 1842),
    housing = c(9562, 9944),
    meals = c(6396, 6651),
    books = c(1250, 1300),
    transport = c(1514, 1574),
    personal = c(1200, 1248),
    stringsAsFactors = FALSE
  ))
  
  # Update year and age in costs table when parameters change
  observe({
    req(input$year_at_start_of_investing)
    req(input$age_at_start_of_investing)
    
    current_costs <- costs_df()
    years_needed <- nrow(current_costs)
    
    # Update years and ages
    start_year <- input$year_at_start_of_investing
    start_age <- input$age_at_start_of_investing
    
    new_years <- seq(from = start_year, length.out = years_needed)
    new_ages <- seq(from = start_age, length.out = years_needed)
    
    current_costs$year <- new_years
    current_costs$age <- new_ages
    
    costs_df(current_costs)
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
    current_costs <- costs_df()
    current_costs[info$row, info$col] <- info$value
    costs_df(current_costs)
  })
  
  # Add a new year to the costs table
  observeEvent(input$add_year, {
    current_costs <- costs_df()
    last_row <- nrow(current_costs)
    new_row <- current_costs[last_row, ]
    
    # Increment year and age
    new_row$year <- new_row$year + 1
    new_row$age <- new_row$age + 1
    
    # Apply inflation to costs
    inflation_factor <- 1 + (input$inflation_of_higher_education_expected_annual / 100)
    for(col in cost_types) {
      new_row[[col]] <- new_row[[col]] * inflation_factor
    }
    
    costs_df(rbind(current_costs, new_row))
  })
  
  # Calculate results when the button is clicked
  results <- eventReactive(input$calculate, {
    # Get inputs
    years_of_college <- input$years_of_college
    age_at_start_of_investing <- input$age_at_start_of_investing
    age_at_start_of_first_year_of_higher_education <- input$age_at_start_of_first_year_of_higher_education
    year_at_start_of_investing <- input$year_at_start_of_investing
    inflation_of_higher_education_expected_annual <- input$inflation_of_higher_education_expected_annual / 100
    multiplier_to_increase_annual_allocation <- input$multiplier_to_increase_annual_allocation / 100
    growth_of_primary_investment_vehicle_expected_annual <- input$growth_of_primary_investment_vehicle_expected_annual / 100
    growth_of_secondary_investment_vehicle_expected_annual <- input$growth_of_secondary_investment_vehicle_expected_annual / 100
    
    # Extract costs from the table
    current_costs <- costs_df()
    
    # Create named lists for each cost type
    costs_actual <- list()
    
    for(cost_type in cost_types) {
      costs_actual[[cost_type]] <- setNames(current_costs[[cost_type]], current_costs$year)
    }
    
    # Calculate age and year sequences
    age_at_start_of_last_year_of_higher_education <- age_at_start_of_first_year_of_higher_education + years_of_college - 1
    age <- seq(from = age_at_start_of_investing, to = age_at_start_of_last_year_of_higher_education)
    year_at_start_of_higher_education <- year_at_start_of_investing + (age_at_start_of_first_year_of_higher_education - age_at_start_of_investing)
    year_at_end_of_higher_education <- year_at_start_of_investing + (age_at_start_of_last_year_of_higher_education - age_at_start_of_investing)
    year <- seq(from = year_at_start_of_investing, to = year_at_end_of_higher_education)
    years_during_college <- year[(length(year) - years_of_college + 1):length(year)]
    
    # Get the latest actual costs for each type
    cost_actual_latest <- list()
    for(cost_type in cost_types) {
      cost_actual_latest[[cost_type]] <- unname(costs_actual[[cost_type]][length(costs_actual[[cost_type]])])
    }
    
    # Estimate costs for remaining years
    costs_est_to_complete <- list()
    for(cost_type in cost_types) {
      years_to_estimate <- setdiff(as.character(year), names(costs_actual[[cost_type]]))
      if(length(years_to_estimate) > 0) {
        num_years_to_estimate <- length(years_to_estimate)
        inflation_factors <- cumprod(rep((1 + inflation_of_higher_education_expected_annual), num_years_to_estimate))
        costs_est_to_complete[[cost_type]] <- setNames(
          inflation_factors * cost_actual_latest[[cost_type]],
          years_to_estimate
        )
      } else {
        costs_est_to_complete[[cost_type]] <- numeric(0)
      }
    }
    
    # Combine actual and estimated costs
    costs_est_at_completion <- list()
    for(cost_type in cost_types) {
      costs_est_at_completion[[cost_type]] <- c(costs_actual[[cost_type]], costs_est_to_complete[[cost_type]])
    }
    
    # Extract costs for college years
    costs_college <- list()
    for(cost_type in cost_types) {
      costs_college[[cost_type]] <- costs_est_at_completion[[cost_type]][as.character(years_during_college)]
    }
    
    # Calculate total costs for each category
    cost_college <- list()
    for(cost_type in cost_types) {
      cost_college[[cost_type]] <- sum(costs_college[[cost_type]])
    }
    
    # Calculate primary and secondary costs
    cost_college_primary <- cost_college$tuition + cost_college$fees + 
      cost_college$housing + cost_college$meals + cost_college$books
    
    cost_college_secondary <- cost_college$transport + cost_college$personal
    
    cost_covered_by_investment_vehicle_primary <- cost_college_primary
    cost_covered_by_investment_vehicle_secondary <- cost_college_secondary
    cost_covered_by_investment_vehicle_total <- cost_covered_by_investment_vehicle_primary + 
      cost_covered_by_investment_vehicle_secondary
    
    # Calculate required initial investment for primary expenses
    allocation_into_primary_vehicle_in_initial_year <- 1
    primary_vehicle_value_total <- 0
    
    while ((cost_covered_by_investment_vehicle_primary - primary_vehicle_value_total) > 0) {
      years_to_invest <- length(year) - years_of_college
      multipliers_for_primary_vehicle <- c(1, cumprod(rep((1 + multiplier_to_increase_annual_allocation), years_to_invest)))
      allocations_into_primary_vehicle_each_year <- allocation_into_primary_vehicle_in_initial_year * multipliers_for_primary_vehicle
      
      years_of_growth <- rev(seq_len(years_to_invest + 1))
      allocations_growth_for_each_year <- (1 + growth_of_primary_investment_vehicle_expected_annual) ^ years_of_growth
      
      primary_vehicle_value_each_year <- allocations_into_primary_vehicle_each_year * allocations_growth_for_each_year
      primary_vehicle_value_total <- sum(primary_vehicle_value_each_year)
      
      if (!(cost_covered_by_investment_vehicle_primary - primary_vehicle_value_total) > 0) {
        break
      }
      
      allocation_into_primary_vehicle_in_initial_year <- allocation_into_primary_vehicle_in_initial_year + 1
    }
    
    # Calculate required initial investment for secondary expenses
    allocation_into_secondary_vehicle_in_initial_year <- 1
    secondary_vehicle_value_total <- 0
    
    while ((cost_covered_by_investment_vehicle_secondary - secondary_vehicle_value_total) > 0) {
      years_to_invest <- length(year) - years_of_college
      multipliers_for_secondary_vehicle <- c(1, cumprod(rep((1 + multiplier_to_increase_annual_allocation), years_to_invest)))
      allocations_into_secondary_vehicle_each_year <- allocation_into_secondary_vehicle_in_initial_year * multipliers_for_secondary_vehicle
      
      years_of_growth <- rev(seq_len(years_to_invest + 1))
      allocations_growth_for_each_year <- (1 + growth_of_secondary_investment_vehicle_expected_annual) ^ years_of_growth
      
      secondary_vehicle_value_each_year <- allocations_into_secondary_vehicle_each_year * allocations_growth_for_each_year
      secondary_vehicle_value_total <- sum(secondary_vehicle_value_each_year)
      
      if (!(cost_covered_by_investment_vehicle_secondary - secondary_vehicle_value_total) > 0) {
        break
      }
      
      allocation_into_secondary_vehicle_in_initial_year <- allocation_into_secondary_vehicle_in_initial_year + 1
    }
    
    # Prepare data for detailed tables and charts
    years_to_invest <- length(year) - years_of_college
    years_for_plot <- year[1:(years_to_invest + 1)]
    
    # Primary vehicle growth data
    primary_contributions <- allocation_into_primary_vehicle_in_initial_year * 
      c(1, cumprod(rep((1 + multiplier_to_increase_annual_allocation), years_to_invest)))
    names(primary_contributions) <- years_for_plot
    
    # Secondary vehicle growth data
    secondary_contributions <- allocation_into_secondary_vehicle_in_initial_year * 
      c(1, cumprod(rep((1 + multiplier_to_increase_annual_allocation), years_to_invest)))
    names(secondary_contributions) <- years_for_plot
    
    # Create investment projection data
    investment_projection <- data.frame(
      year = years_for_plot,
      age = age[1:(years_to_invest + 1)],
      primary_allocation = primary_contributions,
      secondary_allocation = secondary_contributions,
      total_annual_investment = primary_contributions + secondary_contributions
    )
    
    # Create investment growth table
    investment_growth <- data.frame(
      year = years_for_plot,
      age = age[1:(years_to_invest + 1)],
      primary_investment = 0,
      secondary_investment = 0,
      total_investment = 0,
      annual_contribution = primary_contributions + secondary_contributions
    )
    
    # Calculate cumulative investment growth
    primary_running_total <- 0
    secondary_running_total <- 0
    
    for(i in 1:nrow(investment_growth)) {
      # Calculate growth and add new contribution
      primary_running_total <- primary_running_total * (1 + growth_of_primary_investment_vehicle_expected_annual) + primary_contributions[i]
      secondary_running_total <- secondary_running_total * (1 + growth_of_secondary_investment_vehicle_expected_annual) + secondary_contributions[i]
      
      # Update the table
      investment_growth$primary_investment[i] <- primary_running_total
      investment_growth$secondary_investment[i] <- secondary_running_total
      investment_growth$total_investment[i] <- primary_running_total + secondary_running_total
    }
    
    # Create in_college flag for the cost projection table
    all_years_df <- data.frame(
      year = year,
      age = age,
      in_college = year %in% years_during_college
    )
    
    # Create complete costs table
    costs_table <- data.frame(
      year = integer(length(year)),
      age = integer(length(year))
    )
    
    for(i in 1:length(year)) {
      costs_table$year[i] <- year[i]
      costs_table$age[i] <- age[i]
    }
    
    for(cost_type in cost_types) {
      costs_table[[cost_type]] <- unname(costs_est_at_completion[[cost_type]])
    }
    
    # Add total and in_college columns
    costs_table$total <- rowSums(costs_table[, cost_types])
    costs_table$in_college <- costs_table$year %in% years_during_college
    
    # Create college years cost table
    college_costs_table <- costs_table[costs_table$in_college, ]
    
    # Calculate total costs and final investment value
    total_college_cost <- sum(college_costs_table$total)
    final_investment_value <- tail(investment_growth$total_investment, 1)
    projected_surplus <- final_investment_value - total_college_cost
    
    # Create primary vs secondary for plotting
    primary_secondary <- data.frame(
      category = c("Primary Expenses", "Secondary Expenses"),
      value = c(cost_college_primary, cost_college_secondary)
    )
    
    # Create cost breakdown for plotting 
    cost_breakdown <- data.frame(
      category = c("Tuition", "Fees", "Housing", "Meals", "Books", "Transportation", "Personal"),
      cost_type = cost_types, # Added this to make the mapping explicit
      value = c(
        cost_college$tuition,
        cost_college$fees,
        cost_college$housing,
        cost_college$meals,
        cost_college$books,
        cost_college$transport,
        cost_college$personal
      )
    )
    
    # Create data for yearly cost breakdown chart - using cost_types directly
    yearly_costs_long <- reshape2::melt(
      college_costs_table[, c("year", cost_types)], 
      id.vars = "year", 
      variable.name = "category", 
      value.name = "cost"
    )
    
    # Create a lookup table to match cost_types to display names
    category_lookup <- setNames(
      c("Tuition", "Fees", "Housing", "Meals", "Books", "Transportation", "Personal"),
      cost_types
    )
    
    # Add display names to the yearly costs data
    yearly_costs_long$display_category <- category_lookup[as.character(yearly_costs_long$category)]
    
    # Create data for yearly investment chart
    investment_long <- reshape2::melt(
      investment_projection[, c("year", "primary_allocation", "secondary_allocation")],
      id.vars = "year",
      variable.name = "vehicle",
      value.name = "amount"
    )
    investment_long$vehicle <- factor(investment_long$vehicle, 
                                      labels = c("Primary", "Secondary"))
    
    # Return all results
    return(list(
      investment_primary = allocation_into_primary_vehicle_in_initial_year,
      investment_secondary = allocation_into_secondary_vehicle_in_initial_year,
      total_cost = cost_covered_by_investment_vehicle_total,
      primary_cost = cost_college_primary,
      secondary_cost = cost_college_secondary,
      cost_breakdown = cost_breakdown,
      primary_secondary = primary_secondary,
      investment_growth = investment_growth,
      investment_projection = investment_projection,
      costs_table = costs_table,
      college_costs_table = college_costs_table,
      yearly_costs_long = yearly_costs_long,
      investment_long = investment_long,
      years_for_plot = years_for_plot,
      college_years = years_during_college,
      total_years_investing = years_to_invest + 1,
      college_start_year = min(years_during_college),
      college_end_year = max(years_during_college),
      total_college_cost = total_college_cost,
      final_investment_value = final_investment_value,
      projected_surplus = projected_surplus,
      surplus_percentage = (projected_surplus / total_college_cost) * 100,
      cost_types = cost_types,
      category_lookup = category_lookup
    ))
  })
  
  # Display value boxes
  output$total_cost_box <- renderValueBox({
    req(results())
    r <- results()
    valueBox(
      paste0("$", format(round(r$total_cost), big.mark=",")), 
      "Total College Cost",
      icon = icon("university"),
      color = "blue"
    )
  })
  
  output$primary_allocation_box <- renderValueBox({
    req(results())
    r <- results()
    valueBox(
      paste0("$", format(round(r$investment_primary), big.mark=",")), 
      "Initial Primary Allocation",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$secondary_allocation_box <- renderValueBox({
    req(results())
    r <- results()
    valueBox(
      paste0("$", format(round(r$investment_secondary), big.mark=",")), 
      "Initial Secondary Allocation",
      icon = icon("coins"),
      color = "purple"
    )
  })
  
  # Display summary text
  output$summary_text <- renderText({
    req(results())
    r <- results()
    
    paste0(
      "Total Years of Investing: ", r$total_years_investing, "\n",
      "College Start Year: ", r$college_start_year, " (Age ", input$age_at_start_of_first_year_of_higher_education, ")\n",
      "College End Year: ", r$college_end_year, "\n\n",
      "Total College Cost: $", format(round(r$total_college_cost), big.mark = ","), "\n",
      "Final Investment Value: $", format(round(r$final_investment_value, 1), big.mark = ","), "\n\n",
      "Projected Surplus: $", format(round(r$projected_surplus, 2), big.mark = ","), 
      " (", round(r$surplus_percentage, 1), "% more than needed)"
    )
  })
  
  # Display investment growth plot
  output$investment_growth_plot <- renderPlot({
    req(results())
    r <- results()
    
    # Prepare data for plotting
    growth_data <- r$investment_growth
    
    # Add a total line
    plot_data <- data.frame(
      year = rep(growth_data$year, 3),
      value = c(growth_data$primary_investment, growth_data$secondary_investment, growth_data$total_investment),
      type = factor(rep(c("Primary Investment", "Secondary Investment", "Total"), each = nrow(growth_data)),
                    levels = c("Primary Investment", "Secondary Investment", "Total"))
    )
    
    # Create vertical line for college start
    college_start <- min(r$college_years)
    
    ggplot(plot_data, aes(x = year, y = value, color = type, group = type)) +
      geom_line(size = 1.2) +
      theme_minimal() +
      labs(title = "Investment Growth Over Time", 
           x = "Year", 
           y = "Amount ($)", 
           color = "Investment Type") +
      scale_y_continuous(labels = dollar_format()) +
      scale_color_manual(values = c("Primary Investment" = "blue", 
                                    "Secondary Investment" = "green", 
                                    "Total" = "purple")) +
      geom_vline(xintercept = college_start, linetype = "dashed", color = "darkred") +
      annotate("text", x = college_start, y = max(plot_data$value) * 0.9, 
               label = "College Starts", hjust = -0.1, color = "darkred") +
      theme(legend.position = "right", legend.title = element_text(size = 10))
  })
  
  # Display cost breakdown plot
  output$cost_breakdown_plot <- renderPlot({
    req(results())
    r <- results()
    
    # Extract college years costs and prepare data directly
    college_data <- r$college_costs_table[, c("year", r$cost_types)]
    
    # Prepare the data for plotting
    plot_data <- reshape2::melt(college_data, id.vars = "year", variable.name = "category", value.name = "cost")
    
    # Add display names using the lookup table
    plot_data$display_category <- r$category_lookup[as.character(plot_data$category)]
    
    # Create stacked bar chart
    ggplot(plot_data, aes(x = factor(year), y = cost, fill = display_category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "College Costs by Year and Category", 
           x = "Year", 
           y = "Cost ($)", 
           fill = "Expense Category") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "right")
  })
  
  # Display yearly cost breakdown chart
  output$yearly_cost_breakdown <- renderPlot({
    req(results())
    r <- results()
    
    ggplot(r$yearly_costs_long, aes(x = factor(year), y = cost, fill = display_category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Projected College Costs by Year and Category", 
           x = "Year", 
           y = "Cost ($)", 
           fill = "Category") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 0))
  })
  
  # Display yearly contributions chart
  output$yearly_contributions <- renderPlot({
    req(results())
    r <- results()
    
    ggplot(r$investment_long, aes(x = factor(year), y = amount, fill = vehicle)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Annual Investment Allocations", 
           x = "Year", 
           y = "Amount ($)", 
           fill = "Vehicle") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Display annual allocations chart
  output$annual_allocations_chart <- renderPlot({
    req(results())
    r <- results()
    
    ggplot(r$investment_long, aes(x = factor(year), y = amount, fill = vehicle)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Annual Investment Allocations", 
           x = "Year", 
           y = "Amount ($)", 
           fill = "Vehicle") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Display category breakdown chart
  output$cost_category_breakdown <- renderPlot({
    req(results())
    r <- results()
    
    # Use the display names instead of cost types for plotting
    plot_data <- data.frame(
      category = r$cost_breakdown$category,
      value = r$cost_breakdown$value
    )
    
    ggplot(plot_data, aes(x = reorder(category, -value), y = value, fill = category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Expense Categories - Total Cost for All College Years", 
           x = "Category", 
           y = "Total Cost ($)") +
      scale_y_continuous(labels = dollar_format()) +
      scale_fill_brewer(palette = "Set3") +
      theme(legend.position = "none") +
      geom_text(aes(label = paste0("$", format(round(value), big.mark = ","))), 
                vjust = -0.5)
  })
  
  # Display college costs table
  output$college_costs_table <- renderDT({
    req(results())
    r <- results()
    
    # Format the table for display
    display_table <- r$college_costs_table[, c("year", r$cost_types)]
    
    # Format currency values
    for(col in r$cost_types) {
      display_table[[col]] <- paste0("$", format(round(display_table[[col]]), big.mark = ","))
    }
    
    # Rename columns with display names
    colnames(display_table)[-1] <- r$category_lookup[r$cost_types]
    
    datatable(
      display_table,
      options = list(
        pageLength = 5,
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  # Display cost details table
  output$cost_details_table <- renderDT({
    req(results())
    r <- results()
    
    # Format the table for display
    display_table <- r$costs_table[, c("year", "age", r$cost_types, "total", "in_college")]
    
    # Format currency values
    for(col in c(r$cost_types, "total")) {
      display_table[[col]] <- paste0("$", format(round(display_table[[col]]), big.mark = ","))
    }
    
    # Rename columns with display names
    new_colnames <- colnames(display_table)
    idx <- match(r$cost_types, new_colnames)
    new_colnames[idx] <- r$category_lookup[r$cost_types]
    colnames(display_table) <- new_colnames
    
    datatable(
      display_table,
      options = list(
        pageLength = 10,
        dom = 'ftip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # Display investment details table
  output$investment_details_table <- renderDT({
    req(results())
    r <- results()
    
    # Format the table for display
    display_table <- r$investment_growth
    
    # Format currency values
    cols_to_format <- c("primary_investment", "secondary_investment", "total_investment", "annual_contribution")
    for(col in cols_to_format) {
      display_table[[col]] <- paste0("$", format(round(display_table[[col]]), big.mark = ","))
    }
    
    # Rename columns for display
    colnames(display_table) <- c("Year", "Age", "Primary Investment", "Secondary Investment", 
                                 "Total Investment", "Annual Contribution")
    
    datatable(
      display_table,
      options = list(
        pageLength = 10,
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  # Display investment allocations table
  output$investment_allocations_table <- renderDT({
    req(results())
    r <- results()
    
    # Format the table for display
    display_table <- r$investment_projection[, c("year", "primary_allocation", "secondary_allocation", "total_annual_investment")]
    
    # Format currency values
    cols_to_format <- c("primary_allocation", "secondary_allocation", "total_annual_investment")
    for(col in cols_to_format) {
      display_table[[col]] <- paste0("$", format(round(display_table[[col]]), big.mark = ","))
    }
    
    # Rename columns for display
    colnames(display_table) <- c("Year", "Primary Allocation", "Secondary Allocation", "Total Annual Investment")
    
    datatable(
      display_table,
      options = list(
        pageLength = 5,
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  # Hide the result panels initially and show them after calculation
  observe({
    if(input$calculate == 0) {
      shinyjs::hide("total_cost_box")
      shinyjs::hide("primary_allocation_box")
      shinyjs::hide("secondary_allocation_box")
      shinyjs::hide("results_tabs")
    } else {
      shinyjs::show("total_cost_box")
      shinyjs::show("primary_allocation_box")
      shinyjs::show("secondary_allocation_box")
      shinyjs::show("results_tabs")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
