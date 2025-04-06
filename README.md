# College Fund Calculator

## About the Application

This R Shiny application helps users plan for higher education expenses through financial modeling. The calculator enables families to determine how much they need to invest to cover future college costs based on customizable parameters, inflation expectations, and investment growth rates.

## Features

- **Two Investment Approaches**: Allocates funds between primary and secondary investment vehicles
- **Cost Projection**: Uses educational inflation rates to estimate future expenses across seven cost categories
- **Data Entry**: Allows users to modify cost assumptions with automatic inflation adjustments
- **Visualization**: Presents financial data through charts and tables
- **Surplus Analysis**: Calculates expected investment surplus or deficit
- **Detailed Breakdowns**: Shows year-by-year analysis of investment growth and educational costs

## Technical Implementation

The application includes:

- **Reactive Programming**: Creates interdependent elements that update based on user inputs
- **Financial Modeling**: Applies time-series forecasting and compound growth calculations
- **Data Visualization**: Uses ggplot2, gridExtra, and other R visualization libraries
- **Dashboard Interface**: Built with shinydashboard for an organized, tab-based layout
- **Data Transformation**: Implements dplyr and reshape2 for data manipulation
- **Interactive Tables**: Uses the DT package for sortable and filterable data tables

## Financial Models

The application incorporates several financial models:

- **Educational Inflation Projection**: Applies annual growth rates to different expense categories
- **Investment Growth Calculation**: Projects investment value using compound growth formulas
- **Contribution Determination**: Calculates the minimum required initial investment
- **Cost Categorization**: Divides expenses into primary and secondary categories

## How to Use

1. Set basic parameters (years of college, starting age, inflation rate, etc.)
2. Enter known college costs or use the defaults
3. Click "Calculate Required Investment" to generate projections
4. Explore the results across different tabs
5. Adjust parameters to test different scenarios

## Implementation Details

The application demonstrates R programming techniques including:

- Reactive value management
- Event-driven calculation
- Dynamic UI generation
- Financial calculations
- Data transformation
- Various visualization methods
