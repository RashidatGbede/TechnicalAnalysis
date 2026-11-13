
# Small Business Technical Analysis - Stock Data Output

# Load the functions created in the main script
source("stock_analysis_functions.R")

# Load stock data for the selected companies
stock_data <- load_stock_data()

# Calculate statistics for all stocks
stock_statistics <- lapply(stock_data, calculate_statistics)

# Display the first six rows of each stock dataset
display_stock_data(stock_data)

# Display a chart of FedEx stock prices
chartSeries(stock_data$FDX, name = "FedEx Stock Price")
