
# Load the quantmod package
library(quantmod)

# Function to import stock data
load_stock_data <- function(portfolio_file = "portfolio.txt") {
  
  # Read stock symbols from the portfolio file
  symbols <- readLines(portfolio_file)
  
  # Remove any extra spaces or blank lines
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""]
  
  # Create an empty list to store stock data
  stock_data <- list()
  
  # Import data for each stock symbol
  for (symbol in symbols) {
    
    stock_prices <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
    
    # Store each stock as a separate data frame
    stock_data[[symbol]] <- as.data.frame(stock_prices)
  }
  
  # Return all stock data
  return(stock_data)
}

# Function to calculate basic statistics for each stock
calculate_statistics <- function(stock_df, moving_average_period = 20) {
  
  # Identify the Adjusted price column
  adjusted_col <- grep("\\.Adjusted$", names(stock_df), value = TRUE)
  
  if (length(adjusted_col) == 0) {
    stop("Adjusted price column not found.")
  }
  
  # Extract adjusted stock prices
  prices <- as.numeric(stock_df[[adjusted_col]])
  
  # Remove missing values for statistical calculations
  valid_prices <- prices[!is.na(prices)]
  
  # Calculate the mode
  price_values <- unique(valid_prices)
  price_counts <- tabulate(match(valid_prices, price_values))
  highest_count <- max(price_counts)
  
  mode_value <- if (highest_count == 1) {
    NA_real_
  } else {
    price_values[which.max(price_counts)]
  }
  
  # Calculate the moving average
  moving_average <- TTR::SMA(prices, n = moving_average_period)
  
  # Store all calculated statistics in a list
  statistics <- list(
    moving_average = moving_average,
    mean = mean(valid_prices),
    mode = mode_value,
    median = median(valid_prices),
    standard_deviation = sd(valid_prices)
  )
  
  return(statistics)
}
# Function to display stock data
display_stock_data <- function(stock_data) {
  
  for (symbol in names(stock_data)) {
    
    cat("\nStock Symbol:", symbol, "\n")
    
    print(head(stock_data[[symbol]]))
  }
}
