
# Source the RSI and SMA functions
source("Assignment5/rsi.R")
source("Assignment5/sma.R")

# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Calculate the StochRSI
  if (!any(!is.na(rsi_values))) {
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
    
    if (max_rsi == min_rsi) {
      k_values <- rep(NA_real_, length(rsi_values))
    } else {
      k_values <- (rsi_values - min_rsi) /
        (max_rsi - min_rsi)
    }
  }
  
  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line (3-day simple moving average of %K)
  d_line <- sma(k_line, d_period)
  
  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}
