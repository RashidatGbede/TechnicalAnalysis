# Load required packages
library(shiny)
library(ggplot2)
library(quantmod)
# Define stock symbol and date range
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"
# Fetch historical stock data from Yahoo Finance
stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# Step 2: Visualizing Stock Data

# Create Shiny app skeleton
ui <- fluidPage(
  titlePanel("Stock Portfolio Technical Analysis"),
  
  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = "2023-01-01",
    end = "2023-07-01"
  ),
  
  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c("Daily", "Weekly", "Monthly")
  ),
  
  selectInput(
    "chart_type",
    "Select Chart Type:",
    choices = c("Line", "Candlestick", "Area")
  ),
  
  checkboxGroupInput(
    "technical_indicators",
    "Select Technical Indicators:",
    choices = c("Moving Averages", "RSI", "MACD"),
    selected = c("Moving Averages", "RSI", "MACD")
  ),
  
  plotOutput("stock_chart"),
  plotOutput("indicator_chart")
)

server <- function(input, output) {
  output$stock_chart <- renderPlot({
    
    # Filter data based on selected date range
    filtered_data <- subset(stock_data, index(stock_data) >= input$date_range[1] &
                              index(stock_data) <= input$date_range[2])
    
    # Filter data based on selected time frame
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data, indexAt = "firstof")
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data, indexAt = "firstof")
    }
    
    # Prepare data for visualization
    chart_data <- data.frame(
      Date = as.Date(index(filtered_data)),
      Open = as.numeric(Op(filtered_data)),
      High = as.numeric(Hi(filtered_data)),
      Low = as.numeric(Lo(filtered_data)),
      Close = as.numeric(Cl(filtered_data))
    )
    # Calculate 20-period and 50-period Moving Averages
    chart_data$MA20 <- as.numeric(TTR::SMA(Cl(filtered_data), n = 20))
    chart_data$MA50 <- as.numeric(TTR::SMA(Cl(filtered_data), n = 50))
    # Calculate 14-period Relative Strength Index
    chart_data$RSI <- as.numeric(TTR::RSI(Cl(filtered_data), n = 14))
    # Calculate MACD
    macd_values <- TTR::MACD(
      Cl(filtered_data),
      nFast = 12,
      nSlow = 26,
      nSig = 9
    )
    
    chart_data$MACD <- as.numeric(macd_values[, "macd"])
    chart_data$MACD_Signal <- as.numeric(macd_values[, "signal"])
    # Create chart based on user selection
    if (input$chart_type == "Line") {
      
      p <- ggplot(chart_data, aes(x = Date, y = Close)) +
        geom_line(color = "steelblue")
      
    } else if (input$chart_type == "Area") {
      
      p <- ggplot(chart_data, aes(x = Date, y = Close)) +
        geom_area(fill = "lightblue", alpha = 0.5)
      
    } else if (input$chart_type == "Candlestick") {
      
      candle_width <- switch(
        input$time_frame,
        Daily = 0.5,
        Weekly = 3,
        Monthly = 10
      )
      
      p <- ggplot(chart_data, aes(x = Date)) +
        geom_linerange(
          aes(ymin = Low, ymax = High),
          color = "grey40"
        ) +
        geom_rect(
          aes(
            xmin = Date - candle_width,
            xmax = Date + candle_width,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Close >= Open
          ),
          color = "grey30"
        ) +
        scale_fill_manual(
          values = c("TRUE" = "darkseagreen3", "FALSE" = "tomato")
        )
    }
    # Add Moving Averages when selected
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(data = chart_data, aes(x = Date, y = MA20),
                  color = "orange", linewidth = 0.8,
                  na.rm = TRUE, inherit.aes = FALSE) +
        geom_line(data = chart_data, aes(x = Date, y = MA50),
                  color = "red", linewidth = 0.8,
                  na.rm = TRUE, inherit.aes = FALSE)
    }  
    # Add MACD overlay when selected
    if ("MACD" %in% input$technical_indicators) {
      p <- p +
        geom_line(
          data = chart_data,
          aes(x = Date, y = MACD),
          color = "blue",
          linewidth = 0.7,
          na.rm = TRUE,
          inherit.aes = FALSE
        ) +
        geom_line(
          data = chart_data,
          aes(x = Date, y = MACD_Signal),
          color = "purple",
          linewidth = 0.7,
          na.rm = TRUE,
          inherit.aes = FALSE
        )
    }
    # Generate moving average crossover signals
    previous_ma20 <- c(NA, head(chart_data$MA20, -1))
    previous_ma50 <- c(NA, head(chart_data$MA50, -1))
    
    chart_data$Signal <- ifelse(
      chart_data$MA20 > chart_data$MA50 &
        previous_ma20 <= previous_ma50, "Buy",
      ifelse(
        chart_data$MA20 < chart_data$MA50 &
          previous_ma20 >= previous_ma50, "Sell",
        "Hold"
      )
    )
    
    chart_data$Signal[is.na(chart_data$Signal)] <- "Hold"
    
    # Annotate Buy and Sell signals on the chart
    signal_data <- chart_data[
      chart_data$Signal %in% c("Buy", "Sell"),
    ]
    
    if (nrow(signal_data) > 0) {
      p <- p +
        geom_text(
          data = signal_data,
          aes(x = Date, y = Close, label = Signal),
          color = "blue",
          vjust = -0.7,
          size = 3,
          inherit.aes = FALSE
        )
    }
    # Return the ggplot object
    print(p)
  })

# Display RSI and MACD indicators
output$indicator_chart <- renderPlot({
  
  # Filter data using the selected date range
  req(input$date_range)
  
  indicator_stock_data <- stock_data[
    index(stock_data) >= as.Date(input$date_range[1]) &
      index(stock_data) <= as.Date(input$date_range[2])
  ]
  
  # Apply the selected time frame
  if (input$time_frame == "Weekly") {
    indicator_stock_data <- to.weekly(
      indicator_stock_data, indexAt = "firstof"
    )
  } else if (input$time_frame == "Monthly") {
    indicator_stock_data <- to.monthly(
      indicator_stock_data, indexAt = "firstof"
    )
  }
  
  # Prepare RSI data
  indicator_data <- data.frame(
    Date = as.Date(index(indicator_stock_data)),
    RSI = as.numeric(TTR::RSI(Cl(indicator_stock_data), n = 14))
  )
  
  if ("RSI" %in% input$technical_indicators) {
    p <- ggplot(indicator_data, aes(x = Date, y = RSI)) +
      geom_line(color = "purple") +
      geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "green") +
      labs(title = "Relative Strength Index (RSI)", y = "RSI")
    print(p)
  }
  # Display MACD indicator
  if ("MACD" %in% input$technical_indicators) {
    macd_values <- TTR::MACD(
      Cl(indicator_stock_data),
      nFast = 12,
      nSlow = 26,
      nSig = 9
    )
    
    macd_data <- data.frame(
      Date = as.Date(index(indicator_stock_data)),
      MACD = as.numeric(macd_values[, "macd"]),
      Signal = as.numeric(macd_values[, "signal"])
    )
    
    p <- ggplot(macd_data, aes(x = Date)) +
      geom_line(aes(y = MACD), color = "blue") +
      geom_line(aes(y = Signal), color = "red") +
      labs(title = "Moving Average Convergence Divergence (MACD)",
           y = "MACD", x = "Date")
    
    print(p)
  } 
})
}


shinyApp(ui, server)
