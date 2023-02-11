# Stock Price Prediction Model

#Load pacman packagev - This helps load packages without using library function over and over.
library(pacman)
pacman::p_load(data.table, fixest, BatchGetSymbols, finreportr, tidyverse, lubridate, ggplot2)

first.date <- Sys.Date() - 2500
last.date <- Sys.Date()
freq.data <- "monthly"
tickers <- c("HAR", "SNT", "STX", "TFG", "BHP", "SAB", 
             "BTI", "CFR", "AMS", "MTN", "VOD", "CPI", "SOL", 
             "KIO", "ABG","SHP","SLM", "IMP", 
             "SSW", "MEI", "REM")

# Get Stock Prices
stocks <- BatchGetSymbols(tickers = tickers, 
                          first.date = first.date,
                          last.date = last.date, 
                          freq.data = freq.data,
                          do.cache = FALSE,
                          thresh.bad.data = 0)

## Verify Returns
stocks_data <- stocks$df.tickers %>% setDT() %>%          # Convert to data.table
  .[order(ticker, ref.date)]                           # Order by ticker and date

#colnames(stocks_data)
#head(stocks_data)
# returns_plot_all <- ggplot(stocks_data[ticker %in% c("MTN", "SOL", "STX", "SHP")], 
#                            aes(x= ref.date, y = ret.adjusted.prices, colour = ticker)) +
#   geom_line() + theme_bw() + 
#   labs(title = "", x = "Date", y= "Monthly Returns", subtitle = "") 
# returns_plot_all
# price_plot_all <- ggplot(stocks_data[ticker %in% c("MTN", "SOL", "STX", "RMB")], 
#                          aes(x= ref.date, y = price.close, colour = ticker)) +
#   geom_line() + theme_bw() + labs(title = "", x = "Date", y= "Closing Price", subtitle = "")
# price_plot_all

prices_sep <- ggplot(stocks_data[ticker %in% c("HAR", "SNT", "STX", "TFG", "BHP", "SAB", 
                                               "BTI", "CFR", "AMS", "MTN", "VOD", "CPI", "SOL", 
                                               "KIO", "ABG","SHP","SLM", "IMP", 
                                               "SSW", "MEI", "REM")], 
                     aes(x = ref.date, y = price.close)) + geom_line() + 
  facet_wrap(~ticker, scales = "free_y") + theme_bw()
prices_sep

returns_sep <- ggplot(stocks_data[ticker %in% c("HAR", "SNT", "STX", "TFG", "BHP", "SAB", 
                                                 "BTI", "CFR", "AMS", "MTN", "VOD", "CPI", "SOL", 
                                                 "KIO", "ABG","SHP","SLM", "IMP", 
                                                 "SSW", "MEI", "REM")], 
                      aes(x = ref.date, y = ret.adjusted.prices)) + geom_line() + 
  facet_wrap(~ticker, scales = "free_y") + theme_bw()
returns_sep

# Pre-processing the Data
# Get the data for only the "price.close" column

stocks_price_close <- stocks_data[, .(price.close), by = ticker]

library(caTools)
# Building the Model
# Fit a time series model using the auto.arima() function in the forecast library
library(forecast)

# Split the data into training and test data sets
split <- sample.split(stocks_price_close$price.close, SplitRatio = 0.7)

mse <- data.frame(Ticker = character(), MSE = numeric(), stringsAsFactors = FALSE)

for (ticker in tickers) {
  stocks_price_close_train <- stocks_price_close[split == TRUE & ticker == ticker, ]
  stocks_price_close_test <- stocks_price_close[split == FALSE & ticker == ticker, ]
  
  # Building the Model
  # Fit a time series model using the auto.arima() function in the forecast library
  
  library(forecast)
  
  # Use the model to make predictions on the test data
  stocks_price_close_train$price.close <- as.numeric(stocks_price_close_train$price.close)
  
  auto.arima_model <- auto.arima(stocks_price_close_train$price.close,
                                 seasonal = TRUE,
                                 xreg = stocks_price_close_train$price.close)
  
  
  auto.arima_pred <- forecast(auto.arima_model, xreg = stocks_price_close_test$price.close)
  
  mse_temp <- mean((auto.arima_pred$mean - stocks_price_close_test$price.close)^2)
  mse <- rbind(mse, data.frame(Ticker = ticker, MSE = mse_temp))
}

mse_average <- mean(mse$MSE)
mse_average  


