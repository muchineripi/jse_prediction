############################################
# Elias Muchineripi Mashayamombe
# CYO Project Final Submission
# HarvardX PH125.9x - Data Science: Capstone
# Date - 13/02/2023
#############################################


# Stock Price Prediction Models

#Load necessary pacakges if they are not there and iterate through the packages using a for loop
# Fit a time series model using the auto.arima() function in the forecast library

PackageNames <- c("data.table,", "tidyverse", "fixest", "BatchGetSymbols", "finreportr", 
                  "lubridate", "ggplot2", "tseries", "knitr", "caTools", "forecast", "caret")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
} 


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

colnames(stocks_data)
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

results <- data.table(ticker = character(0), test = character(0), statistic = numeric(0), p.value = numeric(0))
for (ticker in tickers) {
  price.close <- stocks_price_close[ticker == ticker, price.close]
  adf.test <- adf.test(price.close, k = 0)
  results <- rbind(results, data.table(ticker = ticker, test = "ADF", statistic = adf.test$statistic, p.value = adf.test$p.value))
}

kable(results, caption = "Unit Root Test Results", align = c("l", "c", "c", "c"))

# Building the Model
# Split the data into training and test data sets
split <- sample.split(stocks_price_close$price.close, SplitRatio = 0.7)

mse <- data.frame(Ticker = character(), MSE = numeric(), stringsAsFactors = FALSE)

for (ticker in tickers) {
  stocks_price_close_train <- stocks_price_close[split == TRUE & ticker == ticker, ]
  stocks_price_close_test <- stocks_price_close[split == FALSE & ticker == ticker, ]
  
# Use the model to make predictions on the test data
  stocks_price_close_train$price.close <- as.numeric(stocks_price_close_train$price.close)
  
  auto.arima_model <- auto.arima(stocks_price_close_train$price.close,
                                 seasonal = TRUE,
                                 xreg = stocks_price_close_train$price.close)
  
  
  auto.arima_pred <- forecast(auto.arima_model, xreg = stocks_price_close_test$price.close)
  
  mse_temp <- mean((auto.arima_pred$mean - stocks_price_close_test$price.close)^2)
  mse <- rbind(mse, data.frame(Ticker = ticker, MSE = mse_temp))
}

# mCalculate the mean square error
mse_average <- mean(mse$MSE)
mse_average  


# Split data into train and test sets for each stock
set.seed(123)
splits <- lapply(unique(stocks_price_close$ticker), function(x) {
  stock_prices <- stocks_price_close[ticker == x,]$price.close
  train_index <- createDataPartition(stock_prices, p = 0.8, list = FALSE)
  list(train = data.frame(price.close = stock_prices[train_index]), 
       test = data.frame(price.close = stock_prices[-train_index]))
})

head(splits)

# Define the RNN model
rnn_model <- function(x) {
  train_df <- x$train
  test_df <- x$test
  colnames(train_df) <- c("price.close")
  colnames(test_df) <- c("price.close")
  model <- train(price.close ~ ., data = train_df, method = "recurrent_neural_network")
  predict(model, test_df)
}

# Use sapply to apply the RNN model to each stock
predictions <- sapply(splits, rnn_model)


rnn_model <- function(x) {
  train_df <- x$train
  test_df <- x$test
  names(train_df) <- c("price.close")
  names(test_df) <- c("price.close")
  model <- train(price.close ~ ., data = train_df, method = "recurrent_neural_network")
  predict(model, test_df)
}
predictions <- sapply(splits$price.close, rnn_model)
