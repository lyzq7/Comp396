library(tidyverse)
library(quadprog)
library(PerformanceAnalytics)
library(caret)
library(ggplot2)
library(gridExtra)
library(xts)
# Function to execute trading strategy and return profit plot
execute_strategy <- function(data) {

  returns <- dailyReturn(data$Close)
  Dmat <- cov(returns)
  dvec <- rep(0, ncol(Dmat))
  Amat <- matrix(1, ncol(Dmat))
  bvec <- 1
  meq <- 1
  
  opt_result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = meq)
  opt_weights <- opt_result$solution
  
  # Quantitative Risk Management
  portfolio_returns <- returns %*% opt_weights
  VaR <- VaR(portfolio_returns, p = 0.95)
  
  # Machine Learning and Prediction Model
  train_data <- data.frame(Close = data$Close, Volume = data$Volume)
  train_control <- trainControl(method = "cv", number = 10)
  model <- train(Close ~ Volume, data = train_data, method = "rf", trControl = train_control)
  predictions <- predict(model, train_data)
  
  # Combining Strategies: Buying when predicted price is higher and SMA is bullish, selling otherwise
  sma_short <- SMA(data$Close, n = 10)
  sma_long <- SMA(data$Close, n = 50)
  buy_signals <- ifelse(predictions > lag(predictions) & sma_short > sma_long, 1, 0)
  sell_signals <- ifelse(buy_signals == 0, 1, 0)
  
  # Calculating profit
  buy_price <- ifelse(buy_signals == 1, data$Close, 0)
  sell_price <- ifelse(sell_signals == 1, data$Close, 0)
  profit <- cumsum(sell_price - dplyr::lag(buy_price, default = first(buy_price)))
  
  # Creating profit plot
  profit_plot <- ggplot(data, aes(x = as.Date(data$Index))) + 
    geom_line(aes(y = profit), color = 'blue') +
    labs(title = "Profit over time", x = "Date", y = "Profit")
  
  return(profit_plot)
}

# 读取所有数据集并执行交易策略
plot_list <- list()
for(i in 1:10) {
  filename <- paste0("DATA/PART1/", sprintf("%02d", i), ".csv")
  data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  plot_list[[i]] <- execute_strategy(data)
}

# 将所有利润图合并为一个图
grid.arrange(grobs = plot_list, ncol = 2)
