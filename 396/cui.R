# 加载必要的库
library(dplyr)
library(zoo)
library(ggplot2)

# 创建函数以模拟交易
simulate_trading <- function(row) {
  if (row$Signal == 'Buy') {
    shares <<- capital / row$Close
    capital <<- 0
    position <<- 1
    transactions <<- rbind(transactions, c(row$name, row$Close, 'Buy'))
  } else if (row$Signal == 'Sell') {
    capital <<- shares * row$Close
    shares <<- 0
    position <<- 0
    transactions <<- rbind(transactions, c(row$name, row$Close, 'Sell'))
  }
  portfolio_value <<- c(portfolio_value, ifelse(position == 0, capital, shares * row$Close))
}

# 应用模拟交易函数
df <- df %>% rowwise() %>% do(simulate_trading(.))

# 将投资组合价值添加到DataFrame
df$PortfolioValue <- portfolio_value

# 计算每次买入/卖出的时间间隔
buy_dates <- transactions[transactions[, 3] == 'Buy', 1]
sell_dates <- transactions[transactions[, 3] == 'Sell', 1]

# 计算时间间隔的平均值
if (length(buy_dates) > 0 && length(sell_dates) > 0) {
  avg_buy_interval <- as.numeric((tail(buy_dates, 1) - head(buy_dates, 1)) / length(buy_dates))
  avg_sell_interval <- as.numeric((tail(sell_dates, 1) - head(sell_dates, 1)) / length(sell_dates))
} else {
  avg_buy_interval <- as.numeric(as.difftime(0, units = "days"))
  avg_sell_interval <- as.numeric(as.difftime(0, units = "days"))
}

cat("Average Buy Interval:", avg_buy_interval, "\n")
cat("Average Sell Interval:", avg_sell_interval, "\n")

# 绘制投资组合价值曲线
ggplot(data = df, aes(x = index, y = PortfolioValue)) +
  geom_line() +
  labs(title = 'Portfolio Value Over Time', x = 'Date', y = 'Portfolio Value')

# 基于移动平均线的趋势跟踪策略
data <- df  # 假设df包含了趋势跟踪分析的数据
# 计算移动平均线
data$`50-day MA` <- rollmean(data$Close, k = 50, fill = NA)
data$`200-day MA` <- rollmean(data$Close, k = 200, fill = NA)

# 生成买入和卖出信号
data$Signal <- 0
data$Signal[data$`50-day MA` > data$`200-day MA`] <- 1
data$Signal[data$`50-day MA` < data$`200-day MA`] <- -1

# 绘制趋势跟踪策略的图表
ggplot(data = data, aes(x = index)) +
  geom_line(aes(y = Close), alpha = 0.5) +
  geom_line(aes(y = `50-day MA`), alpha = 0.7) +
  geom_line(aes(y = `200-day MA`), alpha = 0.7) +
  geom_point(data = data[data$Signal == 1, ], aes(y = `50-day MA`), shape = 17, size = 4, color = 'green', alpha = 0.7) +
  geom_point(data = data[data$Signal == -1, ], aes(y = `50-day MA`), shape = 8, size = 4, color = 'red', alpha = 0.7) +
  labs(title = 'Trend Following Strategy', x = 'Date', y = 'Price')

# 计算累积收益
initial_investment <- df$PortfolioValue[1]
final_portfolio_value <- df$PortfolioValue[nrow(df)]
total_return <- (final_portfolio_value - initial_investment) / initial_investment

cat(paste("Initial Investment: $", sprintf("%.2f", initial_investment), "\n"))
cat(paste("Final Portfolio Value: $", sprintf("%.2f", final_portfolio_value), "\n"))
cat(paste("Total Return: ", sprintf("%.2f%%", total_return * 100), "\n"))