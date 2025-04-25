# 加载必要的库
library(dplyr)
library(zoo)
library(ggplot2)
library(purrr)

# 从DATA/PART1/01.csv加载数据
df <- read.csv("DATA/PART1/01.csv", stringsAsFactors = FALSE)
df$Index <- as.Date(df$Index, format="%Y-%m-%d")

# 初始化变量
capital <- 1000000  # 初始预算
shares <- 0
position <- 0
transactions <- data.frame(Date=as.Date(character()), Price=numeric(), Action=character())
portfolio_value <- numeric()

# 只处理200天之后的数据
df <- df[201:nrow(df), ]

# 基于移动平均线的趋势跟踪策略
# 计算移动平均线
df$`50-day MA` <- rollmean(df$Close, k = 50, fill = NA)
df$`200-day MA` <- rollmean(df$Close, k = 200, fill = NA)

# 重新生成买入和卖出信号
df$Signal <- NA
df$Signal[df$`50-day MA` > df$`200-day MA` & lag(df$`50-day MA`, default = NA) <= lag(df$`200-day MA`, default = NA)] <- 'Buy'
df$Signal[df$`50-day MA` < df$`200-day MA` & lag(df$`50-day MA`, default = NA) >= lag(df$`200-day MA`, default = NA)] <- 'Sell'

# 创建函数以模拟交易
simulate_trading <- function(Signal, Close, Index) {
  if (is.na(Signal)) {
    return(ifelse(capital > 0, capital, shares * Close))
  }
  
  if (Signal == 'Buy' && capital > 0) {
    shares <<- capital / Close
    capital <<- 0
    transactions <<- rbind(transactions, data.frame(Date=Index, Price=Close, Action='Buy'))
  } else if (Signal == 'Sell' && shares > 0) {
    capital <<- shares * Close
    shares <<- 0
    transactions <<- rbind(transactions, data.frame(Date=Index, Price=Close, Action='Sell'))
  }
  return(ifelse(capital > 0, capital, shares * Close))
}

# 应用模拟交易函数
df$PortfolioValue <- pmap_dbl(list(Signal = df$Signal, Close = df$Close, Index = df$Index), simulate_trading)

# 计算每次买入/卖出的时间间隔
buy_dates <- transactions[transactions$Action == 'Buy',]$Date
sell_dates <- transactions[transactions$Action == 'Sell',]$Date

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
ggplot(data = df, aes(x = Index, y = PortfolioValue)) +
  geom_line() +
  labs(title = 'Portfolio Value Over Time', x = 'Date', y = 'Portfolio Value')

# 绘制股票的收盘价和移动平均线
p1 <- ggplot(df, aes(x = Index)) +
  geom_line(aes(y = Close, color = "Close Price"), na.rm = TRUE) +
  geom_line(aes(y = `50-day MA`, color = "50-day MA"), na.rm = TRUE) +
  geom_line(aes(y = `200-day MA`, color = "200-day MA"), na.rm = TRUE) +
  geom_point(aes(y = Close, color = Signal), shape = ifelse(df$Signal == "Buy", 1, ifelse(df$Signal == "Sell", 2, NA)), size = 3, na.rm = TRUE) +
  scale_color_manual(values = c("Close Price" = "black", "50-day MA" = "blue", "200-day MA" = "red", "Buy" = "green", "Sell" = "red")) +
  labs(title = "Trend Following Strategy Based on Moving Averages", y = "Price", color = "Legend") +
  theme_minimal()

# 绘制投资组合的价值随时间的变化
p2 <- ggplot(df, aes(x = Index, y = PortfolioValue)) +
  geom_line(color = "darkgreen", na.rm = TRUE) +
  labs(title = "Portfolio Value Over Time", y = "Value") +
  theme_minimal()

# 显示图表
print(p1)
print(p2)
# 计算累积收益
initial_investment <- df$PortfolioValue[1]
final_portfolio_value <- df$PortfolioValue[nrow(df)]
total_return <- (final_portfolio_value - initial_investment) / initial_investment

cat(paste("Initial Investment: $", sprintf("%.2f", initial_investment), "\n"))
cat(paste("Final Portfolio Value: $", sprintf("%.2f", final_portfolio_value), "\n"))
cat(paste("Total Return: ", sprintf("%.2f%%", total_return * 100), "\n"))
