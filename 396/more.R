# 加载所需的库
library(ggplot2)
library(dplyr)
library(zoo)
library(stats)
execute_strategy <- function(data) {
  # 设定策略参数
  # Set policy parameters
  initial_balance <- 1000000
  balance <- initial_balance
  position <- 0
  
  # 初始化动量、交易量变化、价格均值和评分列
  # Initialize momentum, volume change, price mean, and score column
  lookback <- 20
  data$Momentum <- rep(NA, nrow(data))
  data$VolumeChange <- c(NA, diff(data$Volume))
  data$PriceAvg <- rollapply(data$Close, width = lookback, FUN = mean, align = "right", fill = NA)
  data$Score <- rep(NA, nrow(data))
  data$Balance <- rep(NA, nrow(data))
  data$Balance[1:lookback] <- balance

  # 执行策略
  for (i in (lookback+1):nrow(data)) {
    # 计算动量
    data$Momentum[i] <- data$Close[i] - data$Close[i-lookback]
    
    # 使用线性回归预测未来的收盘价
    if(i < nrow(data)) {
      lm.model <- lm(Close ~ Index, data=data[(i-lookback):i,])
      future_price <- predict(lm.model, newdata=data.frame(Index=data$Index[i+1]))
    } else {
      future_price <- NA
    }
    
    # 综合评分
    price_factor <- ifelse(data$Close[i] < data$PriceAvg[i], 1, -1)
    data$Score[i] <- data$Momentum[i] * data$VolumeChange[i] * price_factor
    
    # 基于评分和预测做出决策
    if (!is.na(future_price) && data$Score[i] > 0 && balance > data$Open[i] && future_price > data$Close[i]) {
      position <- balance / data$Open[i]
      balance <- 0
    } else if (!is.na(future_price) && data$Score[i] < 0 && position > 0 && future_price < data$Close[i]) {
      balance <- balance + position * data$Open[i]
      position <- 0
    }
    
    data$Balance[i] <- balance + position * data$Close[i]
  }
  
  return(data)
}

# 读取和处理10组数据
# Read and process 10 sets of data
all_data <- lapply(1:10, function(i) {
  filename <- paste0("DATA/PART1/", sprintf("%02d", i), ".csv")
  data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  data$Index <- as.Date(data$Index, format="%Y-%m-%d")
  data$Dataset <- paste0("Dataset ", i)
  #data <- data[1:500, ]
  #data <- data[501:nrow(data), ]
  execute_strategy(data)
}) %>% bind_rows()

# 使用ggplot2绘制收益图
# Plot revenue using ggplot2
plot <- ggplot(all_data, aes(x = Index)) +
  geom_line(aes(y = Balance), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = Close), color = "black", na.rm = TRUE) +
  ggtitle("Multifactor strategy return graph") +
  xlab("date") +
  ylab("value") +
  facet_wrap(~ Dataset, scales = "free_x") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Share price"))

print(plot)
