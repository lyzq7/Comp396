# 加载所需的库
library(ggplot2)
library(dplyr)
library(zoo) # 为了使用 rollapply 函数

execute_strategy <- function(data) {
  # 设定策略参数
  initial_balance <- 1000000
  balance <- initial_balance
  position <- 0
  
  # 初始化动量、交易量变化、价格均值和评分列
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
    
    # 综合评分
    momentum_factor <- ifelse(data$Momentum[i] > 0, 1, -1)
    price_factor <- ifelse(data$Close[i] < data$PriceAvg[i], 1, -1)
    volume_factor <- ifelse(data$VolumeChange[i] > 0, 1, -1) # 当交易量上涨时，取1，否则取-1
    data$Score[i] <- momentum_factor  * price_factor * volume_factor
    
    # 基于评分做出决策
    if (data$Score[i] > 0 && balance > data$Open[i]) {
      position <- balance / data$Open[i]
      balance <- 0
    } else if (data$Score[i] < 0 && position > 0) {
      balance <- balance + position * data$Open[i]
      position <- 0
    }
    
    data$Balance[i] <- balance + position * data$Close[i]
  }
  
  return(data)
}

# 读取和处理10组数据
all_data <- lapply(1:10, function(i) {
  filename <- paste0("DATA/PART1/", sprintf("%02d", i), ".csv")
  data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  data$Index <- as.Date(data$Index, format="%Y-%m-%d")
  data$Dataset <- paste0("Dataset ", i)
  execute_strategy(data)
}) %>% bind_rows()

plot <- ggplot(all_data, aes(x = Index)) +
  geom_line(aes(y = Balance), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = Close), color = "black", na.rm = TRUE) +
  ggtitle("a") +
  xlab("date") +
  ylab("peice") +
  facet_wrap(~ Dataset, scales = "free_x") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "share price"))

print(plot)
