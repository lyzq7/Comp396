# 加载所需的库
library(ggplot2)
library(dplyr)
library(TTR) # 为了计算RSI

execute_strategy <- function(data) {
  # 设定策略参数
  initial_balance <- 1000000
  balance <- initial_balance
  position <- 0
  
  # 计算支撑、阻力和RSI
  lookback <- 40
buffer_percentage <- 0.01  # 1% 缓冲

data$Support <- rollapply(data$Low, width = lookback, FUN = min, align = "right", fill = NA) * (1 + buffer_percentage)
data$Resistance <- rollapply(data$High, width = lookback, FUN = max, align = "right", fill = NA) * (1 - buffer_percentage)

  data$RSI <- RSI(data$Close, n = 14)
  data$Balance <- rep(NA, nrow(data))
  data$Balance[1:lookback] <- balance

  # 执行策略
  for (i in (lookback+1):nrow(data)) {
    # 如果价格跌破支撑且RSI低于30，买入
    if (data$Close[i] < data$Support[i] && data$RSI[i] < 30 && balance > data$Open[i]) {
      position <- balance / data$Open[i]
      balance <- 0
    }
    
    # 如果价格突破阻力且RSI高于70，卖出
    if (data$Close[i] > data$Resistance[i] && data$RSI[i] > 70 && position > 0) {
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

# 使用ggplot2绘制收益图
plot <- ggplot(all_data, aes(x = Index)) +
  geom_line(aes(y = Balance), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = Close), color = "black", na.rm = TRUE) +
  geom_line(aes(y = Support), color = "green", linetype = "dashed", na.rm = TRUE) +
  geom_line(aes(y = Resistance), color = "red", linetype = "dashed", na.rm = TRUE) +
  ggtitle("Support, Resistance and RSI Strategy Returns") +
  xlab("Date") +
  ylab("Value") +
  facet_wrap(~ Dataset, scales = "free_x") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Stock Price"))

print(plot)
