# 加载所需的库
library(ggplot2)
library(dplyr)

# 读取数据
data <- read.csv("DATA/PART1/02.csv", header = TRUE, stringsAsFactors = FALSE)
data$Index <- as.Date(data$Index, format="%Y-%m-%d")

# 设定策略参数
initial_balance <- 1000000
balance <- initial_balance
position <- 0

# 初始化Bollinger Bands和余额列
lookback <- 20
data$SMA <- NA
data$UpperBand <- NA
data$LowerBand <- NA
data$Balance <- rep(NA, nrow(data))
data$Balance[1:lookback] <- balance

# 执行策略
for (i in (lookback+1):nrow(data)) {
  # 计算Bollinger Bands
  data$SMA[i] <- mean(data$Close[(i-lookback):i])
  data$UpperBand[i] <- data$SMA[i] + 2 * sd(data$Close[(i-lookback):i])
  data$LowerBand[i] <- data$SMA[i] - 2 * sd(data$Close[(i-lookback):i])
  
  # 如果收盘价低于下轨，买入
  if (data$Close[i] < data$LowerBand[i] && balance > data$Open[i]) {
    position <- balance / data$Open[i]
    balance <- 0
  }
  
  # 如果收盘价高于上轨，卖出
  if (data$Close[i] > data$UpperBand[i] && position > 0) {
    balance <- balance + position * data$Open[i]
    position <- 0
  }
  
  data$Balance[i] <- balance + position * data$Close[i]
}

# 使用ggplot2绘制收益图
plot <- ggplot(data, aes(x = Index)) +
  geom_line(aes(y = Balance), color = "blue", na.rm = TRUE) +
  geom_line(aes(y = Close), color = "black", na.rm = TRUE) +
  geom_line(aes(y = UpperBand), color = "red", linetype = "dashed", na.rm = TRUE) +
  geom_line(aes(y = LowerBand), color = "green", linetype = "dashed", na.rm = TRUE) +
  ggtitle("Bollinger Bands均值回归策略收益图") +
  xlab("日期") +
  ylab("价值") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "股价"))

print(plot)