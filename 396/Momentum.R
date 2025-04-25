# 加载所需的库
library(ggplot2)
library(dplyr)

execute_strategy <- function(data) {
  # 设定策略参数
  initial_balance <- 1000000
  balance <- initial_balance
  position <- 0

  # 初始化动量和余额列
  lookback <- 20
  data$Momentum <- rep(NA, nrow(data))
  data$Balance <- rep(NA, nrow(data))
  data$Balance[1:lookback] <- balance

  # 执行策略
  for (i in (lookback+1):nrow(data)) {
    # 计算动量
    data$Momentum[i] <- data$Close[i] - data$Close[i-lookback]
    
    # 如果动量为正，买入
    if (data$Momentum[i] > 0 && balance > data$Open[i]) {
      position <- balance / data$Open[i]
      balance <- 0
    }
    
    # 如果动量为负，卖出
    if (data$Momentum[i] < 0 && position > 0) {
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
  ggtitle("动量策略收益图") +
  xlab("日期") +
  ylab("价值") +
  facet_wrap(~ Dataset, scales = "free_x") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "股价"))

print(plot)
