library(TTR)
library(zoo)
library(quantmod)
library(ggplot2)
library(gridExtra)

#  定义execute_strategy函数
execute_strategy <- function(data) {
  # 计算RSI
  data$RSI <- RSI(Cl(data), 14)
  # 计算支撑和阻力
  data$Support <- rollapply(Cl(data), 20, min, fill=NA)
  data$Resistance <- rollapply(Cl(data), 20, max, fill=NA)
  
  signal <- rep(0, nrow(data))
  for (i in 15:nrow(data)) {
    if (is.na(data$Support[i]) || is.na(data$RSI[i]) || is.na(data$Close[i]) || is.na(data$Resistance[i])) {
      next
    }
    # 买入信号
    if (data$Close[i] < data$Support[i] && data$RSI[i] < 30) {
      signal[i] <- 1
    }
    # 卖出信号
    if (data$Close[i] > data$Resistance[i] && data$RSI[i] > 70) {
      signal[i] <- -1
    }
  }
  
  data$Signal <- signal
  data$Return <- c(NA, diff(Cl(data))) / lag(Cl(data))
  data$StratReturn <- data$Return * data$Signal
  data$StratReturn[is.na(data$StratReturn)] <- 0  # 将NA值替换为0
  data$CumulativeReturn <- cumprod(1 + data$StratReturn) - 1
  return(data)
}


# 初始化一个空列表来存储图形
plots <- list()

# 循环读取每个文件，应用策略，并绘制图形
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read.csv(file_name, row.names = 1)
  
  # 将行名转换为Date列，并将其转换为日期格式
  data$Date <- as.Date(rownames(data), format="%Y-%m-%d")
  
  # 应用策略
  result <- execute_strategy(data)
  
  # 使用ggplot2绘制图形
  p <- ggplot(result, aes(x = Date)) +
    geom_line(aes(y = Close), color = "black") +
    geom_line(aes(y = Support), color = "green", linetype = "dashed") +
    geom_line(aes(y = Resistance), color = "red", linetype = "dashed") +
    labs(title = paste0("Support and Resistance Levels for File ", sprintf("%02d.csv", i)),
         x = "Date",
         y = "Price") +
    theme_minimal()
  
  # 将图形添加到列表中
  plots[[i]] <- p
}

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 4)  # 这里假设你想要每行显示4个图形
