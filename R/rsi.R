library(readr)
library(ggplot2)
library(gridExtra)
library(TTR)  # 用于计算RSI

# 初始化一个空列表来存储数据和图形
data_list <- list()
plots <- list()

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  
  # 计算14天RSI
  data$RSI <- RSI(data$Close, n = 14)
  
  data_list[[i]] <- data
  
  # 使用ggplot2为每个文件绘制RSI图
  p <- ggplot(data, aes(x = Index, y = RSI)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = 70, linetype = "dashed", color = "red") +  # 超买线
    geom_hline(yintercept = 30, linetype = "dashed", color = "green") +  # 超卖线
    labs(title = paste0("14-day Relative Strength Index (RSI) for File ", sprintf("%02d.csv", i)),
         x = "Date",
         y = "RSI") +
    ylim(0, 100) +
    theme_minimal()
  
  plots[[i]] <- p
}

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 2)
