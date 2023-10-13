library(readr)
library(ggplot2)
library(gridExtra)
library(zoo)  # 用于计算移动平均

# 初始化一个空列表来存储数据
data_list <- list()

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  
  # 计算移动平均
  data$MA7 <- rollmean(data$Close, k = 7, fill = NA, align = "right")
  data$MA15 <- rollmean(data$Close, k = 15, fill = NA, align = "right")
  data$MA30 <- rollmean(data$Close, k = 30, fill = NA, align = "right")
  data$MA90 <- rollmean(data$Close, k = 90, fill = NA, align = "right")
  
  data_list[[i]] <- data
}

# 创建一个函数来绘制每个文件的移动平均曲线
plot_moving_average <- function(data) {
  ggplot(data, aes(x = Index)) +
    geom_line(aes(y = Close), color = "black", size = 0.5) +
    geom_line(aes(y = MA7), color = "blue", size = 0.8) +
    geom_line(aes(y = MA15), color = "green", size = 0.8) +
    geom_line(aes(y = MA30), color = "red", size = 0.8) +
    geom_line(aes(y = MA90), color = "purple", size = 0.8) +
    labs(title = paste0("Moving Averages for File ", unique(data$File)),
         x = "Date",
         y = "Price") +
    theme_minimal()
}

# 使用lapply函数为每个文件创建一个图形列表
plots <- lapply(data_list, plot_moving_average)

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 2)
