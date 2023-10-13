library(readr)
library(ggplot2)
library(gridExtra)
library(TTR)  # 用于计算滚动最大值和最小值

# 初始化一个空列表来存储数据和图形
data_list <- list()
plots <- list()

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  
  # 计算支撑和阻力水平
  data$Support <- zoo::rollapply(data$Low, width = 14, FUN = min, fill = NA, align = "right")
  data$Resistance <- zoo::rollapply(data$High, width = 14, FUN = max, fill = NA, align = "right")
  
  data_list[[i]] <- data
  
  # 使用ggplot2为每个文件绘制支撑和阻力水平
  p <- ggplot(data, aes(x = Index)) +
    geom_line(aes(y = Close), color = "black") +
    geom_line(aes(y = Support), color = "green", linetype = "dashed") +
    geom_line(aes(y = Resistance), color = "red", linetype = "dashed") +
    labs(title = paste0("Support and Resistance Levels for File ", sprintf("%02d.csv", i)),
         x = "Date",
         y = "Price") +
    theme_minimal()
  
  plots[[i]] <- p
}

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 2)
