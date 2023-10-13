library(readr)
library(ggplot2)
library(gridExtra)

# 初始化一个空列表来存储数据和图形
data_list <- list()
plots <- list()

# 手动计算真实范围
calculate_TR <- function(high, low, prev_close) {
  TR <- max(high - low, abs(high - prev_close), abs(low - prev_close))
  return(TR)
}

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  
  # 计算真实范围
  data$TR <- NA
  for (j in 2:nrow(data)) {
    data$TR[j] <- calculate_TR(data$High[j], data$Low[j], data$Close[j-1])
  }
  
  # 计算14天ATR
  data$ATR <- zoo::rollapply(data$TR, width = 14, FUN = mean, fill = NA, align = "right")
  
  # 从第14天开始
  data <- data[14:nrow(data), ]
  
  data_list[[i]] <- data
  
  # 使用ggplot2为每个文件绘制ATR图
  p <- ggplot(data, aes(x = Index, y = ATR)) +
    geom_line() +
    labs(title = paste0("14-day Average True Range (ATR) for File ", sprintf("%02d.csv", i)),
         x = "Date",
         y = "ATR") +
    theme_minimal()
  
  plots[[i]] <- p
}

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 2)
