library(readr)
library(ggplot2)
library(gridExtra)

# 初始化一个空列表来存储数据
data_list <- list()

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  data$File <- i # 添加一个新列来标识文件编号
  data_list[[i]] <- data
}

# 合并所有数据
all_data <- do.call(rbind, data_list)

# 创建一个函数来绘制每个文件的频率分布图
plot_histogram <- function(data) {
  ggplot(data, aes(x = Close)) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste0("Frequency Distribution for File ", unique(data$File)),
         x = "Price",
         y = "Frequency") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(min(data$Close, na.rm = TRUE), max(data$Close, na.rm = TRUE), by = 10))
}

# 使用lapply函数为每个文件创建一个图形列表
plots <- lapply(data_list, plot_histogram)

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 2)
