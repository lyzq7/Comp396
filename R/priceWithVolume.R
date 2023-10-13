library(readr)
library(ggplot2)
library(gridExtra)

# 初始化一个空列表来存储数据
data_list <- list()

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  data_list[[i]] <- data
}

# 创建一个函数来绘制每个文件的散点图并计算相关性
plot_correlation <- function(data) {
  correlation <- cor(data$Close, data$Volume, use = "complete.obs")
  
  p <- ggplot(data, aes(x = Close, y = Volume)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "blue") +  # 添加线性回归线
    labs(title = paste0("Correlation between Price and Volume: ", round(correlation, 2)),
         x = "Price",
         y = "Volume") +
    theme_minimal()
  
  return(p)
}

# 使用lapply函数为每个文件创建一个图形列表
plots <- lapply(data_list, plot_correlation)

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 2)
