library(readr)
library(ggplot2)
library(gridExtra)
library(stats)  # 加载stats库以使用acf函数

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

# 创建一个函数来绘制每个文件的时间序列图和ACF图
plot_mean_reverting <- function(data) {
  p1 <- ggplot(data, aes(x = Index, y = Close)) +
    geom_line(color = "blue") +
    labs(title = paste0("Time Series Plot for File ", unique(data$File)),
         x = "Date",
         y = "Price") +
    theme_minimal()
  
  acf_data <- acf(data$Close, plot = FALSE)
  acf_df <- with(acf_data, data.frame(Lag = lag, ACF = acf))
  p2 <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste0("ACF Plot for File ", unique(data$File)),
         x = "Lag",
         y = "Autocorrelation") +
    theme_minimal()
  
  list(p1, p2)
}

# 使用lapply函数为每个文件创建一个图形列表
plots <- lapply(data_list, plot_mean_reverting)

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
plot_list <- unlist(plots, recursive = FALSE)
grid.arrange(grobs = plot_list, ncol = 2)
