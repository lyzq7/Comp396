library(ggplot2)
library(readr)
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

# 创建一个函数来绘制每个文件的图形
plot_data <- function(data) {
  ggplot(data, aes(x = Index, y = Volume)) +
    geom_line(color = "blue", size = 0.8) +
    labs(title = paste0("Volume Visualization for File ", unique(data$File)),
         x = "Date",
         y = "Volume") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 使用lapply函数为每个文件创建一个图形列表
plots <- lapply(data_list, plot_data)

# 使用gridExtra包的grid.arrange函数将所有图形整合在一起
grid.arrange(grobs = plots, ncol = 2)
