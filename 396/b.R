library(tidyverse)
library(gridExtra)

# 假设这是你的文件路径和文件列表
file_path <- "DATA/PART1"
file_list <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE)

# 定义一个函数来处理每个文件并生成图表
process_file <- function(file) {
  data <- read.csv(file)
  
  # 数据处理
  data <- data %>%
    arrange(Index) %>%
    mutate(Profit = Close - lag(Close, default = first(Close)))
  
  # 计算最后的盈利
  final_profit <- sum(data$Profit, na.rm = TRUE)
  
  # 生成图表
  plot <- ggplot(data, aes(x = as.Date(Index), y = Profit)) +
    geom_line() +
    labs(title = paste("盈利的波动图 -", basename(file), "- 最后的盈利:", round(final_profit, 2)), x = "日期", y = "盈利/亏损") +
    theme_minimal()
  
  return(plot)
}

# 对每个文件应用上述函数
plots <- lapply(file_list, process_file)

# 使用gridExtra包将所有图表组合在一起并显示
do.call(grid.arrange, c(plots, ncol = 2))
