library(readr)

# 初始化一个空列表来存储数据
data_list <- list()

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  data_list[[i]] <- data
}

# 计算每个文件中Close价格的统计数据
statistics <- sapply(data_list, function(data) {
  mean_val <- mean(data$Close, na.rm = TRUE)
  median_val <- median(data$Close, na.rm = TRUE)
  sd_val <- sd(data$Close, na.rm = TRUE)
  range_val <- range(data$Close, na.rm = TRUE)
  return(c(mean_val, median_val, sd_val, range_val[1], range_val[2]))
})

# 打印每个文件的统计数据
rownames(statistics) <- c("Mean", "Median", "Standard Deviation", "Min", "Max")
colnames(statistics) <- sapply(1:10, function(i) paste0("File ", sprintf("%02d.csv", i)))

print(statistics)

