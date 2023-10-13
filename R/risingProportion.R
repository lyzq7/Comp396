library(readr)

# 初始化一个空列表来存储数据
data_list <- list()

# 读取10个CSV文件
for (i in 1:10) {
  file_name <- paste0("DATA/PART1/", sprintf("%02d.csv", i))
  data <- read_csv(file_name)
  data_list[[i]] <- data
}

# 计算每个文件中上涨的天数的占比
upward_ratios <- sapply(data_list, function(data) {
  # 计算每天与前一天的收盘价的差异
  diff_close <- diff(data$Close)
  # 计算上涨的天数
  upward_days <- sum(diff_close > 0)
  # 计算上涨的占比
  upward_ratio <- upward_days / (length(data$Close) - 1)
  return(upward_ratio)
})

# 打印每个文件的上涨占比
for (i in 1:10) {
  cat(paste0("File ", sprintf("%02d.csv", i), ": ", round(upward_ratios[i] * 100, 2), "%\n"))
}

