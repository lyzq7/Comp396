# 导入必要的库
library(ggplot2)
library(tidyverse)
library(lubridate)
library(TTR)

# 读取数据
data <- read.csv("DATA/PART1/02.csv", header = TRUE)
data$Index <- as.Date(data$Index, format = "%Y/%m/%d")

# 绘制股票价格和交易量的时间序列图
p1 <- ggplot(data, aes(x = Index)) +
  geom_line(aes(y = Close, color = "Close")) +
  geom_line(aes(y = Volume/1000, color = "Volume")) +  # Volume除以1000是为了缩放到合适的范围
  labs(title = "Stock Price and Volume Over Time",
       y = "Price / (Volume/1000)",
       color = "Metric") +
  theme_minimal()

print(p1)

# 为数据框添加一个颜色列，以区分上涨和下跌的日子
data$Color <- ifelse(data$Close >= data$Open, "green", "red")

# 使用geom_segment和geom_rect绘制K线图
p2 <- ggplot(data, aes(x = Index)) +
  geom_segment(aes(x = Index, xend = Index, y = Low, yend = High, color = Color)) +
  geom_rect(aes(xmin = as.numeric(Index) - 0.5, xmax = as.numeric(Index) + 0.5, ymin = Open, ymax = Close, fill = Color), position = "identity") +
  labs(title = "Candlestick Chart") +
  theme_minimal() +
  scale_color_identity() +
  scale_fill_identity()

print(p2)


