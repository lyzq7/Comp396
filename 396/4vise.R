# 导入必要的库
library(ggplot2)
library(readr)
library(gridExtra)  # 用于组合多个图形

# 读取数据
data <- read_csv("DATA/PART1/01.csv")

# 创建四个图形
p1 <- ggplot(data, aes(x = Index, y = Open)) +
  geom_line(color = "blue") +
  labs(title = "Open Price",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data, aes(x = Index, y = High)) +
  geom_line(color = "green") +
  labs(title = "High Price",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(data, aes(x = Index, y = Low)) +
  geom_line(color = "red") +
  labs(title = "Low Price",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(data, aes(x = Index, y = Close)) +
  geom_line(color = "black") +
  labs(title = "Close Price",
       x = "Date",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 使用grid.arrange组合图形
grid.arrange(p1, p2, p3, p4, ncol = 1)
