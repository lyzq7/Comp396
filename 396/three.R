# 加载所需的库
# Load the required library
library(ggplot2)
library(dplyr)
library(zoo)

execute_strategy <- function(data) {
  # Set policy parameters
  initial_balance <- 1000000

  stock_quantity <- 0
  # Initialize momentum, volume change, price mean, and score column
  lookback <- 20
  data$Momentum <- rep(NA, nrow(data))
  data$position <- rep(0, nrow(data))
  data$VolumeChange <- c(NA, diff(data$Volume))
  data$PriceAvg <- rollapply(data$Close, width = lookback, FUN = mean, align = "right", fill = NA)
  data$Score <- rep(NA, nrow(data))
  data$StockQuantity <- rep(0, nrow(data))
  data$Balance <- rep(0, nrow(data))
  data$Balance[1] <- initial_balance
  initialScore <- 0

  # Implementation Strategy
  for (i in (lookback+1):nrow(data)) {

    # Calculate momentum
    data$Momentum[i] <- data$Close[i] - data$Close[i-lookback]
    

    # Comprehensive score
    momentum_factor <- ifelse(data$Momentum[i] > 0, 1, -1)
    price_factor <- ifelse(data$Close[i] < data$PriceAvg[i], 1, -1)
    volume_factor <- ifelse(data$VolumeChange[i] > 0, 1, -1)
    data$Score[i] <- momentum_factor * volume_factor * price_factor
    

    # Make decisions based on ratings
    if (data$Score[i] > initialScore) {
      data$position[i] <- 1
    } else if (data$Score[i] < initialScore) {
      data$position[i] <- -1
    } else {
       data$position[i] <- 0
    }

  }
  #Use the lag function to defer position
  data$lagged_position <- lag(data$position)




  # Calculate the total balance and the number of shares held based on the deferred position
  for (i in 2:nrow(data)) {
    if(data$lagged_position[i] == 0 ){
      data$StockQuantity[i] = data$StockQuantity[i-1]
      data$Balance[i] = data$Balance[i-1]
    } else if (data$lagged_position[i] == 1 && data$Balance[i-1] >= data$Open[i]) {
      data$StockQuantity[i] = floor(data$Balance[i-1] / data$Open[i])
      data$Balance[i] = data$Balance[i-1] - data$StockQuantity[i] * data$Open[i]

    } else if (data$lagged_position[i] == -1 && data$StockQuantity[i-1] > 0) {
      data$Balance[i] = data$Balance[i-1] + data$StockQuantity[i-1] * data$Open[i]
      data$StockQuantity[i] = 0

    } else {
      data$StockQuantity[i] = data$StockQuantity[i-1]
      data$Balance[i] = data$Balance[i-1]
    }

  }
  print(data$Balance)
  return(data)

}



# 读取和处理10组数据
# Read and process 10 sets of data
all_data <- lapply(1:10, function(i) {
  filename <- paste0("DATA/PART1/", sprintf("%02d", i), ".csv")
  data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  data$Index <- as.Date(data$Index, format="%Y-%m-%d")
  data$Dataset <- paste0("Dataset ", i)
  #data <- data[1:500, ]
  #data <- data[501:nrow(data), ]
  execute_strategy(data)
}) %>% bind_rows()

plot <- ggplot(all_data, aes(x = Index)) +
  geom_line(aes(y = Balance), color = "blue", na.rm = TRUE) +
  ggtitle("a") +
  xlab("date") +
  ylab("peice") +
  facet_wrap(~ Dataset, scales = "free_x") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "share price"))

print(plot)
