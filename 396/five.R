library(dplyr)
library(ggplot2)
library(zoo)

capital <- 1000000

# Define the new maxdrawdown and pdratio functions
maxdrawdown <- function (cumPnL) {
  as.numeric(max(cummax(cumPnL) - cumPnL))
}

pdratio <- function(cumPnL) {
  mdd <- maxdrawdown(cumPnL) # >= 0
  if (last(cumPnL) > 1000000)
    ret <- (last(cumPnL)-1000000) / mdd
  else 
    ret <- last(cumPnL)-1000000
  return(as.numeric(round(ret, 2)))
}

execute_strategy <- function(data, momentum_weight = 1, volume_weight = 1, 
                            price_mean_weight = 1, N = 25, initial_capital = 1000000) {
  # Calculate momentum
  data$Momentum <- data$Close - lag(data$Close, N)
  # Calculate volume change
  data$VolumeChange <- data$Volume - lag(data$Volume, N)
  # Calculate price mean
  data$PriceMean <- rollapply(data$Close, width = N, FUN = mean, align = "right", fill = NA)
  #data$Diff <- data$Close-data$PriceMean
  # Calculate decision metric
  data$Metric <- momentum_weight * data$Momentum + volume_weight * data$VolumeChange - price_mean_weight * data$PriceMean
  # Create buy/sell signals
  data$Signal <- ifelse(data$Metric > 0, 1, ifelse(data$Metric < 0, -1, 0))
  # Shift the signal by 1 day to delay the decision
  data$Signal <- lag(data$Signal, 1)
  # Set initial capital
  cash <- initial_capital
  shares <- 0
  # Initialize portfolio value column
  data$PortfolioValue <- NA
  # Execute trades based on signals
  for (i in 1:nrow(data)) {
    if (!is.na(data$Signal[i])) {
      if (data$Signal[i] == 1 && cash >= data$Open[i]) { # Buy
        shares_bought <- floor(cash / data$Open[i])
        cash <- cash - shares_bought * data$Open[i]
        shares <- shares + shares_bought
      } else if (data$Signal[i] == -1 && shares > 0) { # Sell
        cash <- cash + shares * data$Open[i]
        shares <- 0
      }
    } else {
       cash <- cash
       shares <- shares
    }
    data$PortfolioValue[i] <- cash + shares * data$Close[i]


  }
  
 # Calculate PD ratio using the new function
  data$PD_Ratio <- pdratio(data$PortfolioValue)
  return(data)
}

# Read and process data
data <- read.csv("DATA/PART2/04.csv", header = TRUE, stringsAsFactors = FALSE)
data$Index <- as.Date(data$Index, format="%Y-%m-%d")

# Define parameter ranges
momentum_weights <- seq(0.5, 4, by = 0.5)
volume_weights <- seq(0.5, 4, by = 0.5)
price_mean_weights <- seq(0.5, 4, by = 0.5)
N_values <- seq(10, 50, by = 5)

# Initialize a list to store results
results <- list()

# Loop over parameter combinations
for (mw in momentum_weights) {
  for (vw in volume_weights) {
    for (pmw in price_mean_weights) {
      for (N in N_values) {
        # Split data into training and testing sets
        training_data <- data[1:770, ]
        testing_data <- data[771:1100, ]

        # Apply strategy on training data
        training_result <- execute_strategy(training_data, momentum_weight = mw, volume_weight = vw, 
                                            price_mean_weight = pmw, N = N, initial_capital = capital)
        
        # Evaluate strategy on testing data
        testing_result <- execute_strategy(testing_data, momentum_weight = mw, volume_weight = vw, 
                                           price_mean_weight = pmw, N = N, initial_capital = capital)
        
        # Store results
        results[[paste(mw, vw, pmw, N, sep = "_")]] <- list(training = training_result, testing = testing_result)
      }
    }
  }
}

# Analyze and visualize the results
# Example: Extract PD Ratios and plot them
pd_ratios <- lapply(results, function(result) {
  list(training = pdratio(result$training$PortfolioValue), testing = pdratio(result$testing$PortfolioValue))
})

pd_ratios_df <- as.data.frame(do.call(rbind, pd_ratios))
names(pd_ratios_df) <- c("Training_PD", "Testing_PD")
pd_ratios_df$Parameter_Combination <- rownames(pd_ratios_df)

# 确保 PD 比率是数值型
pd_ratios_df$Training_PD <- as.numeric(as.character(pd_ratios_df$Training_PD))
pd_ratios_df$Testing_PD <- as.numeric(as.character(pd_ratios_df$Testing_PD))
# 对 PD 比率进行排序，并选择前 10 个
top_pd_ratios <- pd_ratios_df %>%
  arrange(desc(Testing_PD)) %>%
  head(1)

# 对 PD 比率进行排序，并选择前 10 个
top_pd_ratios1 <- pd_ratios_df %>%
  arrange(desc(Training_PD)) %>%
  head(1)  

# 检查 top_pd_ratios 是否包含多个行
print(top_pd_ratios)
print(top_pd_ratios1)
# 绘制 PD 比率图，使用点的大小表示 PD 比率的大小
p <- ggplot(top_pd_ratios, aes(x = Training_PD, y = Testing_PD, size = Testing_PD, label = Parameter_Combination)) +
  geom_point(alpha = 0.5) +  # 增加透明度
  geom_text(vjust = 1.5, hjust = 1.5, check_overlap = TRUE) +
  labs(title = "Top 10 PD Ratios of Different Parameter Combinations",
       x = "Training PD Ratio",
       y = "Testing PD Ratio",
       size = "Testing PD Ratio") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p)
