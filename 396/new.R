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
  if (last(cumPnL) > 1000000){
    ret <- (last(cumPnL)-1000000) / mdd
    print(last(cumPnL))
    }
  else {
    ret <- last(cumPnL)-1000000}
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
  data$Diff <- data$Close-data$PriceMean
  # Calculate decision metric
  data$Metric <- momentum_weight * data$Momentum + volume_weight * data$VolumeChange - price_mean_weight * data$Diff
  print(data$Metric)
  print(data$VolumeChange)
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

# Read and process 10 sets of data
all_data <- lapply(1:10, function(i) {
  filename <- paste0("DATA/PART2/", sprintf("%02d", i), ".csv")
  data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
  data$Index <- as.Date(data$Index, format="%Y-%m-%d")
  data$Dataset <- paste0("Dataset ", i)
  data <- data[771:1100, ]
  execute_strategy(data, momentum_weight = 0.5, volume_weight = 1, price_mean_weight =0.5, N = 25, initial_capital = capital)
}) %>% bind_rows()

# Extract the PD Ratios
pd_ratios_df <- all_data %>%
  group_by(Dataset) %>%
  summarize(PD_Ratio = first(PD_Ratio, order_by = Index))


# Merge PD Ratios with all_data
all_data <- merge(all_data, pd_ratios_df, by = "Dataset", all.x = TRUE)

# Plot cumulative returns using ggplot2 with PD Ratios
p <- ggplot(all_data, aes(x = Index, y = PortfolioValue, group = Dataset)) +
  geom_line(color = "blue") +
  labs(title = "Strategy Cumulative Returns", x = "Date", y = "Portfolio Value") +
  theme_minimal() +
  facet_wrap(~ Dataset, scales = "free_y", ncol = 1) +
  geom_text(data = pd_ratios_df, aes(label = paste("PD Ratio:", round(PD_Ratio, 2))), 
            x = Inf, y = Inf, hjust = 1.1, vjust = 2, size = 3, color = "red", inherit.aes = FALSE)

print(p)