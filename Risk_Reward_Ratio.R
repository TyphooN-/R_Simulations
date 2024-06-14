# Load necessary libraries
library(parallel)

# Function to simulate trading account balance
simulate_trades <- function(starting_balance, risk_percent, num_trades, win_rate, reward_risk_ratio) {
  balance <- starting_balance
  for (i in 1:num_trades) {
    trade_outcome <- sample(c("win", "loss"), 1, prob = c(win_rate, 1 - win_rate))
    risk_amount <- balance * (risk_percent / 100)
    if (trade_outcome == "win") {
      balance <- balance + (risk_amount * reward_risk_ratio)
    } else {
      balance <- balance - risk_amount
    }
  }
  return(balance)
}

# Parameters
starting_balance <- 100000
num_trades <- 10000
risk_percentages <- seq(0.001, 1, by = 0.001)
win_rates <- seq(0.01, 1, by = 0.01)
reward_risk_ratios <- 2:10

# Create a cluster with the number of cores available
num_cores <- detectCores() - 1  # Leave one core free
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, c("simulate_trades", "starting_balance", "num_trades", "risk_percentages", "win_rates", "reward_risk_ratios"))

# Run simulations in parallel
results_list <- parLapply(cl, reward_risk_ratios, function(reward_risk_ratio) {
  sub_results_list <- list()
  for (win_rate in win_rates) {
    end_balances <- sapply(risk_percentages, function(risk_percent) {
      simulate_trades(starting_balance, risk_percent, num_trades, win_rate, reward_risk_ratio)
    })
    results_df <- data.frame(
      RiskPercent = risk_percentages,
      WinRate = win_rate * 100,  # Convert to percentage for readability
      RewardRiskRatio = reward_risk_ratio,
      EndBalance = end_balances
    )
    sub_results_list[[paste0("WinRate_", win_rate * 100)]] <- results_df
  }
  return(do.call(rbind, sub_results_list))
})

# Stop the cluster
stopCluster(cl)

# Combine all results into a single data frame
combined_results_df <- do.call(rbind, results_list)

# Write the combined results to a CSV file
write.csv(combined_results_df, file = "Risk_Reward_Ratio.csv", row.names = FALSE)

# Print a message indicating completion
print("Simulation complete. Results saved to Risk_Reward_Ratio.csv")
