# Function to simulate trading account balance
simulate_trades <- function(starting_balance, risk_percent, num_trades, win_rate, reward_risk_ratio = 3) {
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
num_trades <- 1000
risk_percentages <- c(seq(0.001, 1, by = 0.001))
win_rates <- seq(0.01, 1, by = 0.01)

# Initialize a list to store results
results_list <- list()

# Run simulations for each combination of risk percentage and win rate
for (win_rate in win_rates) {
  end_balances <- sapply(risk_percentages, function(risk_percent) {
    simulate_trades(starting_balance, risk_percent, num_trades, win_rate)
  })
  results_df <- data.frame(
    RiskPercent = risk_percentages * 100 , # Convert to percentage for readability
    WinRate = win_rate * 100,  # Convert to percentage for readability
    EndBalance = end_balances
  )
  results_list[[paste0("WinRate_", win_rate * 100)]] <- results_df
}

# Combine all results into a single data frame
combined_results_df <- do.call(rbind, results_list)

# Write the combined results to a CSV file
write.csv(combined_results_df, file = "Risk_Simulation.csv", row.names = FALSE)

# Print a message indicating completion
print("Simulation complete. Results saved to Risk_Simulation.csv")
