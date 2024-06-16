# Step 1: Read the CSV file
data <- read.csv("Risk_Reward_Ratio.csv")

# Step 2: Filter data
filtered_data <- subset(data,EndBalance >= 100000 & EndBalance <= 9000000 & WinRate <=40  & RiskPercent <= 0.03)

# Step 3: Sort the data based on EndBalance in descending order
sorted_data <- filtered_data[order(-filtered_data$EndBalance), ]

# Step 4: Select the top 31337 entries
top31337_data <- head(sorted_data, 31337)

# Step 5: Output the top 31337 entries to a new CSV file
write.csv(top31337_data, "top31337_riskpercent_endbalance.csv", row.names = FALSE)

# Print the top 31337 data to console
print(top31337_data)

# Step 6: Plot the top 31337 RiskPercent values against EndBalance
library(ggplot2)
ggplot(top31337_data, aes(x = RiskPercent, y = EndBalance)) +
  geom_point() +
  labs(title = "RiskPercent vs EndBalance",
       x = "RiskPercent",
       y = "EndBalance (USD)") +  # Update the y-axis label for human readability
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas for thousands separator
  theme_minimal()
