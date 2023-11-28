library(dplyr)

# Read the CSV files
tableOne <- read.csv("cybersecurity_attacks.csv")
tableTwo <- read.csv("cyberthreat.csv")

# Merge based on the common column titled "Protocol"
result <- merge(tableOne, tableTwo, by = "Protocol", all = TRUE)

# Select only the "Protocol" column in the result
result <- result["Protocol"]

# View the result
print(result)
