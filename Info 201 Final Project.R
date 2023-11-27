library(dplyr)

# Read the CSV files
tableOne <- read.csv("cybersecurity_attacks.csv")
tableTwo <- read.csv("cyberthreat.csv")

# Merge the two different files based on the common column titled "Protocol"
result <- merge(tableOne, tableTwo, by = "Protocol", all = TRUE)

# Create a new categorical variable - protocol type
# creates a counter for each type
catVar <- table(result$Protocol)

# Print the results
cat("Categorical Variable: Protocol:\n")
print(catVar)

# Create at least one new continuous/numerical variable
# Calculate average packet size for each dataset
avgPacketSizeTableOne <- mean(tableOne$Packet.Length)
avgPacketSizeTableTwo <- mean(tableTwo$Packet.Size)

# Calculate average packet size for both tables combined
avgPacketSizeBothTable <- mean(c(tableOne$Packet.Length, tableTwo$Packet.Size))

# Create at least one summarization data frame
# Summarize attack styles and find the maximum attack type
attackStyleSummary <- summarise(group_by(tableOne, Attack.Type), Count = n())

maxAttackType <- attackStyleSummary[which.max(attackStyleSummary$Count), "Attack.Type"]

# Print the results
cat("\nNew Continuous/Numerical Variable (Average Packet Size):\n")
cat("Average Packet Size in tableOne:", avgPacketSizeTableOne, "\n")
cat("Average Packet Size in tableTwo:", avgPacketSizeTableTwo, "\n")
cat("Overall Average Packet Size in Both Tables:", avgPacketSizeBothTable, "\n")

cat("\nSummarization Data Frame:\n")
print(attackStyleSummary)

cat("\nMost Common Attack Type:\n")
print(maxAttackType)
