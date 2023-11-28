library(dplyr)

# Read the CSV files
tableOne <- read.csv("cybersecurity_attacks.csv")
tableTwo <- read.csv("cyberthreat.csv")

# Merge the two different files based on the common column titled "Protocol"
result <- merge(tableOne, tableTwo, by = "Protocol", all = TRUE)

# Create a new categorical variable - protocol type
# creates a counter for each type
catVar <- table(result$Protocol)

# Add the new categorical variable to the result dataset
result <- mutate(result, CatVariable = catVar[match(result$Protocol, names(catVar))])

# Create at least one new continuous/numerical variable
# Calculate average packet size for each dataset
avgPacketSizeTableOne <- mean(tableOne$Packet.Length)
avgPacketSizeTableTwo <- mean(tableTwo$Packet.Size)

# Calculate average packet size for both tables combined
avgPacketSizeBothTable <- mean(c(tableOne$Packet.Length, tableTwo$Packet.Size))

# Add the new continuous/numerical variables to the result dataset
result <- mutate(result,
                 AvgPacketSizeTableOne = avgPacketSizeTableOne,
                 AvgPacketSizeTableTwo = avgPacketSizeTableTwo,
                 AvgPacketSizeBothTable = avgPacketSizeBothTable
)

# Create at least one summarization data frame
# Summarize attack styles and find the maximum attack type
attackStyleSummary <- summarise(group_by(tableOne, Attack.Type), Count = n())

# Add the Count variable to the result dataset
result <- left_join(result, attackStyleSummary, by = "Attack.Type")

# Randomly sample 20,000 rows instead of first 20000 rows
result <- result %>%
  sample_n(20000)

# View the updated result
print(result)
