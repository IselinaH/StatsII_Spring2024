# Load necessary libraries
library(ggplot2)
library(dplyr)

update.packages(ask = FALSE, checkBuilt = TRUE)

install.packages("cli")

# Set the path to the dataset
dataset_path <- "/Users/iseli/Documents/GitHub/StatsII_Spring2024/replication/DataOriginalStudies.csv"

# Read the dataset
data <- read.csv(dataset_path)

# Read the dataset - Again due to error-
data <- read.csv("/Users/iseli/Documents/GitHub/StatsII_Spring2024/replication/DataOriginalStudies.csv", header=TRUE)

#Read data - Again due to another error
data <- read.csv("/Users/iseli/Documents/GitHub/StatsII_Spring2024/replication/DataOriginalStudies.csv", header=TRUE, sep=";")
head(data)

#Add column names
colnames(data) <- c("Gender", "Time_pressure", "Experiment", "Dice_report", "Report_time", "Within_time", "Cheerful", "active", "active2", "happy", "loving", "worried", "tired", "tired2", "angry", "calm", "gloomy", "bored", "sad", "tense", "angry2", "satisfied", "pos_mood", "neg_mood", "filter_$")

# Filter data for Experiment 1 -ERROR while running this code-
data_exp1 <- filter(data, Experiment == 1)

# Load necessary libraries - Agian due to error
library(dplyr)

# Set the path to the dataset
dataset_path <- "/Users/iseli/Documents/GitHub/StatsII_Spring2024/replication/DataOriginalStudies.csv"

# Filter data for Experiment 1 using column index
data_exp1 <- filter(data, data[,3] == 1)

# Load necessary libraries -Just double checking due to errors-
library(ggplot2)
library(dplyr)

# Assuming data_summary is already created and contains 'Time_pressure' and 'RollCategory'

# Convert 'Time_pressure' to a factor with labels 'Low' and 'High' within data_summary
data_summary$Time_pressure <- factor(data_summary$Time_pressure, levels = c(0, 1), labels = c("Low", "High"))

#MORE ERRORS
# Assuming data_exp1 is already filtered for Experiment 1

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Calculate proportions
# Assuming 'Time_pressure' is in the 2nd column and 'Dice_report' is in the 4th column
data_summary <- data_exp1 %>%
  mutate(RollCategory = ifelse(Dice_report <= 3, "1 to 3", "4 to 6")) %>%
  group_by(Time_pressure = data_exp1[,2], RollCategory) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

# Convert 'Time_pressure' to a factor with labels 'Low' and 'High'
data_summary$Time_pressure <- factor(data_summary$Time_pressure, levels = c(0, 1), labels = c("Low", "High"))

# Now plot using the corrected data_summary
ggplot(data_summary, aes(x = Time_pressure, y = Proportion, fill = RollCategory)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Proportion of Reported Die Rolls by Time Pressure",
       x = "Time Pressure",
       y = "Proportion",
       fill = "Roll Category") +
  theme_minimal() +
  scale_fill_manual(values = c("1 to 3" = "blue", "4 to 6" = "green")) +
  geom_errorbar(aes(ymin = Proportion - 0.05, ymax = Proportion + 0.05), width = .2,
                position = position_dodge(.9))

##############################
#ALTERNATIVE OUTCOME BASED ON GENDER
##############################

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set the path to the dataset
dataset_path <- "/Users/iseli/Documents/GitHub/StatsII_Spring2024/replication/my_answers/DataOriginalStudies.csv"

# Read the dataset
data <- read.csv(dataset_path, header=TRUE, sep=";")

# Filter data for Experiment 1 using column index
data_exp1 <- filter(data, data[,3] == 1)

# Calculate proportions including Gender -MAJOR ERROR AGAIN-
data_summary <- data_exp1 %>%
  mutate(RollCategory = ifelse(data[,4] <= 3, "1 to 3", "4 to 6"),
         Gender = factor(data[,1], levels = c(1, 2), labels = c("Female", "Male"))) %>%
  group_by(Gender, Time_pressure = data[,2], RollCategory) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

#Redoing due to non stopping errors
# Assuming data_exp1 is already filtered for Experiment 1

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Calculate proportions including Gender
data_summary <- data_exp1 %>%
  mutate(RollCategory = ifelse(Dice_report <= 3, "1 to 3", "4 to 6"),
         Gender = factor(Gender, levels = c(1, 2), labels = c("Female", "Male"))) %>%
  group_by(Gender, Time_pressure, RollCategory) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

# Convert 'Time_pressure' to a factor with labels 'Low' and 'High'
data_summary$Time_pressure <- factor(data_summary$Time_pressure, levels = c(0, 1), labels = c("Low", "High"))

# Now plot using the corrected data_summary with Gender included
ggplot(data_summary, aes(x = Time_pressure, y = Proportion, fill = RollCategory)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~Gender) + # Add a facet for Gender
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Proportion of Reported Die Rolls by Time Pressure and Gender",
       x = "Time Pressure",
       y = "Proportion",
       fill = "Roll Category") +
  theme_minimal() +
  scale_fill_manual(values = c("1 to 3" = "blue", "4 to 6" = "green")) +
  geom_errorbar(aes(ymin = Proportion - 0.05, ymax = Proportion + 0.05), width = .2,
                position = position_dodge(.9))

