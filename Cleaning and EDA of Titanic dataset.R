## Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(skimr)
#install.packages("ggcorrplot")
library(ggcorrplot)

## Read the data
data <- read.csv("C:\\Users\\USER\\Documents\\Futureinterns internship\\train.csv")

## Clean the data
data_clean <- na.omit(data)
data_clean$Survived <- factor(data_clean$Survived)
data_clean$Pclass <- factor(data_clean$Pclass)
data_clean$Sex <- factosr(data_clean$Sex)
data_clean$Embarked <- factor(data_clean$Embarked)

## Univariate Analysis
# Histogram for Age
ggplot(data_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age", x = "Age", y = "Count") +
  theme_minimal()

# Bar plot for Survived
ggplot(data_clean, aes(x = Survived)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Count of Survival", x = "Survived", y = "Count") +
  theme_minimal()

## Bivariate Analysis
# Scatter Plot for Age vs. Fare
ggplot(data_clean, aes(x = Age, y = Fare)) +
  geom_point(aes(color = Survived), alpha = 0.6) +
  labs(title = "Scatter Plot of Age vs. Fare", x = "Age", y = "Fare") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"), labels = c("Not Survived", "Survived"))

# Boxplot for Fare by Pclass
ggplot(data_clean, aes(x = Pclass, y = Fare)) +
  geom_boxplot(aes(fill = Pclass)) +
  labs(title = "Boxplot of Fare by Passenger Class", x = "Passenger Class", y = "Fare") +
  theme_minimal()

## Multivariate Analysis
# Facet Grid for Age vs. Fare by Survived
ggplot(data_clean, aes(x = Age, y = Fare)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Survived) +
  labs(title = "Scatter Plot of Age vs. Fare by Survival Status", x = "Age", y = "Fare") +
  theme_minimal()

# Correlation Matrix
corr_matrix <- cor(data_clean %>% select(Age, Parch, Fare, SibSp), use = "complete.obs")
ggcorrplot(corr_matrix, lab = TRUE, title = "Correlation Matrix")


