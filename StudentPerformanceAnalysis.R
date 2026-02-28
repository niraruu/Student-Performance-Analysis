install.packages("tidyverse")
install.packages("caret")
install.packages("corrplot")
install.packages("ggplot2")

library(magrittr)
library(dplyr)
library(tidyverse)
library(caret)
library(corrplot)
library(ggplot2)

data <- read.csv("C:/Users/palla/OneDrive/Desktop/Datasets/StudentsPerformance.csv")
head(data)
str(data)
summary(data)

colSums(is.na(data))

data <- na.omit(data)
data <- data %>%
  mutate(across(where(is.character), as.factor))

num_cols <- sapply(data, is.numeric)
numeric_names <- names(data)[num_cols]
data[num_cols] <- scale(data[num_cols])

cor_matrix <- cor(data[, num_cols])
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

first_numeric <- numeric_names[1]
ggplot(data, aes(x = .data[[first_numeric]])) +
  geom_histogram(fill = "blue", bins = 30) +
  theme_minimal() +
  labs(title = paste("Histogram of", first_numeric),
       x = first_numeric,
       y = "Frequency")

ggplot(data, aes(y = .data[[first_numeric]])) +
  geom_boxplot(fill = "pink") +
  theme_minimal() +
  labs(title = paste("Boxplot of", first_numeric),
       y = first_numeric)

if(length(numeric_names) >= 2){
  x_var <- numeric_names[1]
  y_var <- numeric_names[2]
}
ggplot(data, aes(x = .data[[x_var]],
                 y = .data[[y_var]])) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = paste("Scatter Plot of", x_var, "vs", y_var),
       x = x_var,
       y = y_var)
