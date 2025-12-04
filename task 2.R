###############################################
# TITANIC DATA CLEANING + EDA (FULL SCRIPT)
###############################################

# Install and load libraries
packages <- c("tidyverse", "dplyr", "ggplot2", "skimr", "corrplot")
install.packages(setdiff(packages, rownames(installed.packages())))
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)
library(corrplot)

# =============================
# LOAD DATASET
# =============================
titanic <- read.csv("train.csv")  # <-- update with full path if needed

# =============================
# 1. Data Overview
# =============================
print("---- Data Preview ----")
head(titanic)
str(titanic)
skim(titanic)

# =============================
# 2. Data Cleaning
# =============================
print("---- Missing Values Count ----")
print(colSums(is.na(titanic) | titanic == ""))

# Fix missing Age
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# Fix missing Embarked (fill with mode)
titanic$Embarked[titanic$Embarked == "" | is.na(titanic$Embarked)] <-
  names(sort(table(titanic$Embarked), decreasing = TRUE))[1]

# Remove Cabin column
titanic <- titanic %>% select(-Cabin)

# Confirm missing data handled
print("---- Missing After Cleaning ----")
print(colSums(is.na(titanic)))

# =============================
# 3. EDA Visualizations
# =============================
ggplot(titanic, aes(x = factor(Survived))) +
  geom_bar() +
  labs(title = "Survival Count")

ggplot(titanic, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Gender")

ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Passenger Class")

ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 40) +
  labs(title = "Age Distribution")

ggplot(titanic, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point() +
  labs(title = "Age vs Fare by Survival")

# =============================
# 4. Correlation Analysis
# =============================
corr_data <- titanic %>% select_if(is.numeric)
corr_matrix <- cor(corr_data)
corrplot(corr_matrix, method = "color")

print("---- EDA Completed Successfully ----")
