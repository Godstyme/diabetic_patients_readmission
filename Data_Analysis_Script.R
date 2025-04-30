install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("skimr")
install.packages("DataExplorer")
install.packages("corrplot")
install.packages("car")
install.packages("broom") 
install.packages("effsize")
install.packages("stringr") 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(corrplot)
library(car)
library(broom)
library(effsize)
library(stringr)

#load dataset
diabetes_data <- read_csv("C:/Users/Godstime/Desktop/Others/Bolton/Portfolio/Assignment_2/diabetic_data.csv")
diabetes_data


#display all the columns in the dataset
colnames(diabetes_data)
#select some columns
selected_data_column <- diabetes_data %>%
  select(encounter_id, patient_nbr, age, gender,time_in_hospital, number_diagnoses, num_lab_procedures,num_procedures,num_medications, insulin, readmitted)
selected_data_column
# first 20 row
head(selected_data_column, 20)
#summarize table 
glimpse(selected_data_column)

#check missing values
clean_missing_value <- is.na(selected_data_column)
#output missing value output
clean_missing_value
#summary of missing value
colSums(is.na(clean_missing_value))
#check duplicate value
duplicate_value <- duplicated(selected_data_column)
duplicate_value
#summarise duplicate value
sum(duplicated(selected_data_column))

#check if there is "?" in the dataset
sum(selected_data_column == "?", na.rm = TRUE)
#identify unsual gender
table(selected_data_column$gender)

# Keep only rows where gender is male or female
clean_data <- subset(selected_data_column, gender %in% c("Male", "Female"))
clean_data

#original row count
nrow(selected_data_column)
#new row count after filtration
nrow(clean_data)

#check for non-numbers in numeric column
numeric_cols <- sapply(clean_data, is.numeric)
numeric_cols
check_numeric_validity <- function(col) {
  suppressWarnings(sum(is.na(as.numeric(col))) > 0 & !is.numeric(col))
}

check_numeric_validity
invalid_numeric_cols <- sapply(clean_data, check_numeric_validity)
invalid_numeric_cols
names(invalid_numeric_cols[invalid_numeric_cols == TRUE])
# Check if all values in a column are numeric
all(suppressWarnings(!is.na(as.numeric(as.character(clean_data$num_procedures)))))
all(suppressWarnings(!is.na(as.numeric(as.character(clean_data$encounter_id)))))
all(suppressWarnings(!is.na(as.numeric(as.character(clean_data$patient_nbr)))))
all(suppressWarnings(!is.na(as.numeric(as.character(clean_data$time_in_hospital)))))
all(suppressWarnings(!is.na(as.numeric(as.character(clean_data$number_diagnoses)))))
all(suppressWarnings(!is.na(as.numeric(as.character(clean_data$num_lab_procedures)))))
all(suppressWarnings(!is.na(as.numeric(as.character(clean_data$num_medications)))))


#identification of outliers
id_columns <- c("encounter_id", "patient_nbr")
numeric_data <- clean_data[ , sapply(clean_data, is.numeric)]
numeric_data <- numeric_data[ , !(names(numeric_data) %in% id_columns)]
numeric_data

#Visualization of outliers using Boxplot
boxplot(numeric_data,
        main = "Boxplots of Numeric Variables (Excluding IDs)",
        col = "lightblue",
        las = 2,
        cex.axis = 0.8)
#identify outliers using ggplot
selected_data <- clean_data %>%
  select(num_lab_procedures, time_in_hospital, num_medications,number_diagnoses)
long_data <- pivot_longer(selected_data,
                          cols = everything(),
                          names_to = "variable",
                          values_to = "value")
#Visualization of outliers using ggplot
ggplot(long_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Boxplots of Selected Variables",
       x = "Variable",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

#Exploratory Analysis
#checking for the head, tail, dim of the data
head(clean_data)
tail(clean_data)
dim(clean_data)
skim(clean_data)
summary(clean_data)

#Visualisation and check distributions
ggplot(clean_data, aes(x = readmitted)) +
  geom_bar(fill = "tomato") +
  theme_minimal() +
  labs(title = "Hospital Readmission Status")

ggplot(clean_data, aes(x = age)) +
  geom_bar(fill = "green") +
  theme_minimal() +
  labs(title = "Distribution of Patient Age")

ggplot(clean_data, aes(x = insulin)) +
  geom_bar(fill = "yellow") +
  theme_minimal() +
  labs(title = "Distribution of Insulin")

ggplot(clean_data, aes(x = gender)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()

ggplot(clean_data, aes(x = num_lab_procedures)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Number of lab tests done Distribution", x = "Number of Lab Test", y = "Count") +
  theme_minimal()

ggplot(clean_data, aes(x = number_diagnoses)) +
  geom_bar(fill = "skyblue", color = "gray") +
  labs(title = "Number of Different Diagnosis Conducted", x = "Number of Diagnosis", y = "Count") +
  theme_minimal()


ggplot(clean_data, aes(x = "", y = time_in_hospital)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Time in Hospital")

# Detect extreme values manually
boxplot.stats(clean_data$time_in_hospital)$out

ggplot(clean_data, aes(x = num_medications, fill = readmitted)) +
  geom_histogram(bins = 30, position = "dodge") +
  theme_minimal() +
  labs(title = "Readmission by Number of Medications")


#create_report(clean_data)
#as.factor() to transform character to factor
clean_data$gender <- as.factor(clean_data$gender)
clean_data$gender
clean_data$insulin <- as.factor(clean_data$insulin)
clean_data$insulin 
clean_data$readmitted <- as.factor(clean_data$readmitted)
clean_data$readmitted 
#Normalize numeric features
clean_data$num_medications <- scale(clean_data$num_medications)
clean_data$num_medications
#skewed distributions
clean_data$num_lab_procedures <- log1p(clean_data$num_lab_procedures)
clean_data$num_lab_procedures


ggplot(clean_data, aes(x = "", y = number_diagnoses)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Number of Diagnosis")

#trend
ggplot(clean_data, aes(x = age, fill = readmitted)) +
  geom_bar(position = "dodge") +
  labs(title = "Readmission Counts by Age Group", 
       x = "Age Group", y = "Patient Count") +
  theme_minimal()

#mean and sd
numeric_data <- clean_data %>%
  select(where(is.numeric)) %>%
  select(-encounter_id, -patient_nbr)  
sapply(numeric_data[, sapply(numeric_data, is.numeric)], mean, na.rm = TRUE)
sapply(numeric_data[, sapply(numeric_data, is.numeric)], sd, na.rm = TRUE)

#Numeric relationship
numeric_data <- clean_data %>%
  select(where(is.numeric)) %>%
  select(-encounter_id, -patient_nbr)  
#plot.new()
#dev.off()


#Identification of Predictors for Readmission Rate with logistic regression
table(clean_data$readmitted)
unique(clean_data$readmitted)
clean_data$readmitted <- trimws(tolower(clean_data$readmitted)) 
clean_data$readmitted <- ifelse(clean_data$readmitted == "no", 0, 1)
table(clean_data$readmitted)

model_logistic <- glm(readmitted ~ age + gender + num_medications + number_diagnoses + insulin + time_in_hospital,
                      family = binomial(), data = clean_data)
summary(model_logistic)
exp(coef(model_logistic))



#mean between two groups with t-test
t_test_result <- t.test(time_in_hospital ~ readmitted, data = clean_data)
t_test_result
table(clean_data$readmitted)

clean_data$readmitted <- factor(clean_data$readmitted, levels = c("0", "1"), labels = c("Not_Readmitted", "Readmitted"))
cohen_d <- cohen.d(time_in_hospital ~ readmitted, data = clean_data)
cohen_d
table(clean_data$readmitted)
clean_data$readmitted <- as.numeric(clean_data$readmitted == "Readmitted")
table(clean_data$readmitted)

# Chi-Square Test for the relationship between insulin and readmission status
chi_square_result <- chisq.test(table(clean_data$insulin, clean_data$readmitted))
chi_square_result
# Create a contingency table for insulin vs readmitted
insulin_readmitted_table <- table(clean_data$insulin, clean_data$readmitted)
insulin_readmitted_prop <- prop.table(insulin_readmitted_table, 1)
print(insulin_readmitted_prop)




#Visualisation of insights

#visualization of logistic regression=
model_summary <- tidy(model_logistic, exponentiate = TRUE, conf.int = TRUE)


# Plot the odds ratios
ggplot(model_summary, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    title = "Odds Ratios for Readmission",
    x = "Predictor Variables",
    y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal()

#T-test Visualisation
ggplot(clean_data, aes(x = time_in_hospital, fill = factor(readmitted))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62"),
                    labels = c("Not Readmitted", "Readmitted")) +
  labs(
    title = "Density of Hospital Stay by Readmission Status",
    x = "Time in Hospital (Days)",
    fill = "Readmission Status"
  ) +
  theme_minimal()
table(clean_data$readmitted)

#chi square
ggplot(clean_data, aes(x = insulin, fill = factor(readmitted))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Readmission Rate by Insulin Usage",
    x = "Insulin Category",
    y = "Percentage of Patients",
    fill = "Readmitted"
  ) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62"),
                    labels = c("Not Readmitted", "Readmitted")) +
  theme_minimal()

#create_report(clean_data)


