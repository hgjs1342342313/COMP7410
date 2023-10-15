#This is a sample R script for assignment 1.

# Install packages/load libraries
install.packages('dplyr')
install.packages("tidyr")
install.packages("ggplot2")
install.packages('stats')
install.packages("caret")
install.packages("plotly")
install.packages("mice")
install.packages("corrplot")
install.packages("reshape2")
# library(package_name)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(plotly)
library(mice)
library(corrplot)
library(reshape2)
# ...
#Exploratory Data Analysis, e.g., Distinguish Attributes, Univariate Analysis 

# STEP0: Load the data
dataset <- read.csv("enron.csv")

# *********************************
# STEP1: Distinguish Attributes
# *********************************
str(dataset)
summary(dataset)
print("POI Table")
table(dataset$poi)
print("Number of 2 classes")
table(dataset$poi)/length(dataset$poi)
# ...

# *********************************
# STEP2: Univariate Analysis
# *********************************


# *****************************************
# *********************************
# 2.1.1 First pair of variables: from poi to this person & from this person to poi
# This pair of data counts the amount of emails between a specific person to the poi person
hist(dataset$from_poi_to_this_person)
hist(dataset$from_this_person_to_poi)


# *********************************
# 2.1.2 Second group of variables: salary, bonus, total stock value, and expenses
hist(dataset$salary)
hist(dataset$bonus)
hist(dataset$total_stock_value)
hist(dataset$expenses)


# *********************************
# 2.1.3 Third part of variables: deferral_payments, deferred_income and restricted_stock_deferred
hist(dataset$deferral_payments)
hist(dataset$deferred_income)
hist(dataset$restricted_stock_deferred)


# *********************************
# 2.1.4 The rest of the variables: to_messages, total_payments, loan_advances, exercised_stock_options, from_messages, other, long_term_incentive, director_fees, 
columns <- c("to_messages", "total_payments", "loan_advances", "exercised_stock_options", 
             "from_messages", "other", "long_term_incentive", "director_fees", "shared_receipt_with_poi", "restricted_stock")
for (col in columns) {
  hist(dataset[[col]], main = col, xlab = col)
}
# *****************************************


# *****************************************
# *********************************
# 2.2.1 Then, let's take a look of the details of the data above
hist(dataset$from_poi_to_this_person[dataset$from_poi_to_this_person < 100])
hist(dataset$from_this_person_to_poi[dataset$from_this_person_to_poi < 100])


# *********************************
# 2.2.2 Since we can see the outlier from salary, bonus, and total stock value, we can "zoom-in" the data
hist(dataset$salary[dataset$salary < 5e6])
hist(dataset$bonus[dataset$bonus < 1e7])
hist(dataset$total_stock_value[dataset$total_stock_value < 5e7])
hist(dataset$total_stock_value[dataset$total_stock_value < 5e6])
hist(dataset$expenses[dataset$expenses < 5e5])


# *********************************
# 2.2.3 Also, we can see the distribution of defer-relative data
hist(dataset$deferral_payments[dataset$deferral_payments < 5e6])
hist(dataset$deferred_income[dataset$deferred_income > -5e6])
hist(dataset$restricted_stock_deferred[dataset$restricted_stock_deferred > -5e6 & dataset$restricted_stock_deferred < 0])
# *****************************************


# 2.2.4 The rest distributions: to_messages, total_payments, loan_advances, exercised_stock_options, from_messages, other, long_term_incentive, director_fees, 
columns <- c("to_messages", "total_payments", "exercised_stock_options", "from_messages",
              "other", "long_term_incentive", "director_fees", "restricted_stock")
thresholds <- c(5000, 5e7, 5e7, 4000, 5e6, 5e6, 2e5, 2e7)

plot_histogram <- function(dataset, column, threshold) {
  data_subset <- dataset[[column]][dataset[[column]] < threshold]
  hist(data_subset, main = column, xlab = column)
}
for (i in 1:length(columns)) {
  plot_histogram(dataset, columns[i], thresholds[i])
}
hist(dataset$loan_advances[dataset$loan_advances < 2e7])
hist(dataset$loan_advances[dataset$loan_advances > 8e7])


# *****************************************
# *********************************
# 2.3 Barchart
# Simple bar chart for distributions: poi distribution
# ggplot(dataset, aes(x = factor(poi))) +
#        geom_bar()

# Advanced bar chart for distributions: poi distribution
ggplot(dataset, aes(x = factor(poi),
                    y = prop.table(stat(count)), fill = factor(poi),
                    label = scales::percent((prop.table(stat(count)))))) + 
        geom_bar(position="dodge") + 
        geom_text(stat = 'count', 
                  position = position_dodge(.9),
                  vjust = -0.5,
                  size = 3) + 
        scale_x_discrete(labels = c("False", "True")) + 
        scale_y_continuous(labels = scales::percent) + 
        labs(x = "Poi", y = "Percentage") + 
        ggtitle("Distribution of POI status")
# *****************************************


# *****************************************
# 2.4 Box plots

# *********************************
# 2.4.1
# Box plot for from poi to this person
ggplot(dataset, aes(y = from_poi_to_this_person)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "from_poi_to_this_person") + 
  ggtitle("Boxplot of from_poi_to_this_person by frequency")

# Box plot for from this person to poi
ggplot(dataset, aes(y = from_this_person_to_poi)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "from_this_person_to_poi") + 
  ggtitle("Boxplot of from_this_person_to_poi by frequency")


# *********************************
# 2.4.2
# Box plot for salary
ggplot(dataset, aes(y = salary)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "salary") + 
  ggtitle("Boxplot of salary by frequency")

# Box plot for bonus
ggplot(dataset, aes(y = bonus)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "bonus") + 
  ggtitle("Boxplot of bonus by frequency")

# Box plot for total stock value
ggplot(dataset, aes(y = total_stock_value)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "total_stock_value") + 
  ggtitle("Boxplot of total_stock_value by frequency")

# Box plot for expenses
ggplot(dataset, aes(y = expenses)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "Expenses") + 
  ggtitle("Boxplot of expenses by frequency")


# *********************************
# 2.4.3
# Box plot for deferral_payments
ggplot(dataset, aes(y = deferral_payments)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "deferral_payments") + 
  ggtitle("Boxplot of deferral_payments by frequency")

# Box plot for deferred_income
ggplot(dataset, aes(y = deferred_income)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "deferred_income") + 
  ggtitle("Boxplot of deferred_income by frequency")

# Box plot for restricted_stock_deferred
ggplot(dataset, aes(y = restricted_stock_deferred)) + 
  geom_boxplot() + 
  labs(x = "Frequency", y = "restricted_stock_deferred") + 
  ggtitle("Boxplot of restricted_stock_deferred by frequency")



# *********************************
# STEP3: Bi-/Multi-variate Analysis
# *********************************

# *****************************************
# *********************************
# 3.1 Bivariate Analysis: See distribution of features by poi/non-poi. 

# Code structure: [Selected Feature] Status of distributions


# [from_poi_to_this_person] Different distribution
ggplot(dataset, aes(x = from_poi_to_this_person, fill = factor(poi))) + 
    geom_histogram(bins = 100) + 
    labs(x = 'amount of from_poi_to_this_person', y = 'frequency') + 
    ggtitle('Distribution of fp2tp by poi') + 
    facet_grid(poi ~ ., scales = 'free_y')

# [from_this_person_to_poi] Similar distribution
ggplot(dataset, aes(x = from_this_person_to_poi, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'amount of from_this_person_to_poi', y = 'frequency') + 
  ggtitle('Distribution of ftp2p by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')

# [Salary] Similar distribution
ggplot(dataset, aes(x = salary, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'salary', y = 'frequency') + 
  ggtitle('Distribution of salary by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')

# [Bonus] Similar distribution
ggplot(dataset, aes(x = bonus, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'bonus value', y = 'frequency') + 
  ggtitle('Distribution of bonus by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')

# [total_stock_value] Different distribution
ggplot(dataset, aes(x = total_stock_value, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'total_stock_value value', y = 'frequency') + 
  ggtitle('Distribution of total_stock_value by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')

# [expenses] Similar distribution
ggplot(dataset, aes(x = expenses, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'expenses value', y = 'frequency') + 
  ggtitle('Distribution of expenses by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')

# [deferral_payments] Different distribution
ggplot(dataset, aes(x = deferral_payments, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'deferral_payments value', y = 'frequency') + 
  ggtitle('Distribution of deferral_payments by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')

# [deferred_income] Different distribution
ggplot(dataset, aes(x = deferred_income, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'deferred_income value', y = 'frequency') + 
  ggtitle('Distribution of deferred_income by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')

# [restricted_stock_deferred] Different distribution
ggplot(dataset, aes(x = restricted_stock_deferred, fill = factor(poi))) + 
  geom_histogram(bins = 100) + 
  labs(x = 'restricted_stock_deferred value', y = 'frequency') + 
  ggtitle('Distribution of restricted_stock_deferred by poi') + 
  facet_grid(poi ~ ., scales = 'free_y')


# *********************************
# 3.2 Multivariate analysis
# From 3.1, we can have potional useful features which are: from_poi_to_this_person, total_stock_value, deferral_payments, 
#                                                           deferred_income, and restricted_stock_deferred

#cor(x, y, use = "everything", method = "pearson")
selected_features <- dataset[, c("from_poi_to_this_person", "total_stock_value", "deferral_payments", "deferred_income", "restricted_stock_deferred")]
corr_mat <- cor(selected_features, use = "complete.obs", method = "pearson")
corrplot(corr_mat, method="number")
corrplot(corr_mat, method="color")
# Also, we could remove the features which contain many nans to plot the corr_matrix
selected_features <- subset(dataset, select = -c(X, poi, email_address, deferred_income, deferral_payments, director_fees, loan_advances, long_term_incentive, restricted_stock_deferred))
corr_mat <- cor(selected_features, use = "complete.obs", method = "pearson")
corrplot(corr_mat, method="number")
corrplot(corr_mat, method="color")

# *********************************
# STEP4: Analysis missing value 
# *********************************

# Get percentage of missing value
missing_percent <- colMeans(is.na(dataset)) * 100

# Make histogram of missing value
ggplot(data.frame(variable = names(dataset), missing_percent), aes(x = variable, y = missing_percent)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(x = "Variables", y = "Missing Value Percentage") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_cartesian(ylim = c(0, 100))

# *********************************
# STEP5: Outlier detection 
# *********************************

# Bivariate and multivariate outliers: See the histogram above

# Univarite outliers(take salary as an example)
# Compute IQR
q3 <- quantile(dataset$salary, probs = 0.75, na.rm = TRUE) # Ignore NA values
q1 <- quantile(dataset$salary, probs = 0.25, na.rm = TRUE)
iqr <- q3 - q1
print(iqr)
threshold <- 1.5 * iqr
lower_bound <- q1 - threshold
upper_bound <- q3 + threshold
outliers <- dataset$salary[dataset$salary < lower_bound | dataset$salary > upper_bound]
print(outliers)
# Bi-/Multi-varite outliers could be seen in plots above