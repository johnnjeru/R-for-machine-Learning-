## This is r script that is used for analysis and exploration of our data
## first we need to load some of 
library(tidyverse)
library(ggplot2)
library(stringr)

## loading our data 

data <- read.csv(file.choose(), header = T)

## our data is loaded as variable data , has 42538 obs and 116 variables 

## lets view our data 

view(data)

dim(data) # checking for the shape of the data 

duplicated(data) # there are no duplicates in our data 

 # DATA CLEANING
data$int_rate <- str_replace(data$int_rate, "%", " ") ## replacing the % with white space 
data$int_rate <- as.numeric(data$int_rate) ## changing the data type to numeric
class(data$int_rate) == "numeric"
typeof(data$int_rate) # checking the new data type 

## filling missing values 


## using tidyverse to select only our interest of variables 

numeric <- data%>%select(int_rate, loan_amnt, installment, annual_inc)


head(numeric)
## a code to compute missing values using mean 
 
for(i in 1:ncol(numeric)){
  
  numeric[, i][is.na(numeric[, i])] <- mean(numeric[, i], na.rm = TRUE)
}

head(numeric) ## checking a few obs 

summary(numeric$int_rate) ## summary statistics for interest rates 
summary(numeric$installment) ## summary statistics for funded amount
summary(numeric$annual_inc) ## summary statistics for annual income

#numeric%>%summarise(mean(installment)) # computing the mean 
#numeric%>%summarise(mean(loan_amnt))
#numeric%>%summarise(max(loan_amnt))
#numeric%>%summarise(min(loan_amnt))
#numeric%>%summarise(max(installment))

## histogram for installment

ggplot(numeric, aes(x = installment))+
  geom_histogram()+
  ggtitle("Histogram for installment amount")+
  geom_freqpoly()

## histogram for interest rates 
ggplot(numeric, aes(x = int_rate))+
  geom_histogram()+
  ggtitle("Histogram for interest rates")
  
## histogram for annual income 
ggplot(numeric, aes(x = annual_inc))+
  geom_histogram()+
  ggtitle("Histogram for annual income")


## selecting our discrete variables of interest 

discrete <- data%>%select(term, grade, emp_length, home_ownership, loan_status)

## plotting 

ggplot(discrete, aes(term))+
  geom_bar()+
  ggtitle("Count bar graph for term of loan")
  
## bar graph for grade

ggplot(discrete, aes(grade))+
  geom_bar()+
  ggtitle("Count bar graph for grade")
## bar graph for home ownership
ggplot(discrete, aes(home_ownership))+
  geom_bar()+
  ggtitle("Count bar graph for home ownership")

## BAR graph for loan status
ggplot(discrete, aes(loan_status))+
  geom_bar()+
  ggtitle("Count bar graph for loan status")+
  theme(axis.text.x = element_text(angle = 25))


## Bivariate analysis 

#loan term and interest rate
ggplot(discrete, aes(x = discrete$term, y = numeric$int_rate))+
  geom_boxplot()+
  ggtitle("Loan term and interest rates")

## employment length and loan amount

ggplot(discrete, aes(x = discrete$emp_length, y = numeric$loan_amnt))+
  geom_boxplot()+
  ggtitle("Loan amount and employment length")


ggplot(numeric, aes(x = numeric$loan_amnt, y = numeric$annual_inc))+
  geom_point()+
  ggtitle(" Scatterplot for annual income and loan amount")

## Preprocessing the target variable to make it binary
## All other variables will be considered as charged and hence defaulted
data[data == "Current"] = "Charged Off"
data[data == "Default"] = "Charged Off"
data[data == "Does not meet the credit policy. Status:Charged Off"] = "Charged Off"
data[data == "Does not meet the credit policy. Status:Fully Paid"] = "Charged Off"
data[data == "In Grace Period"] = "Charged Off"
data[data == "Late (16-30 days)"] = "Charged Off"
data[data == "Late (31-120 days)"] = "Charged Off"

## plotting the cleaned data for loan status 
ggplot(data, aes(loan_status))+
  geom_bar()+
  ggtitle("Bar graph for the loan status")+
  theme(axis.text.x = element_text(angle = 25, colour = "blue"))
  



## putting variables in one dataframe i.e both discrete and continous 

new_df <-  data %>% select(loan_status, term, annual_inc, grade, int_rate)
view(new_df)

## attaching the new df
attach(new_df)

## fiiting a logistic regression model 
model <- glm(factor(loan_status) ~ term+annual_inc+grade+int_rate, family = binomial(link = "logit"))

summary(model) ## summary of the model 



## machine learning 
library(caTools)
library(party)
library(dplyr)
library(MASS)
library(magrittr)
library(tree)

decision_tree <- ctree(factor(loan_status) ~ factor(term)+int_rate+annual_inc+factor(grade))
plot(decision_tree) ##plotting the decision tree model for interpretation

#sample_data = sample.split(numeric, SplitRatio = 0.8)

#train_data <- subset(numeric, sample_data == TRUE)
#test_data <- subset(numeric, sample_data == FALSE)


## we can predict the amount of funds that will be funded 

















































