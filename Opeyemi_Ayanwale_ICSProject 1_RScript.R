# COURSE: Introductory to Case Study
# PROJECT TITLE: Descriptive Analysis of Demographic Data (Project 1)
# DATE CREATED: 18 October 2021 # To be submitted by 12 November 2021 #
# InSTRUCTOR: Pof.Dr. Sonja Kuhnt, Dr.Paul Wiemann, Dr.Birte Hellwig, M.sc. Hendrik Dohme
# SCRIPT OWNER: Opeyemi Ayanwale (236783)
# PROJECT GROUP: Group 7
# Last changes:05/11/2021
# Submission Date: 08/11/2021

#############################
# R SETUP
#############################

## Download relevant R packages with the following command:
#install.packages()
install.packages(c("ggplot2", "data.table", "dplyr", "tidyverse"))

# 1.	Load the R package needed and with library()
#library()
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse)

#############################
# DATA SETUP
#############################

# 2. Download the dataset "Census_Data" and store it locally in a directory.
#    Set your working directory to this directory with setwd().
getwd()

#setwd("___")
setwd("C:/Users/opeye/OneDrive/Desktop/TU_Dortmund_DataScience/Intro_Case Studies/Census_Data")

# 3. Import the data using the read.csv function and store it in a variable named census_data#
census_data <- read.csv("census_2021_2001.csv")

# 4. Check whether the dataset was correctly loaded:
#    to view the whole dataset: View() #
View(census_data)
str(census_data)
names(census_data)
head(census_data)

# 5. Check for missig values
# to know how many missing value in the variable: sum(is.na()) #
sum(is.na(census_data))
sum(is.na(census_data$country))
sum(is.na(census_data$genc))
sum(is.na(census_data$subregion))
sum(is.na(census_data$region))
sum(is.na(census_data$year))
sum(is.na(census_data$total.fertility.rate))
sum(is.na(census_data$life.expectancy.both.sexes))
sum(is.na(census_data$life.expectancy.males))
sum(is.na(census_data$life.expectancy.females))

# without having to count d missing value we can also
which(is.na(census_data$genc)) # number 281 & 282 is our missing value 
#but its actually not a missing data but an abbr  NA for Nambia.

which(is.na(census_data$total.fertility.rate)) # number 235 325 379 385 393 429 is our missing value

which(is.na(census_data$life.expectancy.both.sexes)) # number 235 325 379 385 393 429 is our missing value

which(is.na(census_data$life.expectancy.males)) # number 235 325 379 385 393 429 is our missing value

which(is.na(census_data$life.expectancy.females)) # number 235 325 379 385 393 429 is our missing value

#6. Data Cleanup: Remove NA rows in R and replacing NA in genc Nambia with NM
# Replacing the abbrevation of Nambia(NA) in column genc with (NM) 
census_data$genc[is.na(census_data$genc)] <- "NM"
View(census_data)
View(census_data$genc)

# Re-check for missing values
sum(is.na(census_data$genc)) # no more missing value here as NA which stands for Nambia has been replace with NM
sum(is.na(census_data))

# Removing Rows with NA na.omit()
census_data2 <- na.omit(census_data)
View(census_data2)
summary(census_data2)
#7. Filter Data for year 2021 
census_data2021 <- filter(census_data2, census_data2$year == 2021)

View(census_data2021)
str(census_data2021)
head(census_data2021)
census_data2021[1:10, ]
summary(census_data$country)


#############################
# DESCRIPTIVE STATISTICS
#############################

#8. QUESTION 1: Descriptive statistics for variables ######## for task 1 #######

summary(census_data2021) # basic descriptive for all variables
sd(census_data2021$life.expectancy.males)
sd(census_data2021$life.expectancy.females)
sd(census_data2021$life.expectancy.both.sexes)
sd(census_data2021$total.fertility.rate)

#for more detailed descriptive, the "psych" package is required
install.packages("psych") # required only the first time
library(psych)
help(psych) # is also used for factor analysis

describe(census_data2021) # gives more detail descriptive for all variables


#9. Frequency tables for categorical data
table(census_data2021$subregion)
table(census_data2021$region)


#10. Data visualization

#GGPLOT: Histogram (Single numeric variables)
#ggplot(data, aes(num)) + 
#geom_histogram(binwidth/bins = 10, color= 'black', fill= 'light blue', alpha= 5) +
#xlab('Number') + ylab('Frquency/count') + ggtitle('Histogram')


ggplot(census_data2021, aes(life.expectancy.both.sexes)) + 
  geom_histogram(aes(y=..density..), binwidth = 10, color= 'black', fill= 'grey', alpha= 5) +
  xlab('Life Expectancy Both Sexes') + ylab('Density') + ggtitle('Life Expectancy Male & Female')

ggplot(census_data2021, aes(life.expectancy.females)) + 
  geom_histogram(aes(y=..density..), bins = 10, color= 'black', fill= 'grey', alpha= 5) +
  xlab('Life Expectancy Female') + ylab('Density') + ggtitle('Distribution of Female Life Expectancy')

ggplot(census_data2021, aes(life.expectancy.males)) + 
  geom_histogram(aes(y=..density..), bins = 10, color= 'black', fill= 'grey', alpha= 5) +
  xlab('Life Expectancy Males') + ylab('Density') + ggtitle('Distribution of Male Life Expectancy')

ggplot(census_data2021, aes(total.fertility.rate)) + 
  geom_histogram(aes(y=..density..), bins = 10, color= 'black', fill= 'grey', alpha= 5) +
  xlab('Fertility Rate') + ylab('Density') + ggtitle('Total Fertility Rate')



######### Let see the distribution of fertility rate by subregion and region ##########
ggplot(census_data2021, aes(total.fertility.rate)) + 
  geom_density() +
  facet_wrap(~region) +
  theme_bw()

ggplot(census_data2021, aes(total.fertility.rate)) + 
  geom_density() +
  facet_wrap(~subregion) +
  theme_bw()



##Barplot for categorical variable
barplot(table(census_data2021$region))   #### OR using GGPLOT #####

ggplot(census_data2021, aes(fct_infreq(region))) +
  geom_bar(fill = "#9FE2BF") +
  xlab("Region") + ggtitle("Distribution of Region")

ggplot(census_data2021, aes(fct_infreq(subregion))) +
  geom_bar(fill = "#E9967A") +
  coord_flip() +
  xlab("Subregion") + ggtitle("Distribution of Subregion")

ggsave()



#11. QUESTION 2:  Correlation ####### for task 2 ############
attach(census_data2021) #so that i can mention a variable without writing the data

cor(total.fertility.rate, life.expectancy.females) 

cor(total.fertility.rate, life.expectancy.males)

cor(total.fertility.rate, life.expectancy.both.sexes)



#12. Data visualization

#Exploring correlation between Life Expectancy and Fertility Rate for both sexes
plot(total.fertility.rate, life.expectancy.both.sexes, main="Life Expectancy vs Fertility Rate in Both Sexes",
     xlab="Total Fertility Rate", ylab="Life Expectancy - Both Sexes", pch=20)  


#Exploring correlation between Life Expectancy for male and Fertility Rate 
plot(total.fertility.rate, life.expectancy.males, main="Life Expectancy vs Fertility Rate in Male",
     xlab="Total Fertility Rate", ylab="Life Expectancy - Male", pch=20)


#Exploring correlation between Life Expectancy for female and Fertility Rate
plot(total.fertility.rate, life.expectancy.females, main="Life Expectancy vs Fertility Rate in Female",
     xlab=" Total Fertility Rate", ylab="Life Expectancy - Female", pch=20)



#13. QUESTION 3:  Variability of the values in the individual subregions #task 3#

#################Using the ggplot (method 1)#####################
#Boxplot of Life Expectancy by Subregion
ggplot(census_data2021, aes(life.expectancy.both.sexes, subregion, fill=subregion)) +
  geom_boxplot() +
  xlab("Life Expectancy Both Sexes") + ggtitle("Life Expectancy by Subregion")


#Boxplot of Fertility Rate by Subregion
ggplot(census_data2021, aes(total.fertility.rate, subregion, fill=subregion)) +
  geom_boxplot() +
  xlab("Total Fertility Rate") + ggtitle("Fertility Rate by Subregion")


#################OR Using the basic R plot (method 2)###############
#Boxplot of Life Expectancy by Subregion
boxplot(life.expectancy.both.sexes~subregion,data=census_data2021, main="Life Expectancy by Subregion",
        xlab="Life Expectancy Both Sexes", ylab="", horizontal=TRUE,las=1)


#Boxplot of Fertility Rate by Subregion
boxplot(total.fertility.rate~subregion,data=census_data2021, main="Fertility Rate by Subregion",
        xlab="Total Fertility Rate", ylab="", horizontal=TRUE,las=1)


#14. QUESTION 4:  comparing 2001 with 2021?  #task 4#
#Boxplot comparing Life Expectancy by Year
boxplot(life.expectancy.both.sexes~year,data=census_data2, main="Life Expectancy by Year",
        xlab="Year", ylab="Life Expectancy - Both Sexes")

boxplot(life.expectancy.males~year,  data=census_data2, main="Life Expectancy Male by Year",
        xlab="Year", ylab="Life Expectancy Male")

boxplot(life.expectancy.females~year,  data=census_data2, main="Life Expectancy Male by Year",
        xlab="Year", ylab="Life Expectancy Female")

#Boxplot comparing Fertility Rate by Year
boxplot(total.fertility.rate~year,data=census_data2, main="Fertility Rate by Year",
        xlab="Year", ylab="Fertility Rate")

