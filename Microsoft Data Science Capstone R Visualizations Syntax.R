#Lab 1:  Intro to Graphing in R
#Class:  DSC 510
#Author: Stephanie Besser
#Date:   October 26, 2020

#Using Microsoft Data Science Capstone Training Datasets

#Note: Run Shortcut:  CTRL+Enter


#Y-Variable:  heart_disease_mortality_per_100k

#Libraries
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(DescTools) #VIF Function
library(leaps) #Best Set Linear Regression Functions
##############################################################################################

#Set Working Directory
setwd("C:/Users/sbess/Downloads")

#Read in Datasets

training_values <- read.csv(file="Training_values.csv", header=TRUE, sep=",")

training_labels <- read.csv(file="Training_labels.csv", header=TRUE, sep=",")

#Check Sample Size and Number of Variables
dim(training_values)
#3,198-Sample Size and 34 variables

dim(training_labels)
#3,198-Sample Size and 2 variables

#Show for first 6 rows of data
head(training_values)

head(training_labels)


#Show last 6 rows of data
tail(training_values)

tail(training_labels)

################################################################################################
#Merge Datasets

training <- merge(training_labels, training_values, by = c("row_id"))

dim(training)
#3,198-Sample Size and 35 variables


#Show the structure of the data

str(training)


#Show names of the variables

names(training)
#################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(training))
#4,536 total missing

#For 1 Variable
sum(is.na(training$heart_disease_mortality_per_100k))
#No Observations Missing

sum(is.na(training$econ__pct_uninsured_adults))
#Two Observations Missing


#Treat Missing Values

#Listwise Deletion
training2 <- na.omit(training)

#Check new data has no missing data
sum(is.na(training2))


#################################################################################################################

#Show column Numbers

library(psych)
describe(training2)

#Create new subsets of data

training3 <- training2[,c(2,6:34)]

str(training3)

econ <- training3[,1:4]
demo <- training3[,5:18]
health <- training3[,19:29]
 
#Show descriptive statistics


library(Hmisc)
describe(training3)


library(psych)
describe(training3)

#############################################################################################################

#Exploratory Analysis Graphing

#library(GGally)
#GGpairs
ggpairs(econ)

#Note: Does not show boxplots or bar charts, because there is no categorical variable in the econ dataset

#Histograms
hist(training3$heart_disease_mortality_per_100k, col="white", freq=FALSE)
x <- seq(0, 500, length.out=50)
y <- with(training3, dnorm(x, mean(heart_disease_mortality_per_100k), sd(heart_disease_mortality_per_100k)))
lines(x, y, col = "red")

library(ggplot2)
#GGPlot Histogram with Normal Curve
x <- seq(0, 500, length.out=50)
df <- with(training3, data.frame(x = x, y = dnorm(x, mean(heart_disease_mortality_per_100k), sd(heart_disease_mortality_per_100k))))

p <-ggplot(training3) + geom_histogram(aes(x=heart_disease_mortality_per_100k, y = ..density..), binwidth=50, colour="black", fill="green") +
  labs(
    title="Heart Disease Mortality (per 100K)",
    y="Density",
    x="Heart Disease Mortality (per 100K)")

p

p1 = p+geom_line(data = df, aes(x = x, y = y), color = "red")
p1

# To save the ggplot as png
ggsave("p1.png")

#Boxplots
boxplot(training3$heart_disease_mortality_per_100k, col = "blue", main="Heart Disease Mortality (per 100K)", 
        ylab="Heart Disease Mortality (per 100K)" )

#GG Boxplot
p2<-ggplot(training2, aes(y=heart_disease_mortality_per_100k)) +
  geom_boxplot(col="blue") +
  labs(
    title="Heart Disease Mortality (per 100K)",
    y="Heart Disease Mortality (per 100k)")
p2

#Five-Number Summary for Boxplot
summary(training3$heart_disease_mortality_per_100k)


#Boxplot by Grouping Variable
boxplot(heart_disease_mortality_per_100k~yr,data=training2, main="Heart Disease Mortality (per 100K)", xlab="Year",
        ylab="Heart Disease Mortality (per 100K)", col = c("blue","green"))


#GGplot Box Plot by Group
p3<-ggplot(training2, aes(x=yr, y=heart_disease_mortality_per_100k, color=yr)) +
  geom_boxplot()+ylim(0,500) +theme_bw() +
  labs(
    title="Heart Disease Mortality (per 100K)",
    y="Heart Disease Mortality (per 100k)",
    x="Year")
p3


#Bar Plot
counts <- table(training2$yr)
barplot(counts, main="Number of Counties per Year",ylab="Number of Counties", xlab="Year", col=c("blue","green"))

#GGPlot Bar Plot
p4 <-ggplot(data=training2, aes(x=yr, y=heart_disease_mortality_per_100k, fill=yr)) +
  geom_bar(stat="identity") + theme_minimal() +guides(fill=guide_legend(title="Year")) +
  labs(
    title="Number of Counties per Year",
    y="Number of Counties",
    x="Year")
p4


#Violin Plots
library(vioplot)
x1 <- training2$heart_disease_mortality_per_100k[training2$yr=="a"]
x2 <- training2$heart_disease_mortality_per_100k[training2$yr=="b"]

vioplot(x1, x2, names=c("a", "b"), 
        col="green")
title("Violin Plots of Heart Mortality (per 100K)")
ylab("Heart Mortality (100K")


#GGplot Violin Plot
p5<-ggplot(training2, aes(x=yr, y=heart_disease_mortality_per_100k, fill=yr)) +
  geom_violin(trim=FALSE) +
  guides(fill=guide_legend(title="Year")) +
  labs(
    title="Heart Disease Mortality (per 100K)",
    y="Heart Disease Mortality (per 100k)",
    x="Year")
p5


#Check for Multicollinearity with Correlations


M<-cor(training3, method="spearman")
M

corrplot(M, method = "square")

#econ
corrplot(cor(econ,method="spearman"), method = "number")
#drop:  econ__pct_uninsured_children

#GGplot Correlation
ggcorr(econ)

#demo
corrplot(cor(demo,method="spearman"), method = "number")
#drop:  demo__birth_rate_per_1k, demo__death_rate_per_1k, demo__pct_adults_less_than_a_high_school_diploma, 
#demo__pct_adults_with_high_school_diploma

#health
corrplot(cor(health,method="spearman"), method = "number")
#drop:health__pct_physical_inacticity, health__pct_adult_obesity 



