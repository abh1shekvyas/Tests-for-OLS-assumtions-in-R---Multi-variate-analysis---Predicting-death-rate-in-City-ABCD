#reading the excel file

install.packages("readxl")
library("readxl")
library("dplyr")
library("magrittr")
data <- read_xlsx("C:/Users/dell/Desktop/Sem 2/Multivariate Predictive Analysis I/assignment/xyz.xlsx",sheet=1)
View(data)
summary(data)

# checking for assumptions
# no null values detected
which(is.na(data))

#checking for outliers
boxplot(data)
#we can see some columns have the outliers which are to be taken care of once selected for the model

#checking for normality of dependant variables
hist(data$P)
#it can be said that there is a certain normality in the depandant variable

#Forming new table with relevant columns
data1 <- data %>% select(P,H,I,L)



#Create a boxplot that labels the outliers  
# install the package 
#install.packages("ggstatsplot")
# Load the package
#library(ggstatsplot)
#ggbetweenstats(warpbreaks, wool, breaks, outlier.tagging = TRUE)
#?ggbetweenstats

#treating outliers for independant variables 'L' & 'N'
Q <- quantile(data1$L, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data1$L)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
data2<- subset(data1, data1$L > (Q[1] - 1.5*iqr) & data1$L < (Q[2]+1.5*iqr))
boxplot(data2)
View(data2)

Q <- quantile(data2$H, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data2$H)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
data2<- subset(data2, data2$H > (Q[1] - 1.5*iqr) & data2$H < (Q[2]+1.5*iqr))
boxplot(data2)
View(data2)


#fit the regression model
model <- lm(P ~ L + H  + I, data = data)

#view the output of the regression model
summary(model)


mean(model$residuals)
#define the variables we want to include in the correlation matrix
data3 <- data[ , c( "L", "H", "I","P")]
boxplot(data3)
#create correlation matrix
cor(data3)


#test for multicollinearity

library(car)
vif(model)
vif_values <- vif(model)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")

#Homoscedasticity test

plot(model)

install.packages("gvlma")
library(gvlma)
gvlma(model)

white_lm(model)


# autocorrelation test - DW test
dwtest(model)
# Durbin's M test(as p=1)
bgtest(P ~ H + I + L, order=1, data=data)

bgtest(P ~ H + I + L, order=35, data=data)



