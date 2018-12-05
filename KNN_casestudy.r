setwd("C:\\Users\\AADHI\\Documents\\Laxmi")
A <- read.csv("E:\\R_Module_Day_10.2_Credit_Risk_Train_data (1).csv")
library(caret)
library(magrittr)
library(stats)
library(e1071)
#install.packages("DMwR")
library(DMwR)
str(A)
A <- A[,-c(1,2,3,5,6)]
colSums(is.na(A))
A <- A[!is.na(A$LoanAmount),]
d <- A[is.na(A$Credit_History),]
e <- A[!is.na(A$Credit_History),]
colSums(is.na(e))
model <- glm(Credit_History~.,e,family = 'binomial')
summary(model)
c <- predict(model,d)
Credit_History <- ifelse(c<0.5,0,1)
d <- d[,-c(6)]
d <- cbind(d,Credit_History)
L <- rbind(d,e)
colSums(is.na(L))
L$Dependents <- as.integer(L$Dependents)
per_person_income = (L$ApplicantIncome/L$Dependents)
L <- cbind(L,per_person_income)
Loan_amt_app_income = (L$ApplicantIncome/L$LoanAmount)
L <- cbind(L,Loan_amt_app_income)
L$Property_Area <- ifelse(L$Property_Area == "Rural",1,ifelse(L$Property_Area == "Semiurban",2,3))
security_value <- (L$LoanAmount/L$Property_Area)
L <- cbind(L,security_value)
L <- L[!is.na(L$Loan_Amount_Term),]

O <- L[,-c(7)]
O <- scale(O)
O <- data.frame(O)

j <- createDataPartition(L$Loan_Status,p = 0.8,list =FALSE) %>% c()
train <- O[j,]
train <- data.frame(train)
test <- O[-j,]

train$Loan_Status <- L$Loan_Status[j]
colSums(is.na(O))
library(class)
#train <- train[!is.na(train$Loan_Amount_Term),]
model <- knn(train[,-11],test,train$Loan_Status, k =15)#just a list of the prediction
summary(model)
confusionMatrix(L$Loan_Status[-j],model)

