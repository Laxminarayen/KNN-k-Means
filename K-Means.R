data("iris")
set.seed(06)
library(caret)
library(magrittr)
setwd("C:\\Users\\AADHI\\Documents\\Laxmi")
library(stats)
A <- iris[,-5]
B <- scale(A)
model <- kmeans(B,3)
df <- model$cluster
table(df)
plot(model$cluster,iris$Species)
levels(iris$Species)
df <- ifelse(df == 3,"setosa",ifelse(df == 1,"versicolor","virginica"))
confusionMatrix(factor(iris$Species),factor(df))
#rfm- recency frequency and monitory analysis customer segmentation