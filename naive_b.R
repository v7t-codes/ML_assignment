library(MASS)
library(klaR)
library("e1071")
library("caret")

train_<- iris[-c(41:50,91:100, 141:150),] ## splitting the data set by removing the test data
test<- iris[-c(1:40, 50:90, 100:140),] ## same as before but by removing the training data from iris

##the above method preserves the data frame

## seperate the features of the train data and targets to train
x1 = train_[,-5]
y1 = train_$Species
model = train(x1,y1,'nb',trControl=trainControl(method='cv',number=5))
model ## the naive bayes model

x_test = test[,-5]
predict(model$finalModel,x_test) ## predicting on the test data
y_test = test$Species
table(predict(model$finalModel,x_test)$class,y_test)