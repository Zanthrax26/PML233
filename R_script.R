library(lattice)
library(ggplot2)
library(caret)
library(kernlab)
library(rattle)
library(corrplot)
set.seed(1234)

setwd("C://Vedant/MIT/7th_Semester/DSC/PML")

trainingData <- read.csv("pml-training.csv")
testingData <- read.csv("pml-testing.csv")

dim(trainingData)
str(trainingData)

dim(testingData)
str(testingData)

trainingNA <- trainingData[,colMeans(is.na(trainingData)) < .9]
dim(trainingNA)
dim(trainingData)

trainingMeta <- trainingNA[,-c(1:7)]
dim(trainingMeta)

nvz <- nearZeroVar(trainingMeta)
trainingZV <- trainingMeta[,-nvz]
dim(trainingZV)

inTrain <- createDataPartition(y=trainingZV$classe, p=0.7, list=F)
training <- trainingZV[inTrain,]
validation <- trainingZV[-inTrain,]

dim(training)
dim(validation)

control <- trainControl(method="cv", number=3, verboseIter=F)

library(caret)
corrPlot <- cor(training[, -length(names(training))])
corrplot(corrPlot, method="square", type="upper", tl.col="Black", 
         col = colorRampPalette(c("purple", "white", "green"))(200), shade.col="blue")



dec_trees <- train(classe~., data=training, method="rpart", trControl = control, tuneLength = 5)
dec_trees_pred <- predict(dec_trees, validation)
trees_pred <- confusionMatrix(dec_trees_pred, factor(validation$classe))
trees_pred


library(caret)
random_forest <- train(classe~., data=training, method="rf", trControl = control, tuneLength = 5)
random_forest_pred <- predict(random_forest, validation)
random_pred <- confusionMatrix(random_forest_pred , factor(validation$classe))
random_pred


SVM <- train(classe~., data=training, method="svmLinear", trControl = control, tuneLength = 5, verbose = F)
SVM_Predictions <- predict(SVM, validation)
Pred_SVN <- confusionMatrix(SVM_Predictions, factor(validation$classe))
Pred_SVN

Test_Predictions <- predict(SVM, testingData)
print(Test_Predictions)
