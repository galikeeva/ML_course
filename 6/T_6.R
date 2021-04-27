dataPath <- "C:/Users/Anna/Desktop/ML/6/"
data <- read.csv(paste0(dataPath,"test_sample.csv"))
library(neuralnet)
data_factor <- data
data_factor$class = as.factor(data_factor$class)
set.seed(1)
neural_model <- neuralnet(class~., data[1:999,], hidden = 3, linear.output = FALSE, 
                          err.fct = "ce", stepmax = 1e6)
nnPred <- round(compute(neural_model, data)$net.result)
(conf <- table(pred = nnPred, true = data$class))
(accuracy = (conf[1,1]+conf[2,2])/sum(conf))
library(caret)
X = data
X$class = NULL
ctrl <- trainControl(method = "cv", number = 5)
set.seed(1)
nnFitOtto <- train(X, as.factor(data$class), method = "nnet", trControl = ctrl,
                   tuneGrid = expand.grid( .size=3:8, .decay = 0.3), trace = FALSE)
print(nnFitOtto)
set.seed(1)
nnFitOtto <- train(data[,1:2], data$class, method = "nnet", trControl = ctrl,
                   tuneGrid = expand.grid( .size=3:8, .decay = 0.3), trace = FALSE)
print(nnFitOtto)

nnCTrain <- train(formula(class~x+y), data, method = "nnet", trControl = ctrl,
                  tuneGrid = expand.grid( .size=3:8, .decay = 0.3), trace = FALSE)
print(nnCTrain)

ctrl <- trainControl(method = "cv", number = 5)
set.seed(1)
nnCTrain <- train(data[,1:2], data$class, method = "nnet", trControl = ctrl,
                  tuneGrid = expand.grid( .size=3:8, .decay = 0.3), trace = FALSE)
print(nnCTrain)
