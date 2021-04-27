dataPath <- "C:/Users/Anna/Desktop/ML/5/"
data <- read.csv(paste0(dataPath,"test_sample.csv"))
library(e1071)
data$class = as.factor(data$class)
set.seed(1)
SVMClf <- svm(class~., data)
pred <- predict(SVMClf, data)
conf <- table(pred = pred, true = data$class)
classAgreement(conf)$diag
length(which(data$class == pred))/length(data$class)
set.seed(1)
SVMClf1 <- svm(class~., data, probability = TRUE)
pred1 <- predict(SVMClf1, data)
length(which(data$class == pred1))/length(data$class)
gamma = 10^(-3:-1)
cost = 5*(2:5)
acc = 0
for (g in gamma){
  for (c in cost){
    set.seed(1)
    tmpSVM <- svm(class~., data, gamma = g, cost = c)
    tmpPred <- predict(tmpSVM, data)
    conf1 <- table(pred = tmpPred, true = data$class)
    tmpacc<-classAgreement(conf1)$diag
    if (tmpacc > acc){
      acc = tmpacc
    }
  }
}
print(acc)
