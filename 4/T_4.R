dataPath <- "C:/Users/Anna/Desktop/ML/4/"
train <- read.csv(paste0(dataPath,"train_sample.csv"))
test <- read.csv(paste0(dataPath,"test_sample.csv"))
m = 8 # = ncol(train)
predictors = paste0("X",1:(m-1))
library(xgboost)
params <- list("objective" = "binary:logistic")
tm <- train
tm$class = NULL
set.seed(1)
cv_tr = xgb.cv(params=params, data = data.matrix(tm), label = train$class,
                  nfold = 5, nrounds = 50,prediction=T,verbose=F)
(bestNR = which.min(cv_tr$evaluation_log$test_error_mean))
set.seed(1)
model_tr<-xgboost(data=data.matrix(tm), label = train$class,params=params,
                     nrounds=bestNR,verbose=F,save_period=NULL)
tm1 <- test
tm1$id = NULL
testXGBOOST <- predict(model_tr, data.matrix(tm1))
library(randomForest)
tr_f <- train
tr_f$class = as.factor(tr_f$class)
set.seed(1)
randFor <- randomForest(tr_f$class~., data = tr_f, importance = TRUE)
randFor$importance
randFor$importance[,3]
print(randFor)
importance(randFor)
varImpPlot(randFor, main="Variable Importance")
plot(randFor)
most <- as.integer(which.max(importance(randFor)[,1]))
testXGBOOSTfinal <- round(testXGBOOST)
pred <- data.frame("item id" = test$id, "class prediction" = testXGBOOSTfinal)
saveRDS(list(RFMostImportant = most, Forecast = pred), "W4answer.rds")

