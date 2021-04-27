dataPath <- "C:/Users/Anna/Desktop/ML/2"
data <- read.csv(paste(dataPath,"test_sample.csv",sep="/"),header=T)
Y <- data[,1]
X <- data[,2:491]
library(glmnet)
install.packages('glmnet')
suppressWarnings(library(glmnet))
set.seed(1)
cv.out=cv.glmnet(x=data.matrix(X),y=data.matrix(Y),alpha=1)
bestlam =cv.out$lambda.min
set.seed(1)
out = glmnet(x=data.matrix(X),y=data.matrix(Y),alpha=1,lambda=bestlam)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef@i
length(lasso.coef@i)
eliminatedByLasso = c(lasso.coef@i[2:426])

lm.fit <- lm(Y~.,data = data)
elimlm <- which(summary(lm.fit)$coefficients[,4]>.05)
elimlm = elimlm - 1
eliminatedByLm <- c(unname(elimlm))
eliminatedByLm

res = matrix(c("lasso","lm","",""),ncol=2)
colnames(res) <- c("model","removed_regressors")
res[,"removed_regressors"][1] = paste0(eliminatedByLasso,collapse = " ")
res[,"removed_regressors"][2] = paste0(eliminatedByLm,collapse = " ")
write.csv(res,"W2answer.csv",quote=FALSE,row.names = F)
