data <- read.table("test_sample.csv",header=T)
View(data)
Y <- data[,1]
data$Y = NULL
View(data[,1:2])
X <- data
rm(data)
rSquared<-sapply(1:491,function(z) summary(lm(Y~.,data=data.frame(Y,X[,1:z])))$r.squared)
matplot(1:491,rSquared.10,type="l",lty=1,lwd=2,col = "black",
        main="OriginalR2",
        xlab="Number of Predictors",ylab="Determination Coefficient")
legend("bottomright",legend="Original",lty=1,lwd=2,col="black")
matplot(1:491,rSquared,type="l",lty=1,lwd=2,col = "black",
        main="OriginalR2",
        xlab="Number of Predictors",ylab="Determination Coefficient")
legend("bottomright",legend="Original",lty=1,lwd=2,col="black")
View(rSquared)
indices = which(rSquared > 0.9)
View(rSquared[311:312])
N.orig <- 312
pca.x <- princomp(X)
plot(pca.x)
suppressWarnings(library(relaimpo))
factorLoading = pca.x$loadings
factorScores = pca.x$scores
factors10Data<-data.frame(Y,factorScores)
m_all.PCA<-lm(Y~.,data=factors10Data)
metrics.PCA <- calc.relimp(m_all.PCA, type = c("lmg", "first", "last","betasq", "pratt"))
metrics.PCA <- calc.relimp(m_all.PCA, type = "lmg")
metrics.PCA <- calc.relimp(m_all.PCA, type = "first")
(first.PCA.rank<-metrics.PCA@first.rank)
orderedFactors<-factorScores[,order(first.PCA.rank)]
orderedPCA.R2<-sapply(1:491,function(z) summary(lm(Y~.,data=data.frame(Y,orderedFactors[,1:z])))$r.squared)
matplot(1:491,cbind(rSquared,orderedPCA.R2),type="l",lty=1,lwd=2,col=c("black","red"),
        main="Improvement of Fit with Number of Predictors",
        xlab="Number of Predictors",ylab="Determination Coefficient")
legend("bottomright",legend=c("Original","PCA"),lty=1,lwd=2,col=c("black","red"))
r2pca <- which(orederPCA.R2 > 0.9)
r2pca <- which(orederedPCA.R2 > 0.9)
r2pca <- which(orderedPCA.R2 > 0.9)
orderedPCA.R2[143:144]
N.Pca <- 144
(res <- N.orig - N.Pca)
(det_coef <- orderedPCA.R2[144])
