# see http://www.statmethods.net/stats/rdiagnostics.html
library(car)
library(ggplot2)

## get x=nXp, y=nX1, contents omitted
library(caret)
# preTreat <- preProcess(x,method=c("center","scale"))
preTreat <- preProcess(x,method=c("range"))
x<- predict(preTreat,x)

#####
## Linear Regression
xy <- data.frame(cbind(x,y))
fit <- lm(y~x)
summary(fit)
# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

## diagnostic plots 
library(car)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
## Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots
## Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

## Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

## homoscedasticity: Non-constant Error Variance
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

## VIF: Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

## Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

## Test for Autocorrelated Errors
durbinWatsonTest(fit)

## Global test of model assumptions
# skewness, kurtosis, and heteroscedasticity.
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)

##### 
## Elastic net
# install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)
fit <- glmnet(x,y,alpha=0)                  # alpha = 0, Ridge
plot(fit,log="x",ylim=c(-10,20))

## Plot coefs vs lambda
# fit$beta                                  # sparse matrix
# beta.df <- summary(fit$beta[,])
beta.df <- t(as(fit$beta,'matrix'))
beta.df <- data.frame(cbind(beta.df,fit$lambda))
colnames(beta.df)[length(colnames(beta.df))] <- "lambda"
# no need
# beta.df <- cbind(beta.df, kkk<-seq_along(beta.df[,1]))
# library(reshape2) # works only on int
# beta.melt <- melt(beta.df,id.vars=c("max_fuel_temp"))
# M is matrix, colname is a list of colnames
mymelt<-function (M,colname){
  allcol <- colnames(M)
  rest <- setdiff(allcol,colname)
  re <- data.frame(M[1,colname])
  colnames(re) <- colname
  re$var <- 0
  re$value <- 0
  for (i in rest){
    part1 <- data.frame(M[,colname])
    colnames(part1) <- colname
    part1<-cbind(part1,var=rep(i,dim(M)[1]))
    part1$var <- as.character(part1$var)
    part1<-cbind(part1,value=M[,i])
    re <- rbind(re,part1)
  }
  colnames(re) <- c(colname,"var","value")
  rownames(re) <- 1:nrow(re)
  re <- re[-1,]
  return (re)
}
kkk<-mymelt(beta.df,c("lambda"))
# kkk$value <- sign(kkk$value)*log10(abs(kkk$value)+1)
plt<-ggplot(data.frame(kkk),aes(lambda,value,col=var)) +
  geom_line() +
  scale_x_reverse() +
  scale_x_log10()
  # scale_y_continuous(limits = c(-20, 20)) +
plt
# find coefs at s(lambda)=0.1
coef(fit,s=0.1)

### cross validation
cvfit = cv.glmnet(x, y,nfolds=3,type.measure="mse")
plot(cvfit)
# lambda gives minimum mean cross-validated error
# gives the most regularized model such that 
# error is within one standard error of the minimum
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
predict(cvfit, newx = x[1:5,], s = "lambda.min")
