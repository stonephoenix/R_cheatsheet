## parallel: for column aggregation, dplyr works well
## doParallel, foreach, need time to copy environment to each worker
x<- iris[which(iris[,5]!="setosa"),c(1,5)]
trials<-1000
library(doParallel)
registerDoParallel(cores=8)
# result is combined as columns
system.time(
r <- foreach(icount(trials), .combine=cbind) %do% {
  ind <- sample(100, 100, replace=TRUE)
  result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
  coefficients(result1)})
# value changed in place, last data returned to r
temp <- replicate(10000,rnorm(10000))
temp <- temp*4
View(temp)
system.time(
r <- foreach(i=1:10000) %do%{
  temp[,i]=(temp[,i]-mean(temp[,i]))/var(temp[,i])
  if (i<5 ){
    temp[,i]=sqrt(abs(temp[,i]))}
  else{
      temp[,i]=temp[,i]^3}
  i
})
## doSNOW, also need to copy environment
detach("package:doParallel", unload=TRUE)
library(foreach)
library(doSNOW)
cl<-makeCluster(8)
registerDoSNOW(cl)
system.time(
  r <- foreach(i=1:10000) %do%{
    temp[,i]=(temp[,i]-mean(temp[,i]))/var(temp[,i])
    if (i<5 ){
      temp[,i]=sqrt(abs(temp[,i]))}
    else{
      temp[,i]=temp[,i]^3}
    i
  })
stopCluster(cl)
detach("package:doSNOW",unload=FALSE)
