# M is matrix, colname is a list of colnames
mymelt<-function (M,colname){
  allcol <- colnames(M)
  rest <- setdiff(allcol,colname)
  re <- NaN
  for (i in rest){
    part1 <- M[,colname]
    part1<-cbind(part1,rep(i,dim(M)[1]))
    part1<-cbind(part1,M[,i])
    re <- rbind(re,part1)
  }
  colnames(re) <- c(colname,"var","value")
  re <- re[-1,]
  return (re)
}
kkk<-mymelt(beta.df,c("lambda"))
