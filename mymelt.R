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
