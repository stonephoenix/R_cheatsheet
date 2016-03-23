# Preprocess Data
# Working on SampleData
SampleData <- AllData[AllData$trte==1,]
SampleData <- subset(SampleData, select=-(trte))
sapply(SampleData,class)
library(car)
pairs(SampleData)
pairsVIM(SampleData)

# ScatterplotMatrix from Car, histogram in diagonal
corplot <- function(SampleData){
  scatterplotMatrix(SampleData, diagonal="histogram", spread = TRUE,
                  legend.plot = TRUE, legend.pos = "top")
}

SampleData0 <- SampleData[SampleData$downloads==0,]
SampleData0 <- subset(SampleData0,select=-c(downloads))
scatterplotMatrix(SampleData0, diagonal="histogram", spread = TRUE,
                  legend.plot = TRUE, legend.pos = "top")
SampleData1 <- SampleData[SampleData$downloads!=0,]
scatterplotMatrix(SampleData1, diagonal="histogram", spread = TRUE,
                  legend.plot = TRUE, legend.pos = "top")

scatterplotMatrix(~app_subcategory+rank_category,data=SampleData,
                                     diagonal="histogram")
X11()
scatterplotMatrix(~rank_kind+log10(total_estimated_installs),data=SampleData,
                  diagonal="histogram")
scatterplotMatrix(~us_price+log10(downloads+1),data=SampleData,
                  diagonal="histogram")
scatterplotMatrix(~us_price+log10(downloads+1)+total_estimated_installs+operating_system,data=SampleData,
                  diagonal="histogram")

scatterplotMatrix(SampleData,
                  diagonal="histogram")

# histrogram plot
hist(log(SampleData$total_estimated_installs))
sum(SampleData$total_estimated_installs>=1e6)
sum(SampleData$total_estimated_installs<1e6)
dim(SampleData)[1]
kkk<- SampleData$total_estimated_installs[SampleData$total_estimated_installs<1e6]
max(kkk)

# regression plot
x11(width=1000,height=1000)
plot(rank_category~app_subcategory, data=SampleData,type="p")
two <- SampleData[,c("rank_category","app_subcategory")]
two$value <- 1
library(reshape2)

# in pivot, col is rank_category, row is app_subcategory
pivot <- dcast(two, rank_category~app_subcategory, value.var="value")
pivot <- subset(pivot, select = -(rank_category))
chi1 <- chisq.test(matrix(unlist(pivot,38)),correct=F)
chi1
apply(pivot, 1,function(x) sum(x!=0))
# delete rank_category
SampleData <- subset(SampleData,select=-(rank_category))

XDummy <- model.matrix(downloads~., SampleData)
XDummy <- data.frame(XDummy[,-1]) # the first one is intercept

## one way ANOVA
boxplot(log2(downloads+1)~rank_kind,data=SampleData,main="ANOVA",
        xlab="rank_kind",ylab="log2(downloads+1)")
boxplot(log10(total_estimated_installs+1)~rank_kind,data=SampleData,main="ANOVA",
        xlab="rank_kind",ylab="log10(total_estimated_installs+1)")
boxplot(log10(total_estimated_installs+1)~operating_system,data=SampleData,main="ANOVA",
        xlab="rank_kind",ylab="downloads")
results <- aov(downloads~rank_kind,data=SampleData)
TukeyHSD(results)

## PCA
library(FactoMineR)
res <- PCA(SampleData,quanti.sup = c(9,12,14),
           quali.sup=c(1:8,10,11,13))
