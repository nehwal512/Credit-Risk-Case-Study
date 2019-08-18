library(readxl) # Library for read excel data
data3 <- read_excel("D:/projects/imarticus projects code/Case Study 3_Credit Risk Case Study/Credit.xlsx")
View(data3) # view data set
str(data3) # Structure of data
data3$ID<-NULL # id variable is not usefull for prediction so delete that column 
colSums(is.na(data3)) # finding the missing values 
table(data3$BanruptcyInd) 
data3$BanruptcyInd<- as.factor(data3$BanruptcyInd) # convert factor from numeric 
data3$TARGET=as.factor(data3$TARGET) # convert factor from numeric
str(data3)

library(VIM) # library for Visualization and Imputation of Missing Values
data3<- kNN(data3) # K nearest nabour for imputing missing value
summary(data3)
data3<-subset(data3,select = TARGET:TLOpen24Pct)
summary(data3) # summary of the data
colSums(is.na(data3))

boxplot(data3$DerogCnt) # boxplot for finding outliers
summary(data3$DerogCnt)
uf_dero <- 2 +1.5*IQR(data3$DerogCnt) # finding upper fence for removing outliers
hist(data3$DerogCnt) # histogram
data3$DerogCnt[data3$DerogCnt >uf_dero ] <-median(data3$DerogCnt) # removing outliers by median 
boxplot(data3$DerogCnt)
# same for CollectCnt variable
boxplot(data3$CollectCnt)
summary(data3$CollectCnt)
uf_colcnt <- 1 +1.5*IQR(data3$CollectCnt) 
hist(data3$CollectCnt)
data3$CollectCnt[data3$CollectCnt >uf_colcnt ] <-median(data3$CollectCnt)
boxplot(data3$CollectCnt)
# for InqCnt06 Variable 
boxplot(data3$InqCnt06)
summary(data3$InqCnt06)
uf_inqcnt06 <- 4 +1.5*IQR(data3$InqCnt06) 
hist(data3$InqCnt06)
data3$InqCnt06[data3$InqCnt06 >uf_inqcnt06 ] <-median(data3$InqCnt06)
boxplot(data3$InqCnt06)
#for InqTimeLast variable 
boxplot(data3$InqTimeLast)
summary(data3$InqTimeLast)
uf_InqTimeLast <- 3 +1.5*IQR(data3$InqTimeLast) 
hist(data3$InqTimeLast)
data3$InqTimeLast[data3$InqTimeLast >uf_inqcnt06 ] <-median(data3$InqTimeLast)
boxplot(data3$InqTimeLast)
# for InqFinanceCnt24 variable 
boxplot(data3$InqFinanceCnt24)
summary(data3$InqFinanceCnt24)
uf_InqFinanceCnt24 <- 5 +1.5*IQR(data3$InqFinanceCnt24) 
hist(data3$InqFinanceCnt24)
data3$InqFinanceCnt24[data3$InqFinanceCnt24 >uf_InqFinanceCnt24 ] <-median(data3$InqFinanceCnt24)
boxplot(data3$InqFinanceCnt24)
# for TLTimeFirst variable
boxplot(data3$TLTimeFirst)
summary(data3$TLTimeFirst)
uf_TLTimeFirst <- 227 +1.5*IQR(data3$TLTimeFirst) 
hist(data3$TLTimeFirst)
data3$TLTimeFirst[data3$TLTimeFirst >uf_TLTimeFirst ] <-mean(data3$TLTimeFirst)
boxplot(data3$TLTimeFirst)
# for TLTimeLast variable
boxplot(data3$TLTimeLast)
summary(data3$TLTimeLast)
uf_TLTimeLast <- 13 +1.5*IQR(data3$TLTimeLast) 
hist(data3$TLTimeLast)
data3$TLTimeLast[data3$TLTimeLast >uf_TLTimeLast ] <-median(data3$TLTimeLast)
boxplot(data3$TLTimeLast)
# for TLCnt03 variable
boxplot(data3$TLCnt03)  # may be we can change this into factors or NULL
summary(data3$TLCnt03)
table(data3$TLCnt03)
data3$TLCnt03<-NULL
# for TLCnt12 variable
boxplot(data3$TLCnt12)
summary(data3$TLCnt12)
table(data3$TLCnt12)
uf_TLCnt12 <- 3 +1.5*IQR(data3$TLCnt12) 
hist(data3$TLCnt12)
data3$TLCnt12[data3$TLCnt12 >uf_TLCnt12 ] <-median(data3$TLCnt24)
boxplot(data3$TLCnt12)
# for TLCnt24 variable
boxplot(data3$TLCnt24)
summary(data3$TLCnt24)
table(data3$TLCnt24)
uf_TLCnt24 <- 6 +1.5*IQR(data3$TLCnt24) 
hist(data3$TLCnt24)
data3$TLCnt24[data3$TLCnt24 >uf_TLCnt24 ] <-median(data3$TLCnt24)
boxplot(data3$TLCnt24)
# for TLCnt variable
boxplot(data3$TLCnt)
summary(data3$TLCnt)
table(data3$TLCnt)
uf_TLCnt <- 11 +1.5*IQR(data3$TLCnt) 
hist(data3$TLCnt)
data3$TLCnt[data3$TLCnt >uf_TLCnt ] <-median(data3$TLCnt)
boxplot(data3$TLCnt)
# for TLSum variable
boxplot(data3$TLSum )                 
summary(data3$TLSum ) 
uf_TLSum  <- 28111  +1.5*IQR(data3$TLSum ) 
hist(data3$TLCnt)
data3$TLSum [data3$TLSum  >uf_TLSum  ] <-median(data3$TLSum )
boxplot(data3$TLSum )          # after filling values in place of outliers there is also exixt some outliers
# for TLMaxSum variable
boxplot(data3$TLMaxSum)
summary(data3$TLMaxSum)
uf_TLMaxSum  <- 44547  +1.5*IQR(data3$TLMaxSum ) 
hist(data3$TLCnt)
data3$TLMaxSum [data3$TLMaxSum  >uf_TLMaxSum] <-median(data3$TLMaxSum )
boxplot(data3$TLMaxSum)     # after filling values in place of outliers there is also exixt some outliers
# for TLSatCnt variable
boxplot(data3$TLSatCnt )
summary(data3$TLSatCnt )
table(data3$TLSatCnt )
uf_TLSatCnt   <- 19  +1.5*IQR(data3$TLSatCnt  ) 
hist(data3$TLSatCnt )
data3$TLSatCnt  [data3$TLSatCnt   >uf_TLSatCnt ] <-median(data3$TLSatCnt  )
boxplot(data3$TLSatCnt )
# for TLDel60Cnt variable
boxplot(data3$TLDel60Cnt )
summary(data3$TLDel60Cnt )
table(data3$TLDel60Cnt )
uf_TLDel60Cnt   <- 2  +1.5*IQR(data3$TLDel60Cnt  ) 
hist(data3$TLSatCnt )
data3$TLDel60Cnt[data3$TLDel60Cnt >uf_TLDel60Cnt] <-median(data3$TLDel60Cnt  )
boxplot(data3$TLDel60Cnt )
# for TLBadCnt24 variable
boxplot(data3$TLBadCnt24 )        # may be we can change this into factors or NULL
summary(data3$TLBadCnt24 )
table(data3$TLBadCnt24 )
uf_TLBadCnt24   <- 1  +1.5*IQR(data3$TLBadCnt24  ) 
hist(data3$TLBadCnt24 )
data3$TLBadCnt24[data3$TLBadCnt24 >uf_TLBadCnt24] <-median(data3$TLBadCnt24  )
data3$TLBadCnt24<-NULL
# for TL75UtilCnt variable
boxplot(data3$TL75UtilCnt )
summary(data3$TL75UtilCnt )
table(data3$TL75UtilCnt )
uf_TL75UtilCnt   <- 4  +1.5*IQR(data3$TL75UtilCnt  ) 
hist(data3$TL75UtilCnt )
data3$TL75UtilCnt[data3$TL75UtilCnt >uf_TL75UtilCnt] <-median(data3$TL75UtilCnt  )
boxplot(data3$TL75UtilCnt )
# for TL50UtilCnt variable
boxplot(data3$TL50UtilCnt )
summary(data3$TL50UtilCnt )
table(data3$TL50UtilCnt )
uf_TL50UtilCnt   <- 5  +1.5*IQR(data3$TL50UtilCnt  ) 
hist(data3$TL50UtilCnt )
data3$TL50UtilCnt[data3$TL50UtilCnt >uf_TL50UtilCnt] <-median(data3$TL50UtilCnt  )
boxplot(data3$TL50UtilCnt )
# for TLBalHCPct variable
boxplot(data3$TLBalHCPct )
summary(data3$TLBalHCPct )
uf_TLBalHCPct   <- 0.8407  +1.5*IQR(data3$TLBalHCPct  ) 
hist(data3$TLBalHCPct )
data3$TLBalHCPct[data3$TLBalHCPct >uf_TLBalHCPct] <-mean(data3$TLBalHCPct  )
boxplot(data3$TLBalHCPct )
# for TLSatPct variable
boxplot(data3$TLSatPct  )
summary(data3$TLSatPct  )

boxplot(data3$TLDel3060Cnt24 )
summary(data3$TLDel3060Cnt24 )
# for TLDel90Cnt24 variable
boxplot(data3$TLDel90Cnt24 )  
summary(data3$TLDel90Cnt24 )
uf_TLDel90Cnt24<- 1  +1.5*IQR(data3$TLDel90Cnt24  ) 
hist(data3$TLDel90Cnt24 )
data3$TLDel90Cnt24[data3$TLDel90Cnt24 >uf_TLDel90Cnt24] <-median(data3$TLDel90Cnt24  )
boxplot(data3$TLDel90Cnt24 )
# for TLDel60CntAll variable
boxplot(data3$TLDel60CntAll )
summary(data3$TLDel60CntAll )
table(data3$TLDel60CntAll )
uf_TLDel60CntAll   <- 4  +1.5*IQR(data3$TLDel60CntAll  ) 
hist(data3$TLDel60CntAll )
data3$TLDel60CntAll[data3$TLDel60CntAll >uf_TLDel60CntAll] <-median(data3$TLDel60CntAll  )
boxplot(data3$TLDel60CntAll )

boxplot(data3$TLOpenPct )
summary(data3$TLOpenPct )
# for TLBadDerogCnt variable
boxplot(data3$TLBadDerogCnt )
summary(data3$TLBadDerogCnt )
table(data3$TLBadDerogCnt )
uf_TLBadDerogCnt   <- 2  +1.5*IQR(data3$TLBadDerogCnt  ) 
hist(data3$TLBadDerogCnt )
data3$TLBadDerogCnt[data3$TLBadDerogCnt >uf_TLBadDerogCnt] <-median(data3$TLBadDerogCnt  )
boxplot(data3$TLBadDerogCnt )
# for TLDel60Cnt24 variable
boxplot(data3$TLDel60Cnt24 )
summary(data3$TLDel60Cnt24 )
table(data3$TLDel60Cnt24 )
uf_TLDel60Cnt24   <- 1  +1.5*IQR(data3$TLDel60Cnt24  ) 
hist(data3$TLDel60Cnt24 )
data3$TLDel60Cnt24[data3$TLDel60Cnt24 >uf_TLDel60Cnt24] <-median(data3$TLDel60Cnt24  )
boxplot(data3$TLDel60Cnt24 )
# for TLOpen24Pct variable
boxplot(data3$TLOpen24Pct )
summary(data3$TLOpen24Pct )
uf_TLOpen24Pct   <- 0.8  +1.5*IQR(data3$TLOpen24Pct  ) 
hist(data3$TLDel60Cnt24 )
data3$TLOpen24Pct[data3$TLOpen24Pct >uf_TLOpen24Pct] <-median(data3$TLOpen24Pct  )
boxplot(data3$TLOpen24Pct )

boxplot(data3)
str(data3)
boxplot(data3$TLSum)
boxplot(data3$TLMaxSum )

library(caTools) # spliting
split<-sample.split(data3$TARGET,SplitRatio = 0.70)
# split
train1<-subset(data3,split == "TRUE")
test1<-subset(data3,split == "FALSE")
# logestic regression 
model1<- glm(TARGET~.,family=binomial,data=train1)
summary(model1)
library(caTools)
fitted.results1 <- predict(model1,newdata=test1[,-1],type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.5,1,0)
cf1<-table(fitted.results1 , test1[,1])

# Accuracy 
acc1<-sum(diag(cf1))/sum(cf1)
acc1
library(ROCR)
p <- predict(model1, newdata=test1[,-1], type="response")
pr <- prediction(p, test1[,1])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
plot(prf,colorize= TRUE,print.cutoffs.at=seq(0.1,by=0.1))
auc <- performance(pr, measure = "auc")
auc1 <- auc@y.values[[1]]
auc1
# VIF 
library(car)
vif(model1)  

# Second Model 
model2<- glm(TARGET~. -(TLCnt24+TLSum+TLMaxSum+TL50UtilCnt),family=binomial,data=train1)
summary(model2)
library(caTools)
fitted.results1 <- predict(model2,newdata=test1[,-1],type='response')
fitted.results1 <- ifelse(fitted.results1 > 0.5,1,0)
cf2<-table(fitted.results1 , test1[,1])
# accuracy
acc2<-sum(diag(cf2))/sum(cf2)
acc2
# Prediction and Area Under Curve
library(ROCR)
p <- predict(model2, newdata=test1[,-1], type="response")
pr <- prediction(p, test1[,1])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc2 <- auc@y.values[[1]]
auc2