install.packages("rpivotTable")
install.packages("corrplot")
install.packages("caret")
install.packages("DMwR")
install.packages("readxl")
install.packages("tidyverse")
install.packages("ROCR")
install.packages("ModelMetrics")
install.packages("ineq")
install.packages("e1071")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("grobs")
install.packages("car")
install.packages("VIF")
install.packages("lmtest")
install.packages("pROC")
install.packages("nFactors")
install.packages("VIM")
install.packages("factoextra")
install.packages("randomForest")
install.packages("InformationValue")

library(InformationValue)
library(rpivotTable)
library(tidyverse)
library("VIM")
library(readr)
library(corrplot)
library(pROC)
library(nFactors)
library(factoextra)
library(caret)
library(MASS)
library(psych)
library(grid)
library(gridExtra)
library(lattice)
library(ModelMetrics)
library(ineq)
library(ROCR)
library(readxl)
library(dplyr)
library(rpart)
library(ggplot2)
library(rpart.plot)
library(dplyr)
library(VIF)
library(lmtest)
library(car)
library(e1071)
library(class)
library(car)
library(randomForest)

#Set working directory
setwd("D:/GreatLakes/Capstone")
getwd()

#Read the dataset

mydata= read.csv("Telecom_Sampled.csv")
attach(mydata)
View(mydata)

#Exploratory Data Analysis
dim(mydata)
nrow(mydata)
ncol(mydata)
names(mydata)
head(mydata,10)
tail(mydata,15)
class(mydata)
summary(mydata)
str(mydata)

#convert forgntv1, mtrcycle, truck, churn to factors
col=c('forgntvl','mtrcycle','truck','churn')
mydata[col]=lapply(mydata[col],factor)

## Converting Income into ordered factors
mydata$income =factor(mydata$income,levels = c("1","2","3","4","5","6","7","8","9"),order = TRUE)

str(mydata)

anyNA(mydata)
sapply(mydata, function(x)sum(is.na(x)))
sapply(mydata, function(x)(sum(is.na(x))/26518)*100)
#As missing values are more than 40% of the data, would drop mailordr,occu1,
#numbcars,retdays,wrkwoman,solflag,proptype,mailresp,cartype,children and div_type              

mydata1=mydata[-c(48,49,52,53,55,61,62,63,64,66,72)]
names(mydata1)

#Missing value treatment

mydata1$`avg6mou`[is.na(mydata1$`avg6mou`)]=median(mydata1$`avg6mou`,na.rm=T)
mydata1$`avg6qty`[is.na(mydata1$`avg6qty`)]=median(mydata1$`avg6qty`,na.rm=T)
mydata1$`mou_Mean`[is.na(mydata1$`mou_Mean`)]=median(mydata1$`mou_Mean`,na.rm=T)
mydata1$`totmrc_Mean`[is.na(mydata1$`totmrc_Mean`)]=median(mydata1$`totmrc_Mean`,na.rm=T)
mydata1$`rev_Range`[is.na(mydata1$`rev_Range`)]=median(mydata1$`rev_Range`,na.rm=T)
mydata1$`mou_Range`[is.na(mydata1$`mou_Range`)]=median(mydata1$`mou_Range`,na.rm=T)
mydata1$`change_mou`[is.na(mydata1$`change_mou`)]=median(mydata1$`change_mou`,na.rm=T)
mydata1$`ovrrev_Mean`[is.na(mydata1$`ovrrev_Mean`)]=median(mydata1$`ovrrev_Mean`,na.rm=T)
mydata1$`rev_Mean`[is.na(mydata1$`rev_Mean`)]=median(mydata1$`rev_Mean`,na.rm=T)
mydata1$`ovrmou_Mean`[is.na(mydata1$`ovrmou_Mean`)]=median(mydata1$`ovrmou_Mean`,na.rm=T)


mydata1=kNN(mydata1,variable =c("prizm_social_one","income","dwlltype","dwllsize",
                                "hnd_webcap","marital","ethnic","age1","age2",
                                "hnd_price","forgntvl","mtrcycle","truck",
                                "roam_Mean","car_buy","csa","da_Mean","da_Range",
                                "datovr_Mean","datovr_Range","area"))


mydata2=mydata1[-c(71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91)]
sapply(mydata2, function(x)sum(is.na(x)))



mydata2[mydata2$'eqpdays'<0,]  ## checking for rows having negative values in Age of current equipment
mydata2$'eqpdays' = abs(mydata2$'eqpdays')# fixing negative values 
dim(mydata2)
str(mydata2)
summary(mydata2)

prop.table(table(mydata2$churn))
#76% of customers did not churn and 24% has churned

#Exploratory Data Analysis and Outlier Treatment


boxplot(mydata2$mou_Mean,main ="Boxplot of Mean number of monthly minutes of use") 
#outliers exist in the upper end
quantile(mydata2$mou_Mean,probs = seq(0,1,0.05))
subset1=mydata2[which(mydata2$mou_Mean>1589.7),]  # Capping in the upper value
subset1$mou_Mean=1589.7
mydata2[which(mydata2$mou_Mean>1589.7),]=subset1
boxplot(mydata2$mou_Mean,main ="Boxplot of Mean number of monthly minutes of use") 
hist(mydata2$mou_Mean,main ="Histogram of Mean number of monthly minutes of use")
ggplot(mydata2,aes(mou_Mean,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(mou_Mean,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$mou_Mean,data=mydata2,fill=churn)


boxplot(mydata2$totmrc_Mean,main ="Boxplot of Mean total monthly recurring charge") 
#outliers exist in the upper end
quantile(mydata2$totmrc_Mean,probs = seq(0,1,0.05))
subset2=mydata2[which(mydata2$totmrc_Mean>104.95),]  # Capping in the upper value
subset2$totmrc_Mean=104.95
mydata2[which(mydata2$totmrc_Mean>104.95),]=subset2
boxplot(mydata2$totmrc_Mean,main ="Boxplot of Mean total monthly recurring charge") 
hist(mydata2$totmrc_Mean,main ="Histogram of Mean total monthly recurring charge")
ggplot(mydata2,aes(totmrc_Mean,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(totmrc_Mean,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$totmrc_Mean,data=mydata2,fill=churn)

boxplot(mydata2$rev_Range,main ="Boxplot of Range of revenue (charge amount)") 
#outliers exist in the upper end
quantile(mydata2$rev_Range,probs = seq(0,1,0.05))
subset4=mydata2[which(mydata2$rev_Range>140.2),]  # Capping in the upper value
subset4$rev_Range=140.2
mydata2[which(mydata2$rev_Range>140.2),]=subset4
boxplot(mydata2$rev_Range,main ="Boxplot of Range of revenue (charge amount)") 
hist(mydata2$rev_Range,main ="Histogram of Range of revenue (charge amount)")
ggplot(mydata2,aes(rev_Range,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(rev_Range,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$rev_Range,data=mydata2,fill=churn)

boxplot(mydata2$mou_Range,main ="Boxplot of Range of number of minutes of use") 
#outliers exist in the upper end
quantile(mydata2$mou_Range,probs = seq(0,1,0.05))
subset5=mydata2[which(mydata2$mou_Range>1037),]  # Capping in the upper value
subset5$mou_Range=1037
mydata2[which(mydata2$mou_Range>1037),]=subset5
boxplot(mydata2$mou_Range,main ="Boxplot of Range of number of minutes of use") 
hist(mydata2$mou_Range,main ="Histogram of Range of number of minutes of use")
ggplot(mydata2,aes(mou_Range,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(mou_Range,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$mou_Range,data=mydata2,fill=churn)


boxplot(mydata2$change_mou,main ="Boxplot of Percentage change in monthly minutes of use vs previous three month average") 
#outliers exist in the upper end
quantile(mydata2$change_mou,probs = seq(0,1,0.05))
subset6=mydata2[which(mydata2$change_mou>283),]  # Capping in the upper value
subset6$change_mou=283
mydata2[which(mydata2$change_mou>283),]=subset6
boxplot(mydata2$change_mou,main ="Boxplot of Percentage change in monthly minutes of use vs previous three month average") 
hist(mydata2$change_mou,main ="Histogram of Percentage change in monthly minutes of use vs previous three month average")
ggplot(mydata2,aes(change_mou,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(change_mou,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$change_mou,data=mydata2,fill=churn)
ggplot(mydata2,aes(change_mou,fill=churn,color=churn))+geom_dotplot()


boxplot(mydata2$drop_blk_Mean,main ="Boxplot of Mean number of dropped or blocked calls") 
#outliers exist in the upper end
quantile(mydata2$drop_blk_Mean,probs = seq(0,1,0.05))
subset8=mydata2[which(mydata2$drop_blk_Mean>28),]  # Capping in the upper value
subset8$drop_blk_Mean=28
mydata2[which(mydata2$drop_blk_Mean>28),]=subset8
boxplot(mydata2$drop_blk_Mean,main ="Boxplot of Mean number of dropped or blocked calls") 
hist(mydata2$drop_blk_Mean,main ="Histogram of Mean number of dropped or blocked calls")
ggplot(mydata2,aes(drop_blk_Mean,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(drop_blk_Mean,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$drop_blk_Mean,data=mydata2,fill=churn)

boxplot(mydata2$drop_vce_Range,main ="Boxplot of Range of number of dropped (failed) voice calls") 
#outliers exist in the upper end
quantile(mydata2$drop_vce_Range,probs = seq(0,1,0.05))
subset9=mydata2[which(mydata2$drop_vce_Range>16),]  # Capping in the upper value
subset9$drop_vce_Range=16
mydata2[which(mydata2$drop_vce_Range>16),]=subset9
boxplot(mydata2$drop_vce_Range,main ="Boxplot of Range of number of dropped (failed) voice calls") 
hist(mydata2$drop_vce_Range,main ="Histogram of Range of number of dropped (failed) voice calls")
ggplot(mydata2,aes(drop_vce_Range,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(drop_vce_Range,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$drop_vce_Range,data=mydata2,fill=churn)

boxplot(mydata2$owylis_vce_Range,main ="Boxplot of Range of number of outbound wireless to wireless voice calls") 
#outliers exist in the upper end
quantile(mydata2$owylis_vce_Range,probs = seq(0,1,0.05))
subset10=mydata2[which(mydata2$owylis_vce_Range>45),]  # Capping in the upper value
subset10$owylis_vce_Range=45
mydata2[which(mydata2$owylis_vce_Range>45),]=subset10
boxplot(mydata2$owylis_vce_Range,main ="Boxplot of Range of number of outbound wireless to wireless voice calls") 
hist(mydata2$owylis_vce_Range,main ="Histogram of Range of number of outbound wireless to wireless voice calls")
ggplot(mydata2,aes(owylis_vce_Range,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(owylis_vce_Range,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$owylis_vce_Range,data=mydata2,fill=churn)

boxplot(mydata2$mou_opkv_Range,main ="Boxplot of Range of unrounded minutes of use of off-peak voice calls") 
#outliers exist in the upper end
quantile(mydata2$mou_opkv_Range,probs = seq(0,1,0.05))
subset11=mydata2[which(mydata2$mou_opkv_Range>330),]  # Capping in the upper value
subset11$mou_opkv_Range=330
mydata2[which(mydata2$mou_opkv_Range>330),]=subset11
boxplot(mydata2$mou_opkv_Range,main ="Boxplot of Range of unrounded minutes of use of off-peak voice calls") 
hist(mydata2$mou_opkv_Range,main ="Histogram of Range of unrounded minutes of use of off-peak voice calls")
ggplot(mydata2,aes(mou_opkv_Range,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(mou_opkv_Range,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$mou_opkv_Range,data=mydata2,fill=churn)

boxplot(mydata2$months,main ="Boxplot of Total number of months in service") 
#outliers exist in the upper end
quantile(mydata2$months,probs = seq(0,1,0.05))
subset12=mydata2[which(mydata2$months>43),]  # Capping in the upper value
subset12$months=43
mydata2[which(mydata2$months>43),]=subset12
boxplot(mydata2$months,main ="Boxplot of Total number of months in service") 
hist(mydata2$months,main ="Histogram of Total number of months in service")
ggplot(mydata2,aes(months,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(months,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$months,data=mydata2,fill=churn)


boxplot(mydata2$totcalls,main ="Boxplot of Total number of calls over the life of the customer") 
#outliers exist in the upper end
quantile(mydata2$totcalls,probs = seq(0,1,0.05))
subset13=mydata2[which(mydata2$totcalls>7530),]  # Capping in the upper value
subset13$totcalls=7530
mydata2[which(mydata2$totcalls>7530),]=subset13
boxplot(mydata2$totcalls,main ="Boxplot of Total number of calls over the life of the customer") 
hist(mydata2$totcalls,main ="Histogram of Total number of calls over the life of the customer")
ggplot(mydata2,aes(totcalls,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(totcalls,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$totcalls,data=mydata2,fill=churn)

boxplot(mydata2$eqpdays,main ="Boxplot of Number of days (age) of current equipment") 
#outliers exist in the upper end
quantile(mydata2$eqpdays,probs = seq(0,1,0.05))
subset14=mydata2[which(mydata2$eqpdays>970),]  # Capping in the upper value
subset14$eqpdays=970
mydata2[which(mydata2$eqpdays>970),]=subset14
boxplot(mydata2$eqpdays,main ="Boxplot of Number of days (age) of current equipment") 
hist(mydata2$eqpdays,main ="Histogram of Number of days (age) of current equipment")
ggplot(mydata2,aes(eqpdays,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(eqpdays,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$eqpdays,data=mydata2,fill=churn)

boxplot(mydata2$custcare_Mean,main ="Boxplot of Mean number of customer care calls") 
#outliers exist in the upper end
quantile(mydata2$custcare_Mean,probs = seq(0,1,0.05))
subset15=mydata2[which(mydata2$custcare_Mean>4),]  # Capping in the upper value
subset15$custcare_Mean=4
mydata2[which(mydata2$custcare_Mean>4),]=subset15
boxplot(mydata2$custcare_Mean,main ="Boxplot of Mean number of customer care calls") 
hist(mydata2$custcare_Mean,main ="Histogram of Mean number of customer care calls")
ggplot(mydata2,aes(custcare_Mean,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(custcare_Mean,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$custcare_Mean,data=mydata2,fill=churn)


boxplot(mydata2$callwait_Mean,main ="Boxplot of Mean number of call waiting calls") 
#outliers exist in the upper end
quantile(mydata2$callwait_Mean,probs = seq(0,1,0.05))
subset16=mydata2[which(mydata2$callwait_Mean>4),]  # Capping in the upper value
subset16$callwait_Mean=4
mydata2[which(mydata2$callwait_Mean>4),]=subset16
boxplot(mydata2$callwait_Mean,main ="Boxplot of Mean number of call waiting calls") 
hist(mydata2$callwait_Mean,main ="Histogram of Mean number of call waiting calls")
ggplot(mydata2,aes(callwait_Mean,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(callwait_Mean,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$callwait_Mean,data=mydata2,fill=churn)  
 
boxplot(mydata2$iwylis_vce_Mean ,main ="Boxplot of Mean number of inbound wireless to wireless voice calls") 
#outliers exist in the upper end
quantile(mydata2$iwylis_vce_Mean ,probs = seq(0,1,0.05))
subset17=mydata2[which(mydata2$iwylis_vce_Mean >23),]  # Capping in the upper value
subset17$iwylis_vce_Mean =23
mydata2[which(mydata2$iwylis_vce_Mean >23),]=subset17
boxplot(mydata2$iwylis_vce_Mean ,main ="Boxplot of Mean number of inbound wireless to wireless voice calls") 
hist(mydata2$iwylis_vce_Mean ,main ="Histogram of Mean number of inbound wireless to wireless voice calls")
ggplot(mydata2,aes(iwylis_vce_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(iwylis_vce_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$iwylis_vce_Mean ,data=mydata2,fill=churn)  

boxplot(mydata2$callwait_Range ,main ="Boxplot of Range of number of call waiting calls") 
#outliers exist in the upper end
quantile(mydata2$callwait_Range ,probs = seq(0,1,0.05))
subset18=mydata2[which(mydata2$callwait_Range >5),]  # Capping in the upper value
subset18$callwait_Range =5
mydata2[which(mydata2$callwait_Range >5),]=subset18
boxplot(mydata2$callwait_Range ,main ="Boxplot of Range of number of call waiting calls") 
hist(mydata2$callwait_Range ,main ="Histogram of Range of number of call waiting calls")
ggplot(mydata2,aes(callwait_Range ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(callwait_Range ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$callwait_Range ,data=mydata2,fill=churn)  

boxplot(mydata2$ccrndmou_Range ,main ="Boxplot of Range of rounded minutes of use of customer care calls") 
#outliers exist in the upper end
quantile(mydata2$ccrndmou_Range ,probs = seq(0,1,0.05))
subset19=mydata2[which(mydata2$ccrndmou_Range >17),]  # Capping in the upper value
subset19$ccrndmou_Range =17
mydata2[which(mydata2$ccrndmou_Range >17),]=subset19
boxplot(mydata2$ccrndmou_Range ,main ="Boxplot of Range of rounded minutes of use of customer care calls") 
hist(mydata2$ccrndmou_Range ,main ="Histogram of Range of rounded minutes of use of customer care calls")
ggplot(mydata2,aes(ccrndmou_Range ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(ccrndmou_Range ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$ccrndmou_Range ,data=mydata2,fill=churn)


boxplot(mydata2$adjqty,main ="Boxplot of Billing adjusted total number of calls over the life of the customer") 
#outliers exist in the upper end
quantile(mydata2$adjqty,probs = seq(0,1,0.05))
subset20=mydata2[which(mydata2$adjqty>7440),]  # Capping in the upper value
subset20$adjqty=7440
mydata2[which(mydata2$adjqty>7440),]=subset20
boxplot(mydata2$adjqty,main ="Boxplot of Billing adjusted total number of calls over the life of the customer") 
hist(mydata2$adjqty,main ="Histogram of Billing adjusted total number of calls over the life of the customer")
ggplot(mydata2,aes(adjqty,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(adjqty ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$adjqty,data=mydata2,fill=churn)

boxplot(mydata2$ovrrev_Mean ,main ="Boxplot of Mean overage revenue") 
#outliers exist in the upper end
quantile(mydata2$ovrrev_Mean ,probs = seq(0,1,0.05))
subset21=mydata2[which(mydata2$ovrrev_Mean >35),]  # Capping in the upper value
subset21$ovrrev_Mean =35
mydata2[which(mydata2$ovrrev_Mean >35),]=subset21
boxplot(mydata2$ovrrev_Mean ,main ="Boxplot of Mean overage revenue") 
hist(mydata2$ovrrev_Mean ,main ="Histogram of Mean overage revenue")
ggplot(mydata2,aes(ovrrev_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(ovrrev_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$ovrrev_Mean ,data=mydata2,fill=churn)

boxplot(mydata2$rev_Mean ,main ="Boxplot of Mean monthly revenue (charge amount)") 
#outliers exist in the upper end
quantile(mydata2$rev_Mean ,probs = seq(0,1,0.05))
subset22=mydata2[which(mydata2$rev_Mean >128),]  # Capping in the upper value
subset22$rev_Mean =128
mydata2[which(mydata2$rev_Mean >128),]=subset22
boxplot(mydata2$rev_Mean ,main ="Boxplot of Range of Mean monthly revenue (charge amount)") 
hist(mydata2$rev_Mean ,main ="Histogram of Range of Mean monthly revenue (charge amount)")
ggplot(mydata2,aes(rev_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(rev_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$rev_Mean ,data=mydata2,fill=churn)

boxplot(mydata2$ovrmou_Mean ,main ="Boxplot of Mean overage minutes of use") 
#outliers exist in the upper end
quantile(mydata2$ovrmou_Mean ,probs = seq(0,1,0.05))
subset23=mydata2[which(mydata2$ovrmou_Mean >103),]  # Capping in the upper value
subset23$ovrmou_Mean =103
mydata2[which(mydata2$ovrmou_Mean >103),]=subset23
boxplot(mydata2$ovrmou_Mean ,main ="Boxplot of Mean overage minutes of use") 
hist(mydata2$ovrmou_Mean ,main ="Histogram of Mean overage minutes of use")
ggplot(mydata2,aes(ovrmou_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(ovrmou_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$ovrmou_Mean ,data=mydata2,fill=churn)

boxplot(mydata2$comp_vce_Mean ,main ="Boxplot of Mean number of completed voice calls") 
#outliers exist in the upper end
quantile(mydata2$comp_vce_Mean ,probs = seq(0,1,0.05))
subset24=mydata2[which(mydata2$comp_vce_Mean >341),]  # Capping in the upper value
subset24$comp_vce_Mean =341
mydata2[which(mydata2$comp_vce_Mean >341),]=subset24
boxplot(mydata2$comp_vce_Mean ,main ="Boxplot of Mean number of completed voice calls") 
hist(mydata2$comp_vce_Mean ,main ="Histogram of Mean number of completed voice calls")
ggplot(mydata2,aes(comp_vce_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(comp_vce_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$comp_vce_Mean ,data=mydata2,fill=churn)


boxplot(mydata2$plcd_vce_Mean ,main ="Boxplot of Mean number of attempted voice calls placed") 
#outliers exist in the upper end
quantile(mydata2$plcd_vce_Mean ,probs = seq(0,1,0.05))
subset25=mydata2[which(mydata2$plcd_vce_Mean >448),]  # Capping in the upper value
subset25$plcd_vce_Mean =448
mydata2[which(mydata2$plcd_vce_Mean >448),]=subset25
boxplot(mydata2$plcd_vce_Mean ,main ="Boxplot of Mean number of attempted voice calls placed") 
hist(mydata2$plcd_vce_Mean ,main ="Histogram of Mean number of attempted voice calls placed")
ggplot(mydata2,aes(plcd_vce_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(plcd_vce_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$plcd_vce_Mean ,data=mydata2,fill=churn)

boxplot(mydata2$avg3mou ,main ="Boxplot of Average monthly minutes of use over the previous three months") 
#outliers exist in the upper end
quantile(mydata2$avg3mou ,probs = seq(0,1,0.05))
subset26=mydata2[which(mydata2$avg3mou >1604),]  # Capping in the upper value
subset26$avg3mou =1604
mydata2[which(mydata2$avg3mou >1604),]=subset26
boxplot(mydata2$avg3mou ,main ="Boxplot of Average monthly minutes of use over the previous three months") 
hist(mydata2$avg3mou ,main ="Histogram of Average monthly minutes of use over the previous three months")
ggplot(mydata2,aes(avg3mou ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(avg3mou ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$avg3mou ,data=mydata2,fill=churn)


boxplot(mydata2$avgmou ,main ="Boxplot of Average monthly minutes of use over the life of the customer") 
#outliers exist in the upper end
quantile(mydata2$avgmou ,probs = seq(0,1,0.05))
subset27=mydata2[which(mydata2$avgmou >1409),]  # Capping in the upper value
subset27$avgmou =1409
mydata2[which(mydata2$avgmou >1409),]=subset27
boxplot(mydata2$avgmou ,main ="Boxplot of Average monthly minutes of use over the life of the customer") 
hist(mydata2$avgmou ,main ="Histogram of Average monthly minutes of use over the life of the customer")
ggplot(mydata2,aes(avgmou ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(avgmou ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$avgmou ,data=mydata2,fill=churn)


boxplot(mydata2$avg3qty ,main ="Boxplot of Average monthly number of calls over the previous three months") 
#outliers exist in the upper end
quantile(mydata2$avg3qty ,probs = seq(0,1,0.05))
subset28=mydata2[which(mydata2$avg3qty >532),]  # Capping in the upper value
subset28$avg3qty =532
mydata2[which(mydata2$avg3qty >532),]=subset28
boxplot(mydata2$avg3qty ,main ="Boxplot of Average monthly number of calls over the previous three months") 
hist(mydata2$avg3qty ,main ="Histogram of Average monthly number of calls over the previous three months")
ggplot(mydata2,aes(avg3qty ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(avg3qty ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$avg3qty ,data=mydata2,fill=churn)

boxplot(mydata2$avgqty ,main ="Boxplot of Average monthly number of calls over the life of the customer") 
#outliers exist in the upper end
quantile(mydata2$avgqty ,probs = seq(0,1,0.05))
subset29=mydata2[which(mydata2$avgqty >488),]  # Capping in the upper value
subset29$avgqty =488
mydata2[which(mydata2$avgqty >488),]=subset29
boxplot(mydata2$avgqty ,main ="Boxplot of Average monthly number of calls over the life of the customer") 
hist(mydata2$avgqty ,main ="Histogram of Average monthly number of calls over the life of the customer")
ggplot(mydata2,aes(avgqty ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(avgqty ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$avgqty ,data=mydata2,fill=churn)

boxplot(mydata2$avg6mou ,main ="Boxplot of Average monthly minutes of use over the previous six months") 
#outliers exist in the upper end
quantile(mydata2$avg6mou ,probs = seq(0,1,0.05))
subset30=mydata2[which(mydata2$avg6mou >1499),]  # Capping in the upper value
subset30$avg6mou =1499
mydata2[which(mydata2$avg6mou >1499),]=subset30
boxplot(mydata2$avg6mou ,main ="Boxplot of Average monthly minutes of use over the previous six months") 
hist(mydata2$avg6mou ,main ="Histogram of Average monthly minutes of use over the previous six months")
ggplot(mydata2,aes(avg6mou ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(avg6mou ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$avg6mou ,data=mydata2,fill=churn)


boxplot(mydata2$avg6qty ,main ="Boxplot of Average monthly number of calls over the previous six months") 
#outliers exist in the upper end
quantile(mydata2$avg6qty ,probs = seq(0,1,0.05))
subset31=mydata2[which(mydata2$avg6qty >502),]  # Capping in the upper value
subset31$avg6qty =502
mydata2[which(mydata2$avg6qty >502),]=subset31
boxplot(mydata2$avg6qty ,main ="Boxplot of Average monthly number of calls over the previous six months") 
hist(mydata2$avg6qty ,main ="Histogram of Average monthly number of calls over the previous six months")
ggplot(mydata2,aes(avg6qty ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(avg6qty ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$avg6qty ,data=mydata2,fill=churn)

boxplot(mydata2$age1 ,main ="Boxplot of Age of first household member") 
#outliers does not exist
hist(mydata2$age1 ,main ="Histogram of Age of first household member")
ggplot(mydata2,aes(age1 ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(age1 ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$age1 ,data=mydata2,fill=churn)

boxplot(mydata2$age2 ,main ="Boxplot of Age of second household member") 
#outliers does not exist 
hist(mydata2$age2 ,main ="Histogram of Age of second household member")
ggplot(mydata2,aes(age2 ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(age2 ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$age2 ,data=mydata2,fill=churn)


boxplot(mydata2$models ,main ="Boxplot of Number of models issued") 
#outliers exist in the upper end
quantile(mydata2$models ,probs = seq(0,1,0.05))
subset32=mydata2[which(mydata2$models >3.5),]  # Capping in the upper value
subset32$models =3.5
mydata2[which(mydata2$models >3.5),]=subset32
boxplot(mydata2$models ,main ="Boxplot of Number of models issued") 
hist(mydata2$models ,main ="Histogram of Number of models issued")
ggplot(mydata2,aes(models ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(models ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$models ,data=mydata2,fill=churn)

boxplot(mydata2$hnd_price ,main ="Boxplot of Current handset price") 
#outliers exist in the upper end
quantile(mydata2$hnd_price ,probs = seq(0,1,0.05))
subset33=mydata2[which(mydata2$hnd_price >284),]  # Capping in the upper value
subset33$hnd_price =284
mydata2[which(mydata2$hnd_price >284),]=subset33
boxplot(mydata2$hnd_price ,main ="Boxplot of Current handset price") 
hist(mydata2$hnd_price ,main ="Histogram of Current handset price")
ggplot(mydata2,aes(hnd_price ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(hnd_price ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$hnd_price ,data=mydata2,fill=churn)

boxplot(mydata2$actvsubs ,main ="Boxplot of Number of active subscribers in household") 
#outliers exist in the upper end
quantile(mydata2$actvsubs ,probs = seq(0,1,0.05))
subset34=mydata2[which(mydata2$actvsubs >3.5),]  # Capping in the upper value
subset34$actvsubs =3.5
mydata2[which(mydata2$actvsubs >3.5),]=subset34
boxplot(mydata2$actvsubs ,main ="Boxplot of Number of active subscribers in household") 
hist(mydata2$actvsubs ,main ="Histogram of Number of active subscribers in household")
ggplot(mydata2,aes(actvsubs ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(actvsubs ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$actvsubs ,data=mydata2,fill=churn)

boxplot(mydata2$uniqsubs ,main ="Boxplot of Number of unique subscribers in the household") 
#outliers exist in the upper end
quantile(mydata2$uniqsubs ,probs = seq(0,1,0.05))
subset35=mydata2[which(mydata2$uniqsubs >3.5),]  # Capping in the upper value
subset35$uniqsubs =3.5
mydata2[which(mydata2$uniqsubs >3.5),]=subset35
boxplot(mydata2$uniqsubs ,main ="Boxplot of Number of unique subscribers in the household") 
hist(mydata2$uniqsubs ,main ="Histogram of Number of unique subscribers in the household")
ggplot(mydata2,aes(uniqsubs ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(uniqsubs ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$uniqsubs ,data=mydata2,fill=churn)


quantile(mydata2$opk_dat_Mean ,probs = seq(0,1,0.05))
hist(mydata2$opk_dat_Mean ,main ="Histogram of Mean number of off-peak data calls")
ggplot(mydata2,aes(opk_dat_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(opk_dat_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$opk_dat_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(opk_dat_Mean,fill=churn,color=churn))+geom_dotplot()

boxplot(mydata2$roam_Mean ,main ="Boxplot of Mean number of roaming calls") 
#outliers exist in the upper end
quantile(mydata2$roam_Mean ,probs = seq(0,1,0.05))
subset37=mydata2[which(mydata2$roam_Mean >.64),]  # Capping in the upper value
subset37$roam_Mean =.64
mydata2[which(mydata2$roam_Mean >.64),]=subset37
boxplot(mydata2$roam_Mean ,main ="Boxplot of Mean number of roaming calls") 
hist(mydata2$roam_Mean ,main ="Histogram of Mean number of roaming calls")
ggplot(mydata2,aes(roam_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(roam_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$roam_Mean ,data=mydata2,fill=churn)


quantile(mydata2$recv_sms_Mean ,probs = seq(0,1,0.05))
hist(mydata2$recv_sms_Mean ,main ="Histogram of Mean number of received SMS calls")
ggplot(mydata2,aes(recv_sms_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(recv_sms_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$recv_sms_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(recv_sms_Mean,fill=churn,color=churn))+geom_dotplot()


quantile(mydata2$blck_dat_Mean ,probs = seq(0,1,0.05))
hist(mydata2$blck_dat_Mean ,main ="Histogram of Mean number of blocked (failed) data calls")
ggplot(mydata2,aes(blck_dat_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(blck_dat_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$blck_dat_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(blck_dat_Mean,fill=churn,color=churn))+geom_dotplot()


quantile(mydata2$mou_pead_Mean ,probs = seq(0,1,0.05))
hist(mydata2$mou_pead_Mean ,main ="Histogram of Mean unrounded minutes of use of peak data calls")
ggplot(mydata2,aes(mou_pead_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(mou_pead_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$mou_pead_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(mou_pead_Mean,fill=churn,color=churn))+geom_dotplot()

boxplot(mydata2$da_Mean ,main ="Boxplot of Mean number of directory assisted calls") 
#outliers exist in the upper end
quantile(mydata2$da_Mean ,probs = seq(0,1,0.05))
subset41=mydata2[which(mydata2$da_Mean >2.4),]  # Capping in the upper value
subset41$da_Mean =2.4
mydata2[which(mydata2$da_Mean >2.4),]=subset41
boxplot(mydata2$da_Mean ,main ="Boxplot of Mean number of directory assisted calls") 
hist(mydata2$da_Mean ,main ="Histogram of Mean number of directory assisted calls")
ggplot(mydata2,aes(da_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(da_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$da_Mean ,data=mydata2,fill=churn)

boxplot(mydata2$da_Range ,main ="Boxplot of Range of number of directory assisted calls") 
#outliers exist in the upper end
quantile(mydata2$da_Range ,probs = seq(0,1,0.05))
subset42=mydata2[which(mydata2$da_Range >4.9),]  # Capping in the upper value
subset42$da_Range =4.9
mydata2[which(mydata2$da_Range >4.9),]=subset42
boxplot(mydata2$da_Range ,main ="Boxplot of Range of number of directory assisted calls") 
hist(mydata2$da_Range ,main ="Histogram of Range of number of directory assisted calls")
ggplot(mydata2,aes(da_Range ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(da_Range ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$da_Range ,data=mydata2,fill=churn)


quantile(mydata2$datovr_Mean ,probs = seq(0,1,0.05))
hist(mydata2$datovr_Mean ,main ="Histogram of Mean revenue of data overage")
ggplot(mydata2,aes(datovr_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(datovr_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$datovr_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(datovr_Mean,fill=churn,color=churn))+geom_dotplot()


quantile(mydata2$datovr_Range ,probs = seq(0,1,0.05))
hist(mydata2$datovr_Range ,main ="Histogram of Range of revenue of data overage")
ggplot(mydata2,aes(datovr_Range ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(datovr_Range ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$datovr_Range ,data=mydata2,fill=churn)
ggplot(mydata2,aes(datovr_Range,fill=churn,color=churn))+geom_dotplot()


quantile(mydata2$drop_dat_Mean ,probs = seq(0,1,0.05))
hist(mydata2$drop_dat_Mean ,main ="Histogram of Mean number of dropped (failed) data calls")
ggplot(mydata2,aes(drop_dat_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(drop_dat_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$drop_dat_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(drop_dat_Mean,fill=churn,color=churn))+geom_dotplot()

boxplot(mydata2$drop_vce_Mean ,main ="Boxplot of Mean number of dropped (failed) voice calls") 
#outliers exist in the upper end
quantile(mydata2$drop_vce_Mean ,probs = seq(0,1,0.05))
subset46=mydata2[which(mydata2$drop_vce_Mean >18),]  # Capping in the upper value
subset46$drop_vce_Mean =18
mydata2[which(mydata2$drop_vce_Mean >18),]=subset46
boxplot(mydata2$drop_vce_Mean ,main ="Boxplot of Mean number of dropped (failed) voice calls") 
hist(mydata2$drop_vce_Mean ,main ="Histogram of Mean number of dropped (failed) voice calls")
ggplot(mydata2,aes(drop_vce_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(drop_vce_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$drop_vce_Mean ,data=mydata2,fill=churn)


quantile(mydata2$adjmou ,probs = seq(0,1,0.05))
hist(mydata2$adjmou ,main ="Histogram of Billing adjusted total minutes of use over the life of the customer")
ggplot(mydata2,aes(adjmou ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(adjmou ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$adjmou ,data=mydata2,fill=churn)
ggplot(mydata2,aes(adjmou,fill=churn,color=churn))+geom_dotplot()


quantile(mydata2$totrev ,probs = seq(0,1,0.05))
hist(mydata2$totrev ,main ="Histogram of Total revenue")
ggplot(mydata2,aes(totrev ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(totrev ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$totrev ,data=mydata2,fill=churn)
ggplot(mydata2,aes(totrev,fill=churn,color=churn))+geom_dotplot()


boxplot(mydata2$adjrev ,main ="Boxplot of Billing adjusted total revenue over the life of the customer") 
#outliers exist in the upper end
quantile(mydata2$adjrev ,probs = seq(0,1,0.05))
subset49=mydata2[which(mydata2$adjrev >2315),]  # Capping in the upper value
subset49$adjrev =2315
mydata2[which(mydata2$adjrev >2315),]=subset49
boxplot(mydata2$adjrev ,main ="Boxplot of Billing adjusted total revenue over the life of the customer") 
hist(mydata2$adjrev ,main ="Histogram of Billing adjusted total revenue over the life of the customer")
ggplot(mydata2,aes(adjrev ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(adjrev ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$adjrev ,data=mydata2,fill=churn)

boxplot(mydata2$avgrev ,main ="Boxplot of Average monthly revenue over the life of the customer") 
#outliers exist in the upper end
quantile(mydata2$avgrev ,probs = seq(0,1,0.05))
subset50=mydata2[which(mydata2$avgrev >120),]  # Capping in the upper value
subset50$avgrev =120
mydata2[which(mydata2$avgrev >120),]=subset50
boxplot(mydata2$avgrev ,main ="Boxplot of Average monthly revenue over the life of the customer") 
hist(mydata2$avgrev ,main ="Histogram of Average monthly revenue over the life of the customer")
ggplot(mydata2,aes(avgrev ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(avgrev ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$avgrev ,data=mydata2,fill=churn)


quantile(mydata2$comp_dat_Mean ,probs = seq(0,1,0.05))
hist(mydata2$comp_dat_Mean ,main ="Histogram of Mean number of completed data calls")
ggplot(mydata2,aes(comp_dat_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(comp_dat_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$comp_dat_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(comp_dat_Mean,fill=churn,color=churn))+geom_dotplot()


quantile(mydata2$plcd_dat_Mean ,probs = seq(0,1,0.05))
hist(mydata2$plcd_dat_Mean ,main ="Histogram of Mean number of attempted data calls placed")
ggplot(mydata2,aes(plcd_dat_Mean ,fill=churn,color=churn))+geom_density(alpha=0.1)
g1=ggplot(mydata2,aes(plcd_dat_Mean ,fill=Churn,color=Churn))+geom_density(alpha=0.1)
qplot(mydata2$plcd_dat_Mean ,data=mydata2,fill=churn)
ggplot(mydata2,aes(plcd_dat_Mean,fill=churn,color=churn))+geom_dotplot()

#Categorical Data

qplot(mydata2$income,data=mydata2,fill=churn)
counts1 = table(mydata2$income, mydata2$churn)
chisq.test(counts1) # variables are related to each other as pvalue is less than .05
fisher.test(counts1,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts1, main="Income vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts1), beside=TRUE)
prop.table(table(mydata2$income,mydata2$churn),1)*100
table(mydata2$churn, mydata2$income)

qplot(mydata2$crclscod,data=mydata2,fill=churn)
prop.table(table(mydata2$crclscod,mydata2$churn),1)*100

qplot(mydata2$asl_flag,data=mydata2,fill=churn)
counts2 = table(mydata2$asl_flag, mydata2$churn)
chisq.test(counts2) # variables are related to each other as pvalue is less than .05
fisher.test(counts2,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts2, main="Account spending limit vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts2), beside=TRUE)
prop.table(table(mydata2$asl_flag,mydata2$churn),1)*100
table(mydata2$churn, mydata2$asl_flag)

qplot(mydata2$prizm_social_one,data=mydata2,fill=churn)
counts3 = table(mydata2$prizm_social_one, mydata2$churn)
chisq.test(counts3) # variables are related to each other as pvalue is less than .05
fisher.test(counts3,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts3, main="Social group letter only vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts3), beside=TRUE)
prop.table(table(mydata2$prizm_social_one,mydata2$churn),1)*100
table(mydata2$churn, mydata2$prizm_social_one)

qplot(mydata2$area,data=mydata2,fill=churn)
counts4 = table(mydata2$area, mydata2$churn)
chisq.test(counts4) # variables are related to each other as pvalue is less than .05
fisher.test(counts4,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts4, main="Geographic area vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts4), beside=TRUE)
prop.table(table(mydata2$area,mydata2$churn),1)*100
table(mydata2$churn, mydata2$area)

qplot(mydata2$refurb_new,data=mydata2,fill=churn)
counts5 = table(mydata2$refurb_new, mydata2$churn)
chisq.test(counts5) # variables are related to each other as pvalue is less than .05
fisher.test(counts5,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts5, main="Handset: refurbished or new vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts5), beside=TRUE)
prop.table(table(mydata2$refurb_new,mydata2$churn),1)*100
table(mydata2$churn, mydata2$refurb_new)

qplot(mydata2$hnd_webcap,data=mydata2,fill=churn)
counts6 = table(mydata2$hnd_webcap, mydata2$churn)
chisq.test(counts6) # variables are related to each other as pvalue is less than .05
fisher.test(counts6,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts6, main="Handset web capability vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts6), beside=TRUE)
prop.table(table(mydata2$hnd_webcap,mydata2$churn),1)*100
table(mydata2$churn, mydata2$hnd_webcap)

qplot(mydata2$marital,data=mydata2,fill=churn)
counts7 = table(mydata2$marital, mydata2$churn)
chisq.test(counts7) # variables are related to each other as pvalue is less than .05
fisher.test(counts7,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts7, main="Marital status vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts7), beside=TRUE)
prop.table(table(mydata2$marital,mydata2$churn),1)*100
table(mydata2$churn, mydata2$marital)

qplot(mydata2$ethnic,data=mydata2,fill=churn)
counts8 = table(mydata2$ethnic, mydata2$churn)
chisq.test(counts8) # variables are related to each other as pvalue is less than .05
fisher.test(counts8,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts8, main="Ethnicity roll-up code vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts8), beside=TRUE)
prop.table(table(mydata2$ethnic,mydata2$churn),1)*100
table(mydata2$churn, mydata2$ethnic)

qplot(mydata2$forgntvl,data=mydata2,fill=churn)
counts9 = table(mydata2$forgntvl, mydata2$churn)
chisq.test(counts9) # variables are not related to each other as pvalue is greater than .05
fisher.test(counts9,simulate.p.value = T) # variables are not related to each other as pvalue is greater than .05
barplot(counts9, main="Foreign travel dummy variable vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts9), beside=TRUE)
prop.table(table(mydata2$forgntvl,mydata2$churn),1)*100
table(mydata2$churn, mydata2$forgntvl)

qplot(mydata2$dwlltype,data=mydata2,fill=churn)
counts10 = table(mydata2$dwlltype, mydata2$churn)
chisq.test(counts10) # variables are related to each other as pvalue is less than .05
fisher.test(counts10,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts10, main="Dwelling unit type vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts10), beside=TRUE)
prop.table(table(mydata2$dwlltype,mydata2$churn),1)*100
table(mydata2$churn, mydata2$dwlltype)

qplot(mydata2$dwllsize,data=mydata2,fill=churn)
counts11 = table(mydata2$dwllsize, mydata2$churn)
chisq.test(counts11) # variables are related to each other as pvalue is less than .05
fisher.test(counts11,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts11, main="Dwelling size vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts11), beside=TRUE)
prop.table(table(mydata2$dwllsize,mydata2$churn),1)*100
table(mydata2$churn, mydata2$dwllsize)

qplot(mydata2$mtrcycle,data=mydata2,fill=churn)
counts12 = table(mydata2$mtrcycle, mydata2$churn)
chisq.test(counts12) # variables are not related to each other as pvalue is greater than .05
fisher.test(counts12,simulate.p.value = T) # variables are not related to each other as pvalue is greater than .05
barplot(counts12, main="Motorcycle indicator vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts12), beside=TRUE)
prop.table(table(mydata2$mtrcycle,mydata2$churn),1)*100
table(mydata2$churn, mydata2$mtrcycle)

qplot(mydata2$truck,data=mydata2,fill=churn)
counts13 = table(mydata2$truck, mydata2$churn)
chisq.test(counts13) # variables are not related to each other as pvalue is greater than .05
fisher.test(counts13,simulate.p.value = T) # variables are not related to each other as pvalue is greater than .05
barplot(counts13, main="Truck indicator vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts13), beside=TRUE)
prop.table(table(mydata2$truck,mydata2$churn),1)*100
table(mydata2$churn, mydata2$truck)

qplot(mydata2$car_buy,data=mydata2,fill=churn)
counts14 = table(mydata2$car_buy, mydata2$churn)
chisq.test(counts14) # variables are related to each other as pvalue is less than .05
fisher.test(counts14,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts14, main="New or used car buyer vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts14), beside=TRUE)
prop.table(table(mydata2$car_buy,mydata2$churn),1)*100
table(mydata2$churn, mydata2$car_buy)

qplot(mydata2$csa,data=mydata2,fill=churn)
counts15 = table(mydata2$csa, mydata2$churn)
chisq.test(counts15) # variables are related to each other as pvalue is less than .05
fisher.test(counts15,simulate.p.value = T) # variables are related to each other as pvalue is less than .05
barplot(counts15, main="Communications local service area vs Churn Status",
        xlab="Churn Status No vs Yes", legend = rownames(counts1), beside=TRUE)
prop.table(table(mydata2$csa,mydata2$churn),1)*100
table(mydata2$churn, mydata2$csa)

str(mydata2)


mydata3=mydata2
str(mydata3[,c(12,31,32,33,34,35,36,37,38,45,46,47,50,55,56,57)])
mydata3$income=as.numeric(mydata3$income)
mydata3$crclscod=as.numeric(mydata3$crclscod)
mydata3$asl_flag =as.numeric(mydata3$asl_flag )
mydata3$prizm_social_one=as.numeric(mydata3$prizm_social_one)
mydata3$area=as.numeric(mydata3$area)
mydata3$refurb_new=as.numeric(mydata3$refurb_new)
mydata3$hnd_webcap=as.numeric(mydata3$hnd_webcap)
mydata3$marital=as.numeric(mydata3$marital)
mydata3$ethnic=as.numeric(mydata3$ethnic)
mydata3$forgntvl=as.numeric(mydata3$forgntvl)
mydata3$dwlltype=as.numeric(mydata3$dwlltype)
mydata3$dwllsize=as.numeric(mydata3$dwllsize)
mydata3$mtrcycle=as.numeric(mydata3$mtrcycle)
mydata3$truck=as.numeric(mydata3$truck)
mydata3$car_buy=as.numeric(mydata3$car_buy)
mydata3$csa=as.numeric(mydata3$csa)


str(mydata3)



### Correlation between the numeric variables

cordata = mydata3[-55]
round(cor(cordata,),2)
corrplot(cor(cordata,),type="lower")


ScaleData=scale(cordata)
ScaleData = as.data.frame(ScaleData)
str(ScaleData)

modeldata=mydata3
modeldata$churn=as.numeric(modeldata$churn)
m = lm(churn ~., data = modeldata) 
summary(m)


vif(m)

#mou_Mean, months, totcalls, adjqty, ovrrev_Mean, rev_Mean, ovrmou_Mean, 
#comp_vce_Mean,  plcd_vce_Mean, avg3mou, avgmou, avg3qty, avgqty, avg6mou, 
#avg6qty, opk_dat_Mean, blck_dat_Mean, da_Mean, da_Range, datovr_Mean, 
#datovr_Range, adjmou, totrev, adjrev, avgrev,comp_dat_Mean and plcd_dat_Mean 
#seems to be less significant in the linear model

modeldata1= modeldata[,-c(1,10,11,19,20,21,22,23,24,25,26,27,28,29,30,48,53,58,59,60,61,64,65,66,67,69,70)]
str(modeldata1)
m1 = lm(churn ~., data = modeldata1) 
summary(m1)
m2=aov(churn~.,data=modeldata1)
summary(m2)

vif(m1)
anova(m1)

#Step 1 - Generate a correlation matrix
matrix1=cor(ScaleData)
matrix1
round(matrix1,2)
corrplot(matrix1,type="lower")

#Step 2 - Bartlett test, H0: All dimensions are same

cortest.bartlett(matrix1)
# If p-value is less than 0.05, then it is an ideal case for dimension reduction

#Step 3: KMO test for checking sampling adequacy

KMO(matrix1)

#Step 4: Extract eigen values
ev=eigen(cor(matrix1))
print(ev,digits=5)
EigenValue=ev$values
EigenValue
#Factor=c(1,2,3,4,5,6,7)
#Scree=data.frame(Factor,EigenValue)
##plot(Scree,main="Scree Plot",col="Blue")
#lines(Scree, col="Red")


#Kaisers

plot(EigenValue, xlab = "Principal Component", ylab="Eigen Value", pch=20, col="blue")
lines(EigenValue,col="red")

#PCA
Unrotate=principal(ScaleData,nfactors = 3,rotate="none")

print(Unrotate,digits = 4)

UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))

Rotate=principal(ScaleData,nfactors = 3,rotate="varimax")
print(Rotate,digits = 4)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)
Rotate$scores


factor.scores(ScaleData,f=Rotate$loadings,method="Harman")

#Rotated
fa1=fa(ScaleData,nfactors = 3,rotate = "none",fm="pa")
fa1
print(fa1)
fa1$scores
attributes(fa1)
fa.diagram(fa1)
fa2 = fa(ScaleData, nfactors=3, rotate="varimax", fm="pa")
fa2
fa.diagram(fa2)

mydata.pca=prcomp(ScaleData,scale=TRUE)
names(mydata.pca)
eig=(mydata.pca$sdev)^2
variance=eig*100/sum(eig)
cumVar=cumsum(variance)
eig.mydata=data.frame(eig=eig,variance=variance,cumvariance=cumVar)
head(eig.mydata)

#LDA

help(lda)
# Fit the model
lda.status=lda(churn~.,data = mydata3,CV=TRUE)
lda.status
lda.status2=lda(churn~.,data = mydata3)
lda.status2
lda.status1=lda(churn~.,prior=c(0.5,0.5),data=mydata3)
lda.status$posterior
lda.status$posterior[,2]
#there is 24% default probability and 76% non default

plot(churn,lda.status$posterior[,2])

status.predicted=ifelse(lda.status$posterior[,2]<.3,"0","1")
status.predicted
table(churn,status.predicted)

roc(churn,lda.status$posterior[,2]) #Accuracy: 62.73
plot.roc(churn,lda.status$posterior[,2])
lda.status$class
table(churn,lda.status$class)


# Make predictions
#predictions <- model %>% predict(data.transformed[,2])
# Model accuracy
#mean(predictions$class==test.transformed$Species)

str(mydata3)


#splitting the dataset into train and test dataset
set.seed(100)
train3=createDataPartition(mydata3$churn,p=0.7,list=FALSE,times=1)
traindata3=mydata3[train3,1:length(mydata3)]
testdata3=mydata3[-train3,1:length(mydata3)]
dim(testdata3)
dim(traindata3)
prop.table(table(traindata3$churn))#76% of train data has not canceled and 24% has canceled service 
prop.table(table(testdata3$churn))#76% of train data has not canceled and 24% has canceled service 
table(traindata3$churn)


#Logistic Regression model

glm(traindata3$churn~.,data = traindata3,family="binomial")
logmodel3=glm(traindata3$churn~.,data = traindata3,family="binomial")
summary(logmodel3)

#rev_Range,drop_vce_Range,owylis_vce_Range,totcalls,custcare_Mean,callwait_Mean,callwait_Range,ccrndmou_Range
#adjqty, plcd_vce_Mean,avg3mou,avg3qty,avgqty,avg6qty, crclscod, prizm_social_one
#marital,age2, models, forgntvl, dwlltype, opk_dat_Mean, mtrcycle,truck,roam_Mean, recv_sms_Mean,blck_dat_Mean,
#mou_pead_Mean,car_buy,csa, da_Mean,da_Range,datovr_Mean,datovr_Range,drop_dat_Mean,drop_vce_Mean,adjmou,
#totrev,adjrev,avgrev,Customer_ID,comp_dat_Mean and plcd_dat_Mean seems to be less significant
#There could be multicollinearity
exp(coef(logmodel3)) #Odds ratio
exp(coef(logmodel3))/(1+exp(coef(logmodel3))) #Probability

#Will create a null model
glm(traindata3$churn~1,data = traindata3,family="binomial")
summary(glm(traindata3$churn~1,data = traindata3,family="binomial"))

#To compare null model and the created model
lrtest(logmodel3)
#p value is 2.2e-16. Null hypothesis is rejected. Hence the model is a valid one.

#Check for multicollinearity


vif(logmodel3) #Heteroscedasticity test
#vif values of mou_Mean,months,--totcalls,--adjqty,ovrrev_Mean,rev_Mean, 
#ovrmou_Mean, comp_vce_Mean, plcd_vce_Mean, avg3mou,avgmou, avg3qty,avgqty, 
#avg6mou, avg6qty, opk_dat_Mean, da_Mean,da_Range,datovr_Mean,datovr_Range,
#adjrev,totrev,adjmou,--plcd_dat_Mean,--comp_dat_Mean,avgrev are too high
#they seem to be correlated
#Hence there is multicollinearity



#Create a model after dropping all the mentioned parameters
glm(traindata3$churn~.,data = traindata3[,-c(1,10,11,19,20,21,22,23,24,25,26,27,28,29,30,58,59,60,61,64,65,66,67,69,70)],family="binomial")
logmodel4=glm(traindata3$churn~.,data = traindata3[,-c(1,10,11,19,20,21,22,23,24,25,26,27,28,29,30,58,59,60,61,64,65,66,67,69,70)],family="binomial")
summary(logmodel4)
exp(coef(logmodel4)) #Odds ratio
exp(coef(logmodel4))/(1+exp(coef(logmodel4))) #Probability



vif(logmodel4)

#The values are less than 5, hence there is no multicollinearity

logmodel4$coefficients
likelihood=exp(logmodel4$coefficients)
print(likelihood)
#If there is 1 unit change in drop_blk_Mean, there is 1.01107577 units change in the odds of Churn being '1' 
#Probability=1.01107577/1+1.01107577 = 0.502753693
#If there is 1 unit increase in drop_blk_Mean, probability of customer canceling the service increases by 50.28% 


#We shall predict on test data

predictTest=predict(logmodel4,newdata = testdata3,type="response")
table(testdata3$churn,(predictTest>0.16))
#Confusion matrix with threshold of 0.5
table(testdata3$churn,(predictTest>0.5))
head(predictTest)

#Accuracy
sum(diag(table(testdata3$churn,(predictTest>0.5))))/nrow(testdata3) #0.7592709
#This predicts well on test data
table(testdata3$churn)
prop.table(table(testdata3$churn))

#Test set AUC
ROCRPredictTest=prediction(predictTest,testdata3$churn)
perf1=performance(ROCRPredictTest,"tpr","fpr")
plot(perf1)
as.numeric(performance(ROCRPredictTest,"auc")@y.values) #AUC is 0.610479
#If I build a model on my training dataset & then look at a new set of data, & pick from it 
#random customers who cancelled and not cancelled the service, then 61% of the time, the churned customers
#will have higher predicted churn and the non churn customers will have low predicted churn


#Step AIC Regression method

step_both=stepAIC(logmodel4,direction = "both")
summary(step_both)
#Given all the variables that are significant
#and gives the best model with AIC: 19930

#Model tuning and building model using balanced data
control=trainControl(method = "repeatedcv",number = 5,repeats = 3,sampling = "up")
model=train(churn~.,data=traindata3,trControl=control,method='glm')
model #Accuracy : 0.5931151
summary(model)
varImp(model)
#eqpdays,asl_flag and uniqsubs is found to be the most important variables

#We shall predict on the test data

pred=predict(model,newdata = testdata3)
table(testdata3$churn,pred)
sum(diag(table(testdata3$churn,pred)))/sum(table(testdata3$churn,pred))

#Accuracy of 58.85607%
#Specificity and Sensitivity also shows that it is a good model


##CART Model##

CTModel=rpart(formula=churn~.,data =traindata3 ,minsplit =150,method="class",xval=10,cp=0)
rpart.plot(CTModel,cex=0.6)
print(CTModel)
printcp(CTModel)
attributes(CTModel)
CTModel$cptable
plotcp(CTModel)


#We will have to prune the data considering .0013 as the pruned parameter from the rpart plot
ptree=prune(CTModel,cp=.0013,"cp")
printcp(ptree)
#rpart(formula=ptree$churn~.,data=ptree,method = "class",control = r.ctrl)
rpart.plot(ptree,cex=0.6)
ptree
path.rpart(ptree,c(1:12))

#predict on train data
predCT_train=predict(CTModel,newdata = traindata3,type = "class")

#Confusion matrix
tab1=table(traindata3$churn,predCT_train) 
tab1  ##we get the confusion matrix
sum(diag(tab1))/nrow(traindata3)#Accuracy is 77.25% on train data

#predict on test data
#Carttestdata=testdata2[-c(31,34,38,47,57)]
predCT_test=predict(CTModel,newdata = testdata3,type = "class")
#predCT_test$prediction=predict(ptree,newdata = testdata2)
predCT_test$predictscore=predict(ptree,newdata = testdata3,type="prob")
predCT_test

#Confusion matrix
tab=table(testdata3$churn,predCT_test) 
tab  ##we get the confusion matrix
sum(diag(tab))/nrow(testdata3)
# Overall accuracy is 74.81% on test data

confusionMatrix(testdata3$churn,predCT_test)
#confusionMatrix(table(as.factor(testdata3$churn),predCT_test$prediction))

##ROC for pruned tree
pred.cart=predict(ptree,newdata=testdata3,type = "prob")[,2]
Pred2=prediction(pred.cart,testdata3$churn)
pred.cart1=predict(ptree,newdata=traindata3,type = "prob")[,2]
Pred3=prediction(pred.cart1,traindata3$churn)
plot(performance(Pred2,"tpr","fpr"))
plot(performance(Pred3,"tpr","fpr"))
abline(0,1,lty=2)

#plotting AUC
auc.perf=performance(Pred2,"auc")
auc=as.numeric(auc.perf@y.values)
auc.perf1=performance(Pred3,"auc")
auc1=as.numeric(auc.perf@y.values)
print(auc)  #Area under the curve is around 0.6113


#CART Model is around to 61.13% accurate in predicting churn on train and test data

##Random Forest Model##

library(randomForest)
set.seed(1000)
#train_new1=train_new[1:12]
str(traindata3)
rndForest=randomForest(churn~.,data=traindata3,type="class",nTree=1001,
                       nodesize=10,importance=TRUE)
rndForest
print(rndForest)
print(rndForest$err.rate)
plot(rndForest)
importance(rndForest)

#Prediction on train data
pred=predict(rndForest,newdata = traindata3)
confusionMatrix(traindata3$churn, pred)
tab2=table(traindata3$churn,pred)
tab2 # Confusion matrix
sum(diag(tab2)/sum(tab2))  ##Accuracy provided is 97.25%


#Prediction on test data
pred2=predict(rndForest,newdata = testdata3)
confusionMatrix(testdata3$churn, pred2)
tab3=table(testdata3$churn,pred2)
tab3 # Confusion matrix
sum(diag(tab3)/sum(tab3))  ##Accuracy provided is 76.49%

#Tuning the RF model
set.seed(1000)
tRndForest=tuneRF(x=traindata3[,-55],y=traindata3$churn,mtryStart=3,stepFactor=1.5,ntreeTry = 451,
improve=0.0001,trace=TRUE,plot=TRUE,doBest=TRUE,nodesize=10,importance=TRUE)
tRndForest
print(tRndForest)
plot(tRndForest)
tRndForest$importance
predRF_test=predict(tRndForest,testdata3,type="class",mtry=3)
tab=table(testdata3$churn,predRF_test)
sum(diag(tab))/sum(tab)# Accuracy=76.35%

traindata3$predict.class=predict(tRndForest,traindata3,type="class")


importance(tRndForest)


##ROC for Random forest

pred_rf=predict(rndForest,testdata3,type='prob')[,2]
require(pROC)
rf.roc=roc(testdata3$churn,pred_rf)
plot(rf.roc)
rf.roc
##ROC is close to ideal one
as.numeric(auc.perf@y.values)
auc(testdata3$churn,predict(rndForest,testdata3,type='prob')[,2])
##Area under the curve is .6704




varImpPlot(rndForest,sort = T,n.var=10,main = "Top 12 - Variable Importance")
