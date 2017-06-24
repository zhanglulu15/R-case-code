install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

############分类回归树: rpart包
BuyOrNot<-read.table(file="消费决策数据.txt",header=TRUE)
BuyOrNot$Income<-as.factor(BuyOrNot$Income)
BuyOrNot$Gender<-as.factor(BuyOrNot$Gender)

Ctl<-rpart.control(minsplit=2,maxcompete=4,xval=10,maxdepth=10,cp=0)
set.seed(12345)
TreeFit1<-rpart(Purchase~.,data=BuyOrNot,method="class",parms=list(split="gini"),control=Ctl)
rpart.plot(TreeFit1,type=4,branch=0,extra=2)
printcp(TreeFit1)
plotcp(TreeFit1)

set.seed(12345)
(TreeFit2<-rpart(Purchase~.,data=BuyOrNot,method="class",parms=list(split="gini"))) 
rpart.plot(TreeFit2,type=4,branch=0,extra=2)
printcp(TreeFit2)

TreeFit3<-prune(TreeFit1,cp=0.008) 
rpart.plot(TreeFit3,type=4,branch=0,extra=2)
printcp(TreeFit3)
plotcp(TreeFit3)

#######################建立单个分类树
library("rpart")
MailShot<-read.table(file="邮件营销数据.txt",header=TRUE)
MailShot<-MailShot[,-1]
Ctl<-rpart.control(minsplit=20,maxcompete=4,maxdepth=30,cp=0.01,xval=10)
set.seed(12345)
TreeFit<-rpart(MAILSHOT~.,data=MailShot,method="class",parms=list(split="gini"))
#rpart.plot(TreeFit,type=4,branch=0,extra=1)
CFit1<-predict(TreeFit,MailShot,type="class")  #CFit1<-predict(TreeFit,MailShot)
ConfM1<-table(MailShot$MAILSHOT,CFit1)
(E1<-(sum(ConfM1)-sum(diag(ConfM1)))/sum(ConfM1))

###############利用ipred包中的bagging建立组合分类树1
install.packages("ipred")
library("ipred")
set.seed(12345)
(BagM1<-bagging(MAILSHOT~.,data=MailShot,nbagg=25,coob=TRUE,control=Ctl))
CFit2<-predict(BagM1,MailShot,type="class")
ConfM2<-table(MailShot$MAILSHOT,CFit2)
(E2<-(sum(ConfM2)-sum(diag(ConfM2)))/sum(ConfM2))

#####################利用adabag包中的bagging建立组合分类树2
install.packages("adabag")
detach("package:ipred")
library("adabag")
MailShot<-read.table(file="邮件营销数据.txt",header=TRUE)
MailShot<-MailShot[,-1]
Ctl<-rpart.control(minsplit=20,maxcompete=4,maxdepth=30,cp=0.01,xval=10)
set.seed(12345)
BagM2<-bagging(MAILSHOT~.,data=MailShot,control=Ctl,mfinal = 25)
BagM2$importance
CFit3<-predict.bagging(BagM2,MailShot)
CFit3$confusion
CFit3$error

###################利用adabag包中的boosting建立组合模型
library("adabag")
MailShot<-read.table(file="邮件营销数据.txt",header=TRUE)
MailShot<-MailShot[,-1]
Ctl<-rpart.control(minsplit=20,maxcompete=4,maxdepth=30,cp=0.01,xval=10)
set.seed(12345)
BoostM<-boosting(MAILSHOT~.,data=MailShot,boos=TRUE,mfinal=25,coeflearn="Breiman",control=Ctl)
BoostM$importance
ConfM4<-table(MailShot$MAILSHOT,BoostM$class)
(E4<-(sum(ConfM4)-sum(diag(ConfM4)))/sum(ConfM4))

#########################随机森林
install.packages("randomForest")
library("randomForest")
MailShot<-read.table(file="邮件营销数据.txt",header=TRUE)
MailShot<-MailShot[,-1]
set.seed(12345)
(rFM<-randomForest(MAILSHOT~.,data=MailShot,importance=TRUE,proximity=TRUE))
head(rFM$votes)             
head(rFM$oob.times)       
DrawL<-par()
par(mfrow=c(2,1),mar=c(5,5,3,1))
plot(rFM,main="随机森林的OOB错判率和决策树棵树")
plot(margin(rFM),type="h",main="边界点探测",xlab="观测序列",ylab="比率差") 
par(DrawL)
Fit<-predict(rFM,MailShot)
ConfM5<-table(MailShot$MAILSHOT,Fit)
(E5<-(sum(ConfM5)-sum(diag(ConfM5)))/sum(ConfM5))

head(treesize(rFM))   
head(getTree(rfobj=rFM,k=1,labelVar=TRUE))
barplot(rFM$importance[,3],main="输入变量重要性测度(预测精度变化)指标柱形图")
box()
importance(rFM,type=1)
varImpPlot(x=rFM, sort=TRUE, n.var=nrow(rFM$importance),main="输入变量重要性测度散点图") 

