install.packages("e1071")

#############模拟线性可分下的SVM
set.seed(12345)
x<-matrix(rnorm(n=40*2,mean=0,sd=1),ncol=2,byrow=TRUE)
y<-c(rep(-1,20),rep(1,20))
x[y==1,]<-x[y==1,]+1.5
data_train<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y))  #生成训练样本集

x<-matrix(rnorm(n=20,mean=0,sd=1),ncol=2,byrow=TRUE)
y<-sample(x=c(-1,1),size=10,replace=TRUE)
x[y==1,]<-x[y==1,]+1.5
data_test<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y)) #生成测试样本集

plot(data_train[,2:1],col=as.integer(as.vector(data_train[,3]))+2,pch=8,cex=0.7,main="训练样本集-1和+1类散点图")
library("e1071")
SvmFit<-svm(Fy~.,data=data_train,type="C-classification",kernel="linear",cost=10,scale=FALSE)
summary(SvmFit)
SvmFit$index
plot(x=SvmFit,data=data_train,formula=Fx1~Fx2,svSymbol="#",dataSymbol="*",grid=100)
SvmFit<-svm(Fy~.,data=data_train,type="C-classification",kernel="linear",cost=0.1,scale=FALSE)
summary(SvmFit)
##############10折交叉验证选取损失惩罚参数C
set.seed(12345)
tObj<-tune.svm(Fy~.,data=data_train,type="C-classification",kernel="linear",
  cost=c(0.001,0.01,0.1,1,5,10,100,1000),scale=FALSE)
summary(tObj)
BestSvm<-tObj$best.model
summary(BestSvm)
yPred<-predict(BestSvm,data_test)
(ConfM<-table(yPred,data_test$Fy))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))

##############模拟线性不可分下的SVM
set.seed(12345)
x<-matrix(rnorm(n=400,mean=0,sd=1),ncol=2,byrow=TRUE)
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
y<-c(rep(1,150),rep(2,50))
data<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y))
flag<-sample(1:200,size=100)
data_train<-data[flag,]
data_test<-data[-flag,]
plot(data_train[,2:1],col=as.integer(as.vector(data_train[,3])),pch=8,cex=0.7,main="训练样本集散点图")
library("e1071")
set.seed(12345)
tObj<-tune.svm(Fy~.,data=data_train,type="C-classification",kernel="radial",
  cost=c(0.001,0.01,0.1,1,5,10,100,1000),gamma=c(0.5,1,2,3,4),scale=FALSE)
plot(tObj,xlab=expression(gamma),ylab="损失惩罚参数C",
  main="不同参数组合下的预测错误率",nlevels=10,color.palette=terrain.colors)
   
BestSvm<-tObj$best.model
summary(BestSvm)
plot(x=BestSvm,data=data_train,formula=Fx1~Fx2,svSymbol="#",dataSymbol="*",grid=100)
yPred<-predict(BestSvm,data_test)
(ConfM<-table(yPred,data_test$Fy))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))


##############模拟多类别的SVM
set.seed(12345)
x<-matrix(rnorm(n=400,mean=0,sd=1),ncol=2,byrow=TRUE)
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
x<-rbind(x,matrix(rnorm(n=100,mean=0,sd=1),ncol=2,byrow=TRUE))
y<-c(rep(1,150),rep(2,50))
y<-c(y,rep(0,50))
x[y==0,2]<-x[y==0,2]+3
data<-data.frame(Fx1=x[,1],Fx2=x[,2],Fy=as.factor(y))
plot(data[,2:1],col=as.integer(as.vector(data[,3]))+1,pch=8,cex=0.7,main="训练样本集散点图")
library("e1071")
set.seed(12345)
tObj<-tune.svm(Fy~.,data=data,type="C-classification",kernel="radial",
  cost=c(0.001,0.01,0.1,1,5,10,100,1000),gamma=c(0.5,1,2,3,4),scale=FALSE)
BestSvm<-tObj$best.model
summary(BestSvm)
plot(x=BestSvm,data=data,formula=Fx1~Fx2,svSymbol="#",dataSymbol="*",grid=100)
SvmFit<-svm(Fy~.,data=data,type="C-classification",kernel="radial",cost=5,gamma=1,scale=FALSE)
head(SvmFit$decision.values)
yPred<-predict(SvmFit,data)
(ConfM<-table(yPred,data$Fy))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))

################天猫数据SVM
Tmall_train<-read.table(file="天猫_Train_1.txt",header=TRUE,sep=",")
Tmall_train$BuyOrNot<-as.factor(Tmall_train$BuyOrNot)
set.seed(12345)
library("e1071")
tObj<-tune.svm(BuyOrNot~.,data=Tmall_train,type="C-classification",kernel="radial",gamma=10^(-6:-3),cost=10^(-3:2))
plot(tObj,xlab=expression(gamma),ylab="损失惩罚参数C",
  main="不同参数组合下的预测错误率",nlevels=10,color.palette=terrain.colors)
BestSvm<-tObj$best.model
summary(BestSvm)
Tmall_test<-read.table(file="天猫_Test_1.txt",header=TRUE,sep=",")
Tmall_test$BuyOrNot<-as.factor(Tmall_test$BuyOrNot)
yPred<-predict(BestSvm,Tmall_test)
(ConfM<-table(yPred,Tmall_test$BuyOrNot))
(Err<-(sum(ConfM)-sum(diag(ConfM)))/sum(ConfM))



