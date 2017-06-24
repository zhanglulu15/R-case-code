
####################可视化模拟数据1
Data<-read.table(file="模式甄别模拟数据1.txt",header=TRUE,sep=",")
head(Data)
plot(Data[,1:2],main="样本观测点的分布",xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8)  #可视化观测点分布特征
####################可视化模拟数据2
Data<-read.table(file="模式甄别模拟数据2.txt",header=TRUE,sep=",")
head(Data)
plot(Data[,1:2],main="样本观测点的分布",xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8)  #可视化观测点分布特征


############模拟数据异常点甄别：EM聚类(无监督)
Data<-read.table(file="模式甄别模拟数据1.txt",header=TRUE,sep=",")
library("mclust") 
EMfit<-Mclust(data=Data[,-3])  
par(mfrow=c(2,2))
Data$ker.scores<-EMfit$uncertainty  
Data.Sort<-Data[order(x=Data$ker.scores,decreasing=TRUE),]
P<-0.1                     
N<-length(Data[,1])         
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main="EM聚类的模式诊断结果(10%)",xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8,col=colP)
library("ROCR")
pd<-prediction(Data$ker.scores,Data$y)
pf1<-performance(pd,measure="rec",x.measure="rpp") 
pf2<-performance(pd,measure="prec",x.measure="rec")   
plot(pf1,main="模式甄别的累计回溯精度曲线")
plot(pf2,main="模式甄别的决策精度和回溯精度曲线")
P<-0.25
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main="EM聚类的模式诊断结果(25%)",xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8,col=colP)


################模拟数据异常点甄别：DB法(无监督)
Data<-read.table(file="模式甄别模拟数据1.txt",header=TRUE,sep=",")
N<-length(Data[,1])
DistM<-as.matrix(dist(Data[,1:2]))
par(mfrow=c(2,2)) 
(D<-quantile(x=DistM[upper.tri(DistM,diag=FALSE)],prob=0.75))  #计算距离的分位数作为阈值D
for(i in 1:N){
 x<-as.vector(DistM[i,])
 Data$DB.scores[i]<-length(which(x>D))/N    #计算观测x与其他观测间的距离大于阈值D的个数占比
}
Data.Sort<-Data[order(x=Data$DB.score,decreasing=TRUE),]
P<-0.1
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main=paste("DB的模式诊断结果:p=",P,sep=""),xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8,col=colP)
library("ROCR")
pd<-prediction(Data$DB.scores,Data$y)
pf1<-performance(pd,measure="rec",x.measure="rpp") #y轴为回溯精度，X轴为预测的模式占总样本的比例
pf2<-performance(pd,measure="prec",x.measure="rec")   #y轴为决策精度，X轴为回溯精度
plot(pf1,main="模式甄别的累计回溯精度曲线")
plot(pf2,main="模式甄别的决策精度和回溯精度曲线")
P<-0.25
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main=paste("DB的模式诊断结果:p=",P,sep=""),xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8,col=colP)


#############模拟数据异常点甄别：LOF法(无监督)
Data<-read.table(file="模式甄别模拟数据1.txt",header=TRUE,sep=",")
library("DMwR")
lof.scores<-lofactor(data=Data[,-3],k=20)
par(mfrow=c(2,2)) 
Data$lof.scores<-lof.scores
Data.Sort<-Data[order(x=Data$lof.scores,decreasing=TRUE),]
P<-0.1
N<-length(Data[,1])
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main="LOF的模式诊断结果",xlab="",ylab="",pch=Data[,3]+1,cex=0.8,col=colP)
library("ROCR")
pd<-prediction(Data$lof.scores,Data$y)
pf1<-performance(pd,measure="rec",x.measure="rpp") #y轴为回溯精度，X轴为预测的模式占总样本的比例
pf2<-performance(pd,measure="prec",x.measure="rec")   #y轴为决策精度，X轴为回溯精度
plot(pf1,main="模式甄别的累计回溯精度曲线")
plot(pf2,main="模式甄别的决策精度和回溯精度曲线")

#################模拟数据异常点甄别：朴素贝叶斯(有监督)
Data<-read.table(file="模式甄别模拟数据2.txt",header=TRUE,sep=",")
library("klaR")
BayesModel<-NaiveBayes(x=Data[,1:2],grouping=factor(Data[,3]))  #输出变量应为因子
BayesModel$apriori   #显示先验概率
BayesModel$tables    #显示各分布的参数估计值
plot(BayesModel)    #可视化各个分布
BayesFit<-predict(object=BayesModel,newdata=Data[,1:2])    #预测
head(BayesFit$class)  #显示预测类别
head(BayesFit$posterior)     #显示后验概率
par(mfrow=c(2,2))
plot(Data[,1:2],main="朴素贝叶斯分类的模式甄别结果",xlab="x1",ylab="x2",
    pch=Data[,3]+1,col=as.integer(as.vector(BayesFit$class))+1,cex=0.8)  #可视化观测点分布特征
library("ROCR")
pd<-prediction(BayesFit$posterior[,2],Data$y)
pf1<-performance(pd,measure="rec",x.measure="rpp") #y轴为回溯精度，X轴为预测的模式占总样本的比例
pf2<-performance(pd,measure="prec",x.measure="rec")   #y轴为决策精度，X轴为回溯精度
plot(pf1,main="模式甄别的累计回溯精度曲线")
plot(pf2,main="模式甄别的决策精度和回溯精度曲线")

#################模拟数据异常点甄别：Logistic回归(有监督)
Data<-read.table(file="模式甄别模拟数据2.txt",header=TRUE,sep=",")
(LogModel<-glm(factor(y)~.,data=Data,family=binomial(link="logit")))  
LogFit<-predict(object=LogModel,newdata=Data,type="response")
Data$Log.scores<-LogFit
library("ROCR")
par(mfrow=c(2,2))
pd<-prediction(Data$Log.scores,Data$y)
pf1<-performance(pd,measure="rec",x.measure="rpp") #y轴为回溯精度，X轴为预测的模式占总样本的比例
pf2<-performance(pd,measure="prec",x.measure="rec")   #y轴为决策精度，X轴为回溯精度
plot(pf1,main="模式甄别的累计回溯精度曲线",print.cutoffs.at=c(0.15,0.1))
plot(pf2,main="模式甄别的决策精度和回溯精度曲线")
Data.Sort<-Data[order(x=Data$Log.scores,decreasing=TRUE),]
P<-0.20
N<-length(Data[,1])
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main="Logistic回归的模式甄别结果(20%)",xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8,col=colP)
P<-0.30
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main="Logistic回归的模式甄别结果(30%)",xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8,col=colP)

#################模拟数据异常点甄别：非平衡样本的平衡化处理
Data<-read.table(file="模式甄别模拟数据2.txt",header=TRUE,sep=",")
library("DMwR")
Data$y<-factor(Data$y)
set.seed(12345)
newData<-SMOTE(y~.,data=Data,k=5,perc.over=1000,perc.under=200)  
plot(newData[,1:2],main="SMOTE处理后的观测点分布",xlab="x1",ylab="x2",pch=as.integer(as.vector(Data[,3]))+1,cex=0.8)
#############SMOTE处理前后的甄别效果对比
LogModel<-glm(y~.,data=newData,family=binomial(link="logit"))  
LogFit<-predict(object=LogModel,newdata=Data,type="response")  
Data$Log.scores<-LogFit
library("ROCR")
par(mfrow=c(2,2))
pd<-prediction(Data$Log.scores,Data$y)
pf1<-performance(pd,measure="rec",x.measure="rpp") 
pf2<-performance(pd,measure="prec",x.measure="rec")   
plot(pf1,main="模式甄别的累计回溯精度曲线")
plot(pf2,main="模式甄别的决策精度和回溯精度曲线")
Data.Sort<-Data[order(x=Data$Log.scores,decreasing=TRUE),]
P<-0.30
N<-length(Data[,1])
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main="SMOTE处理后的模式甄别结果(30%)",xlab="x1",ylab="x2",pch=as.integer(as.vector(Data[,3]))+1,cex=0.8,col=colP)
LogModel<-glm(y~.,data=Data,family=binomial(link="logit"))   
LogFit<-predict(object=LogModel,newdata=Data,type="response")
Data$Log.scores<-LogFit
Data.Sort<-Data[order(x=Data$Log.scores,decreasing=TRUE),]
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
plot(Data[,1:2],main="平衡化处理前的模式甄别结果(30%)",xlab="x1",ylab="x2",pch=as.integer(as.vector(Data[,3]))+1,cex=0.8,col=colP)

###########模拟数据异常点甄别：半监督
Data<-read.table(file="模式甄别模拟数据3.txt",header=TRUE,sep=",")
par(mfrow=c(2,2))
plot(Data[,1:2],main="样本观测点的分布",xlab="x1",ylab="x2",pch=Data[,3]+1,cex=0.8)
library("DMwR")
Data[which(Data[,3]==3),3]<-NA
Data$y<-factor(Data$y)
mySelfT<-function(ModelName,TestD)
{
 Yheat<-predict(object=ModelName,newdata=TestD,type="response") 
 return(data.frame(cl=ifelse(Yheat>=0.1,1,0),pheat=Yheat))
}
SemiT<-SelfTrain(y~.,data=Data,
 learner("glm",list(family=binomial(link="logit"))),
 predFunc="mySelfT",thrConf=0.02,maxIts=100,percFull=1)   
SemiP<-predict(object=SemiT,newdata=Data,type="response")   
Data$SemiP<-SemiP
Data.Sort<-Data[order(x=Data$SemiP,decreasing=TRUE),]
P<-0.30
N<-length(Data[,1])
NoiseP<-head(Data.Sort,trunc(N*P))
colP<-ifelse(1:N %in% rownames(NoiseP),2,1)
a<-as.integer(as.vector(Data[,3]))
plot(Data[,1:2],main="自训练模式甄别结果(30%)",xlab="x1",ylab="x2",pch=ifelse(is.na(a),3,a)+1,cex=0.8,col=colP)


