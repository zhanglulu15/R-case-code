##############对模拟数据的K-Means聚类
set.seed(12345)
x<-matrix(rnorm(n=100,mean=0,sd=1),ncol=2,byrow=TRUE)  
x[1:25,1]<-x[1:25,1]+3           
x[1:25,2]<-x[1:25,2]-4
par(mfrow=c(2,2))
plot(x,main="样本观测点的分布",xlab="",ylab="")  
set.seed(12345)
(KMClu1<-kmeans(x=x,centers=2,nstart=1))  
plot(x,col=(KMClu1$cluster+1),main="K-Means聚类K=2",xlab="",ylab="",pch=20,cex=1.5)
points(KMClu1$centers,pch=3)
set.seed(12345)
KMClu2<-kmeans(x=x,centers=4,nstart=1)   
plot(x,col=(KMClu2$cluster+1),main="K-Means聚类K=4,nstart=1",xlab="",ylab="",pch=20,cex=1.5)
points(KMClu2$centers,pch=3)
KMClu1$betweenss/(2-1)/KMClu1$tot.withinss/(50-2)
KMClu2$betweenss/(4-1)/KMClu2$tot.withinss/(50-4)
set.seed(12345)
KMClu2<-kmeans(x=x,centers=4,nstart=30)
plot(x,col=(KMClu2$cluster+1),main="K-Means聚类K=4,nstart=30",xlab="",ylab="",pch=20,cex=1.5)
points(KMClu2$centers,pch=3)

#####################K-Means聚类应用
PoData<-read.table(file="环境污染数据.txt",header=TRUE)
CluData<-PoData[,2:7]
#############K-Means聚类
set.seed(12345)
CluR<-kmeans(x=CluData,centers=4,iter.max=10,nstart=30)
CluR$size
CluR$centers

###########K-Means聚类结果的可视化 
par(mfrow=c(2,1))
PoData$CluR<-CluR$cluster
plot(PoData$CluR,pch=PoData$CluR,ylab="类别编号",xlab="省市",main="聚类的类成员",axes=FALSE)
par(las=2)
axis(1,at=1:31,labels=PoData$province,cex.axis=0.6)
axis(2,at=1:4,labels=1:4,cex.axis=0.6)
box()
legend("topright",c("第一类","第二类","第三类","第四类"),pch=1:4,cex=0.6)
###########K-Means聚类特征的可视化
plot(CluR$centers[1,],type="l",ylim=c(0,82),xlab="聚类变量",ylab="组均值(类质心)",main="各类聚类变量均值的变化折线图",axes=FALSE)
axis(1,at=1:6,labels=c("生活污水排放量","生活二氧化硫排放量","生活烟尘排放量","工业固体废物排放量","工业废气排放总量","工业废水排放量"),cex.axis=0.6)
box()
lines(1:6,CluR$centers[2,],lty=2,col=2)
lines(1:6,CluR$centers[3,],lty=3,col=3)
lines(1:6,CluR$centers[4,],lty=4,col=4)
legend("topleft",c("第一类","第二类","第三类","第四类"),lty=1:4,col=1:4,cex=0.6)

###########K-Means聚类效果的可视化评价
CluR$betweenss/CluR$totss*100
par(mfrow=c(2,3))
plot(PoData[,c(2,3)],col=PoData$CluR,main="生活污染情况",xlab="生活污水排放量",ylab="生活二氧化硫排放量")
points(CluR$centers[,c(1,2)],col=rownames(CluR$centers),pch=8,cex=2)
plot(PoData[,c(2,4)],col=PoData$CluR,main="生活污染情况",xlab="生活污水排放量",ylab="生活烟尘排放量")
points(CluR$centers[,c(1,3)],col=rownames(CluR$centers),pch=8,cex=2)
plot(PoData[,c(3,4)],col=PoData$CluR,main="生活污染情况",xlab="生活二氧化硫排放量",ylab="生活烟尘排放量")
points(CluR$centers[,c(2,3)],col=rownames(CluR$centers),pch=8,cex=2)

plot(PoData[,c(5,6)],col=PoData$CluR,main="工业污染情况",xlab="工业固体废物排放量",ylab="工业废气排放总量")
points(CluR$centers[,c(4,5)],col=rownames(CluR$centers),pch=8,cex=2)
plot(PoData[,c(5,7)],col=PoData$CluR,main="工业污染情况",xlab="工业固体废物排放量",ylab="工业废水排放量")
points(CluR$centers[,c(4,6)],col=rownames(CluR$centers),pch=8,cex=2)
plot(PoData[,c(6,7)],col=PoData$CluR,main="工业污染情况",xlab="工业废气排放总量",ylab="工业废水排放量")
points(CluR$centers[,c(5,6)],col=rownames(CluR$centers),pch=8,cex=2)

#################PAM聚类
set.seed(12345)
x<-matrix(rnorm(n=100,mean=0,sd=1),ncol=2,byrow=TRUE) 
x[1:25,1]<-x[1:25,1]+3            
x[1:25,2]<-x[1:25,2]-4
library("cluster")
set.seed(12345)
(PClu<-pam(x=x,k=2,do.swap=TRUE,stand=FALSE)) 
plot(x=PClu,data=x)

################层次聚类
PoData<-read.table(file="环境污染数据.txt",header=TRUE)
CluData<-PoData[,2:7]
DisMatrix<-dist(CluData,method="euclidean")
CluR<-hclust(d=DisMatrix,method="ward")

###############层次聚类的树形图
plot(CluR,labels=PoData[,1])
box()
###########层次聚类的碎石图
plot(CluR$height,30:1,type="b",cex=0.7,xlab="距离测度",ylab="聚类数目")

######取4类的聚类解并可视化
par(mfrow=c(2,1))
PoData$memb<-cutree(CluR,k=4)
table(PoData$memb)
plot(PoData$memb,pch=PoData$memb,ylab="类别编号",xlab="省市",main="聚类的类成员",axes=FALSE)
par(las=2)
axis(1,at=1:31,labels=PoData$province,cex.axis=0.6)
axis(2,at=1:4,labels=1:4,cex.axis=0.6)
box()

##############混合高斯分布模拟
library("MASS")  
set.seed(12345)
mux1<-0    
muy1<-0    
mux2<-15    
muy2<-15    
ss1<-10   
ss2<-10    
s12<-3   
sigma<-matrix(c(ss1,s12,s12,ss2),nrow=2,ncol=2)  
Data1<-mvrnorm(n=100,mu=c(mux1,muy1),Sigma=sigma,empirical=TRUE)  
Data2<-mvrnorm(n=50,mu=c(mux2,muy2),Sigma=sigma,empirical=TRUE) 
Data<-rbind(Data1,Data2)
plot(Data,xlab="x",ylab="y")
library("mclust")
DataDens<-densityMclust(data=Data)      
plot(x=DataDens,type="persp",col=grey(level=0.8),xlab="x",ylab="y") 

#########################对模拟数据的EM聚类
library("mclust") 
EMfit<-Mclust(data=Data)  
summary(EMfit)
summary(EMfit,parameters=TRUE)   
plot(EMfit,"classification") 
plot(EMfit,"uncertainty")
plot(EMfit,"density")

#############或者
(BIC<-mclustBIC(data=Data))
plot(BIC,G=1:7,col="black")
(BICsum<-summary(BIC,data=Data))
mclust2Dplot(Data,classification=BICsum$classification,parameters=BICsum$parameters)

###################实例数据的EM聚类
PoData<-read.table(file="环境污染数据.txt",header=TRUE)
CluData<-PoData[,2:7]
library("mclust") 
EMfit<-Mclust(data=CluData)  
summary(EMfit)
plot(EMfit,"BIC")
plot(EMfit,"classification")




