
###############模拟数据的BIRCH聚类
install.packages("birch")
library("birch")
library(MASS)
set.seed(12345) 
Data<-mvrnorm(1000,mu=rep(0,2),Sigma=diag(1,2))
Data<-rbind(Data,mvrnorm(1000,mu=rep(10,2),Sigma=diag(0.1,2)+0.9))
par(mfrow=c(2,2))
plot(Data,main="样本观测点的分布",xlab="x1",ylab="x2")  
Mybirch<-birch(x=Data,radius=5,keeptree=TRUE)   
(OutBirch<-birch.getTree(Mybirch))           
plot(OutBirch,main="BIRCH聚类解",xlab="x1",ylab="x2")           

set.seed(12345) 
NewData<-mvrnorm(10,mu=rep(7,2),Sigma=diag(0.1,2)+0.9)  
plot(Data,main="样本观测点的分布",xlab="x1",ylab="x2") 
points(NewData,col=2)
birch.addToTree(x=NewData,birchObject=OutBirch)   
OutBirch<-birch.getTree(birchObject=OutBirch)   
plot(OutBirch,main="BIRCH聚类解",xlab="x1",ylab="x2")

set.seed(12345)
kOut<-kmeans.birch(OutBirch,center=4,nstart=2)   
plot(OutBirch,col=kOut$clust$sub,main="BIRCH聚类解优化",xlab="x1",ylab="x2")      
plot(Data,col=kOut$clust$obs,main="最终聚类解",xlab="x1",ylab="x2")    
bDist<-dist.birch(OutBirch)     
hc<-hclust(bDist,method="complete")    
plot(hc,main="BIRCH聚类解的距离树形图")
box()
hc<-cutree(hc,k=4)      
plot(kOut$clust$sub,pch=hc,main="K-Means和分层聚类的优化结果对比",ylab="K-Means聚类")   #对比两种优化结果
birch.killTree(birchObject=OutBirch)


################BIRCH聚类应用
library("birch")
TrainData<-read.table(file="员工培训数据.txt",header=TRUE)
CluData<-as.matrix(TrainData[which(TrainData$X6==1),1:5])
NewData<-as.matrix(TrainData[which(TrainData$X6==2),1:5])
par(mfrow=c(2,3))
plot(TrainData[,1:2],col=TrainData[,6])
set.seed(12345)
Mybirch<-birch(x=CluData,radius=0.4,keeptree=TRUE)
(OutBirch<-birch.getTree(Mybirch)) 
dim(OutBirch) 
plot(OutBirch)
plot(OutBirch,centers=TRUE,xlim=c(0,1),ylim=c(0,1))
set.seed(12345)
kOut<-kmeans.birch(OutBirch,center=3)
plot(OutBirch[,1:2],col=kOut$clust$sub)
birch.addToTree(x=NewData,birchObject=OutBirch)  
OutBirch<-birch.getTree(birchObject=OutBirch)
dim(OutBirch)
plot(OutBirch[,1:2],centers=TRUE,xlim=c(0,1),ylim=c(0,1))
birch.killTree(birchObject=OutBirch)

###############模拟数据的SOM聚类
install.packages("kohonen")
library("kohonen")
set.seed(12345)
Data<-matrix(rnorm(n=100,mean=0,sd=1),ncol=2,byrow=TRUE)  
Data[1:25,1]<-Data[1:25,1]+3            
Data[1:25,2]<-Data[1:25,2]-4
set.seed(12345)
My.som<-som(data=Data,grid=somgrid(xdim=1,ydim=2,topo="rectangular"),n.hood="circular")
summary(My.som)           #mean(My.som$distances)
table(My.som$unit.classif)
par(mfrow=c(2,2))
plot(Data,main="模拟数据观测点的分布",xlab="x1",ylab="x2")  
plot(My.som,type="mapping",main="SOM网络输出层示意图",pchs=My.som$unit.classif)
plot(My.som,type="changes",main="SOM网络聚类评价图")
plot(My.som$data,pch=My.som$unit.classif,main="模拟数据SOM网络聚类结果",xlab="x1",ylab="x2")
points(My.som$code,col=2,pch=10)     
My.som$code    

###############邮政编码数据的可视化：16*16的点阵灰度数据
ZipCode<-read.table(file="邮政编码数据.txt",header=FALSE)
ZipCode[,-1]<-(ZipCode[,-1]-min(ZipCode[,-1]))/(max(ZipCode[,-1])-min(ZipCode[,-1]))  #将灰度数据转换到0~1之间
plot(1,1,col=gray(1),pch=20,xlim=c(0,20),ylim=c(0,20),xlab="",ylab="",main="手写邮政编码")
for(q in 1:10){   #字母所在的行
 w<-(q-1)*10     #字母数据在矩阵的行号
 k<-0
 for(w in (w+1):(w+10)){
  k<-k+1         #字母所在列
  alpha<-ZipCode[w,-1]
  a<-matrix(alpha,nrow=16,ncol=16,byrow=FALSE)
  for(i in 1:16){
   r<-i+(q-1)*20  #单个字母点阵的行坐标
   for(j in 1:16){
    c<-16-j+1+(k-1)*20   #单个字母点阵的列坐标
    points(r/10,c/10,col=gray(a[i,j]),pch=20,cex=1.5)
   }
  }
 }
}

###########邮政编码6,7,8的SOM聚类
ZipCode<-read.table(file="邮政编码数据.txt",header=FALSE)
ZipCode<-subset(ZipCode,ZipCode[,1]=="6"|ZipCode[,1]=="7"|ZipCode[,1]=="8")
set.seed(12345)
flag<-sample(x=1:length(ZipCode[,1]),size=round(length(ZipCode[,1])*0.8))
ZipCode_train<-as.matrix(ZipCode[flag,])  
ZipCode_test<-as.matrix(ZipCode[-flag,])    
table(ZipCode_train[,1])    
table(ZipCode_test[,1])
library("kohonen")
set.seed(12345)
My.som<-som(data=ZipCode_train[,-1],grid=somgrid(xdim=3,ydim=1,topo="rectangular"),
  n.hood="circular",rlen=200)
summary(My.som)
head(ZipCode_train[,1])
head(My.som$unit.classif)
par(mfrow=c(2,2))
plot(My.som,type="counts",main="SOM网络聚类样本量分布情况图")
plot(My.som,type="codes",main="SOM网络聚类解的类质心向量图")
plot(My.som,type="changes",main="SOM网络聚类迭代情况图")
plot(My.som,type="quality",main="SOM网络聚类类内差异情况图")
Zip<-cbind(ZipCode_train[,1],My.som$unit.classif)
Zip[,2]<-sapply(Zip[,2],FUN=function(x)switch(x,8,7,6))
(ConfM.SOM<-table(Zip[,1],Zip[,2]))  #识别正确与否的混淆矩阵
(Err.SOM<-(sum(ConfM.SOM)-sum(diag(ConfM.SOM)))/sum(ConfM.SOM))  

mapping<-map(x=My.som,ZipCode_test)   #识别测试样本集的阿拉伯数字
Zip<-cbind(ZipCode_test[,1],mapping$unit.classif)
Zip[,2]<-sapply(Zip[,2],FUN=function(x)switch(x,8,7,6))
(ConfM.SOM<-table(Zip[,1],Zip[,2]))  #识别正确与否的混淆矩阵
(Err.SOM<-(sum(ConfM.SOM)-sum(diag(ConfM.SOM)))/sum(ConfM.SOM))  


##############拓展SOM网络聚类：数据预测
WineData<-read.table(file="红酒品质数据.txt",header=TRUE)
WineData<-WineData[,-1]   
set.seed(12345)
flag<-sample(x=1:length(WineData[,1]),size=round(length(WineData[,1])*0.7))
WineData_train<-WineData[flag,]   
WineData_test<-WineData[-flag,]    
library("kohonen")
set.seed(12345)
Pre.som<-xyf(data=scale(WineData_train[,-12]),Y=classvec2classmat(WineData_train$quality),
  contin=FALSE, xweight=0.5,grid=somgrid(3,3,"rectangular"),rlen=200)
summary(Pre.som)
par(mfrow=c(2,3))
plot(Pre.som,type="changes",main="红酒拓展SOM网络聚类评价图")
plot(Pre.som,type="quality",main="类内平均距离")
plot(Pre.som,type="code")
plot(Pre.som,type="counts",main="样本分布(训练集)")
quality.pre<-predict(object=Pre.som,newdata=scale(WineData_test[-12]))  #对测试样本集预测
plot(Pre.som,type="property",property=table(quality.pre$unit.classif),main="样本分布(测试集)")
(ConfM.SOM<-table(WineData_test$quality,quality.pre$prediction))
round(prop.table(ConfM.SOM,margin=1),2)
(Err.SOM<-(sum(ConfM.SOM)-sum(diag(ConfM.SOM)))/sum(ConfM.SOM))  

################ DBSCAN聚类示例
Data<-read.table(file="模式识别数据.txt",sep=",",head=TRUE)
library("fpc")
par(mfrow=c(2,3))
plot(Data,cex=0.5,main="观测点的分布图")
(DBS1<-dbscan(data=Data,eps=0.2,MinPts=200,scale = FALSE)) 
plot(DBS1,Data,cex=0.5,main="DBSCAN聚类(eps=0.2,MinPts=200)")
(DBS2<-dbscan(data=Data,eps=0.5,MinPts=80,scale = FALSE)) 
plot(DBS2,Data,cex=0.5,main="DBSCAN聚类(eps=0.5,MinPts=80)")
(DBS3<-dbscan(data=Data,eps=0.2,MinPts=100,scale = FALSE))
plot(DBS3,Data,cex=0.5,main="DBSCAN聚类(eps=0.2,MinPts=100)")
(DBS4<-dbscan(data=Data,eps=0.5,MinPts=300,scale = FALSE))
plot(DBS4,Data,cex=0.5,main="DBSCAN聚类(eps=0.5,MinPts=300)")
(DBS5<-dbscan(data=Data,eps=0.2,MinPts=30,scale = FALSE))
plot(DBS5,Data,cex=0.5,main="DBSCAN聚类(eps=0.2,MinPts=30)")





