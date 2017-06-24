install.packages("arules")
install.packages("arulesViz")

library("arules")

###########生成transactoins对象：方式一
MyList<-list(   
  c("A","C","D"),
  c("B","C","E"),
  c("A","B","C","E"),
  c("B","E")
)
names(MyList)<-paste("Tr",c(1:4),sep="")
MyTrans<-as(MyList,"transactions")
summary(MyTrans)
inspect(MyTrans)
image(MyTrans)

###########生成transactoins对象：方式二
MyFact<-matrix(c(
      1,0,1,1,0,
	0,1,1,0,1,
	1,1,1,0,1,
	0,1,0,0,1
  ),nrow=4,ncol=5,byrow=TRUE)
dimnames(MyFact)<-list(paste("Tr",c(1:4), sep = ""),c("A","B","C","D","E"))
MyFact
(MyTrans<-as(MyFact,"transactions"))
(as(MyTrans,"data.frame"))

###########生成transactoins对象：方式三
MyT<-data.frame(
  TID=c(1,1,1,2,2,2,3,3,3,3,4,4), 
  items=c("A","C","D","B","C","E","A","B","C","E","B","E")
  )
(MyList<-split(MyT[,"items"],MyT[,"TID"]))
(MyTrans<-as(MyList,"transactions"))

###########生成transactoins对象：方式四
MyTrans<-read.transactions(file="事务原始数据.txt",format="basket",sep=",")
MyTrans<-read.transactions(file="事务表数据.txt",format="single",cols=c("TID","ITEMS"),sep="	")


##################apriori算法：搜索频繁项集
MyTrans<-read.transactions(file="事务原始数据.txt",format="basket",sep=",")
MyRules<-apriori(data=MyTrans,parameter=list(support=0.5,confidence=0.6,target="frequent itemsets"))
inspect(MyRules)
MyRules<-apriori(data=MyTrans,parameter=list(support=0.5,confidence=0.6,target="maximally frequent itemsets"))
inspect(MyRules)

##################apriori算法：生成关联规则
MyTrans<-read.transactions(file="事务原始数据.txt",format="basket",sep=",")
MyRules<-apriori(data=MyTrans,parameter=list(support=0.5,confidence=0.6,target="rules"))
inspect(MyRules)
size(x=MyRules)
MyRules.sorted<-sort(x=MyRules,by="lift",decreasing=TRUE)
inspect(MyRules.sorted)

##################apriori算法：筛选关联规则
MyRules.D<-subset(x=MyRules,subset=size(MyRules)==2)
inspect(MyRules.D)
MyRules.D<-subset(x=MyRules,subset=slot(object=MyRules,name="quality")$lift>1)
inspect(MyRules.D)
#MyRules.D<-subset(x=MyRules,subset=quality(MyRules)$lift>1)  
#a<-as(MyRules,"data.frame") 
#a[a$lift>1,]

MyRules<-apriori(data=MyTrans,parameter=list(support=0.5,confidence=0.6,target="rules"),
                   appearance=list(lhs=c("B"),default="rhs"))
inspect(MyRules)


#############可视化频繁项集
library("arulesViz")
MyTrans<-read.transactions(file="事务原始数据.txt",format="basket",sep=",")
MyRules<-apriori(data=MyTrans,parameter=list(support=0.5,confidence=0.6,target="frequent itemsets"))
inspect(MyRules)
plot(x=MyRules,method="graph",control=list(main="示例的频繁项集可视化结果"))

#############可视化关联规则
MyTrans<-read.transactions(file="事务原始数据.txt",format="basket",sep=",")
MyRules<-apriori(data=MyTrans,parameter=list(support=0.5,confidence=0.6,target="rules"))
plot(MyRules,method="grouped")
plot(MyRules,method="paracoord")
plot(MyRules,method="graph",control=list(arrowSize=2,main="示例的关联规则可视化结果"))

#############eclat算法
library("arules")
library("arulesViz")
MyTrans<-read.transactions(file="事务原始数据.txt",format="basket",sep=",")
MyFSets<-eclat(data=MyTrans,parameter=list(support=0.5,target="maximally frequent itemsets"))
inspect(MyFSets)
MyFSets<-eclat(data=MyTrans,parameter=list(support=0.5,target="frequent itemsets"))
plot(MyFSets)
MyRules<-ruleInduction(x=MyFSets,transactions=MyTrans,confidence=0.6)
inspect(sort(x=MyRules,by="lift"))

#############简单关联规则应用：发现连带销售商品
library("arules")
library("arulesViz")
Data<-read.table(file="购物篮数据.txt",header=TRUE,sep=",")
Data<-as.matrix(Data[,-1:-7])
MyTrans<-as(Data,"transactions")
summary(MyTrans)
MyRules<-apriori(data=MyTrans,parameter=list(support=0.1,confidence=0.5,target="rules"))
plot(MyRules,method="graph",control=list(arrowSize=2,main="连带销售商品可视化结果"))

#############简单关联规则应用：性别和年龄的啤酒选择性倾向对比
Data<-read.table(file="购物篮数据.txt",header=TRUE,sep=",")
Data<-Data[,c(4,7,14)]
Data$beer<-factor(Data$beer)
Data[,2]<-sapply(Data[,2],FUN=function(x){
  if(x %in% 0:29) x<-1 else
  if(x %in% 30:49) x<-2 else
  if(x %in% 50:59) x<-3})
Data$age<-factor(Data$age)
MyTrans<-as(Data,"transactions")
MyRules<-apriori(data=MyTrans,parameter=list(support=0.01,confidence=0.2,minlen=2,target="rules"),
                   appearance=list(rhs=c("beer=1"),
                              lhs=c("age=1","age=2","age=3","sex=M","sex=F"),
                              default="none"))
inspect(MyRules)
(SuperSetF<-is.subset(MyRules,MyRules))     
inspect(MyRules[-which(colSums(SuperSetF)>1)])   
MyRules<-subset(x=MyRules,subset=quality(MyRules)$lift>1)
plot(MyRules,method="graph",control=list(arrowSize=2,main="性别与年龄的啤酒选择倾向对比"))


#############序列关联规则示例
library("arulesSequences")
MyTrans<-read_baskets(con="事务序列原始数据.txt",sep=",",info=c("sequenceID","eventID"))
MyFsets<-cspade(data=MyTrans,parameter=list(support=0.5))
inspect(MyFsets)
MyRules<-ruleInduction(x=MyFsets,confidence=0.3)  
MyRules.DF<-as(MyRules,"data.frame")  
MyRules.DF[MyRules.DF$lift>=1,]   

#############序列关联规则应用：
MyTrans<-read_baskets(con="网页浏览数据.txt",sep=",",info=c("sequenceID","eventID"))
summary(MyTrans)
MyFsets<-cspade(data=MyTrans,parameter=list(support=0.1))
inspect(MyFsets)
MyRules<-ruleInduction(x=MyFsets,confidence=0.3)  
MyRules.DF<-as(MyRules,"data.frame")  
MyRules.DF[MyRules.DF$lift>=1,]   




