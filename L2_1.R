######################创建包含一个元素的向量
V1<-59         
V1             
V2<-53.5       
V2
V3<-"abcD"     
print(V3)      
(V4<-TRUE)     

is.vector(V1)
is.logical(V4)
rm(V1,V2,V3,V4)

########################创建包含多个元素的向量
holderage<-c(22,22,23,23)
length(holderage)
vehiclegroup<-rep("A",each=4)
vehicleage<-seq(from=1,to=4,by=1)
claimamt<-c(2312,2256,1064,1280)
nclaims<-c(8,8,4,1)
str(vehiclegroup)
str(vehicleage)
ls()

####################从键盘输入数据到向量
a<-scan()
a
rm(a)

#########################访问指定位置上的元素
a<-vector(length=10)
a
a[1]<-1
a[2:4]<-c(2,3,4)
a
b<-seq(from=5,to=9,by=1)
a[c(5:9,10)]<-c(b,10)
a

#########################利用位置向量访问指定位置上的元素
b<-(2:4)
a[b]
b<-c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
a[b]

#########################访问指定位置之外的元素
a[-1]
a[-(2:4)]
a[-c(5:9,10)]
b<-(2:4)
a[-b]

######################管理对象
ls()
rm(a,b)

########################创建矩阵
ClaimData<-cbind(holderage,vehicleage,claimamt,nclaims)
dim(ClaimData)
ClaimData
str(ClaimData)
colnames(ClaimData)
colnames(ClaimData[,2:4])
rownames(ClaimData)<-c("1","2","3","4")
rownames(ClaimData[c(1,3),])
is.matrix(ClaimData)

a<-(1:9)
b<-(1:3)
c<-(1:2)
cbind(a,b)
cbind(a,b,c)
rbind(a,b)
rm(a,b,c)

a<-(1:30)
dim1<-c("R1","R2","R3","R4","R5")
dim2<-c("C1","C2","C3","C4","C5","C6")
a<-matrix(a,nrow=5,ncol=6,byrow=FALSE,dimnames=list(dim1,dim2))
a
rm(a,dim1,dim2)

#############访问矩阵中的元素
ClaimData
ClaimData[2,3]
ClaimData[1:2,1:3]
a<-(1:2)
ClaimData[a,c(1,3)]
ClaimData[2,]
ClaimData[c(1,3),]
a<-c(TRUE,FALSE,TRUE)
ClaimData[a,]
ClaimData[,1:3]
rm(a)

a<-matrix(nrow=5,ncol=2)
a
a[,1]=seq(from=1,to=10,by=2)
a[,2]=seq(from=10,to=1,by=-2)
a

fix(ClaimData)


##############创建数据框
ClaimDataFrm<-data.frame(Fholderage=holderage,Fvehiclegroup=vehiclegroup,Fvehicleage=vehicleage,Fclaimamt=claimamt,Fnclaims=nclaims)
ClaimDataFrm
names(ClaimDataFrm)
str(ClaimDataFrm)
is.data.frame(ClaimDataFrm)


##############创建一个空的数据框
a<-data.frame(x1=numeric(0),x2=character(0),x3=logical(0))
str(a)
fix(a)

#############访问数据框
ClaimDataFrm
ClaimDataFrm$Fvehicleage
ClaimDataFrm[["Fvehicleage"]]
ClaimDataFrm[[3]]
attach(ClaimDataFrm)
Fvehicleage
detach(ClaimDataFrm)
Fvehicleage

with(ClaimDataFrm,{
print(Fvehicleage)
SumClaim<-Fclaimamt*Fnclaims
print(SumClaim)
})
SumClaim

ClaimDataFrm<-within(ClaimDataFrm,{
SumClaim<-Fclaimamt*Fnclaims
})
ClaimDataFrm

#############创建和访问数组
a<-(1:60)
dim1<-c("R1","R2","R3","R4")
dim2<-c("C1","C2","C3","C4","C5")
dim3<-c("T1","T2","T3")
a<-array(a,c(4,5,3),dimnames=list(dim1,dim2,dim3))
is.array(a)
a
a[1:3,c(1,3),]
rm(a,dim1,dim2,dim3)

######################################创建列表
a<-c(1,2,3)
b<-matrix(nrow=5,ncol=2)
b[,1]=seq(from=1,to=10,by=2)
b[,2]=seq(from=10,to=1,by=-2)
c<-array(1:60,c(4,5,3))
d<-list(L1=a,L2=b,L3=c) 
names(d)  
str(d)
is.list(d) 
d$L1 
d[["L2"]]
d[[2]]
rm(a,b,c,d)

##############数据对象存储类型的转换
a<-123.4
is.numeric(a)
is.integer(a)
is.double(a)
is.character(a)
is.logical(a)
b<-"123.4"
typeof(b)
c<-TRUE
typeof(c)
rm(a,b,c)

a<-123.4
(a<-as.integer(a))
typeof(a)
a<-123.4
(a<-as.character(a))
typeof(a)
a<-"abcd"
(a<-as.double(a))
a<-TRUE
(a<-as.integer(a))
typeof(a)
a<-TRUE
(a<-as.character(a))
typeof(a)
rm(a)

######################################对象结构类型的转换
(a<-c(1:10))
(b<-matrix(a,nrow=5,ncol=2,byrow=TRUE))
(a<-as.matrix(a))
is.matrix(a)
(b<-as.vector(b))
is.vector(b)
rm(a,b)

(a<-c("Poor","Improved","Excellent","Poor")) 
(b<-as.factor(a))
is.factor(b)
levels(b)
nlevels(b)  
typeof(b)

(a<-c("Poor","Improved","Excellent","Poor")) 
(b<-factor(a,order=FALSE,levels=c("Poor","Improved","Excellent")))  
(b<-factor(a,order=TRUE,levels=c("Poor","Improved","Excellent")))

#######利用factor函数重新设置类别值  
(a<-c("Poor","Improved","Excellent","Poor")) 
(b<-factor(a,levels=c("Poor","Improved","Excellent")))  
(b<-factor(a,levels=c("Poor","Improved","Excellent"),labels=c("C","B","A")))  
rm(a,b)

#############创建因子
gl(5,4)

###############借助类型转换增加因子的水平
(a<-c("A","C","B","C")) 
(b<-as.factor(a))
b[5]<-"D"
c<-as.vector(b)
typeof(c)
c[5]<-"D"
(b<-as.factor(c))


############################################读取文本数据
#形成向量
ClaimData<-scan(file="车险数据.txt",what=double(),skip=1)   #无法成功执行

#形成数据框
ClaimData<-read.table(file="车险数据.txt",header=TRUE,sep="	")
str(ClaimData)
names(ClaimData)
head(ClaimData)

ClaimData<-read.table(file="车险数据.txt",header=TRUE,sep="	",stringsAsFactors=FALSE)
str(ClaimData)
ClaimData<-read.table(file="车险数据.txt",header=TRUE,sep="	",
   colClass=c("integer","character","character","integer","integer"))
str(ClaimData)

########################################读取SPSS数据
library(foreign)
ClaimData<-read.spss(file="车险数据.sav",use.value.labels = TRUE, to.data.frame = TRUE)
str(ClaimData)


#######################读取数据库数据
install.packages("RODBC")
library("RODBC")
MyConn<-odbcConnectAccess2007("车险数据.accdb",uid="",pwd="")
ClaimData<-sqlFetch(MyConn,"车险数据")
close(MyConn)
str(ClaimData)

##########读取Excel数据
MyConn<-odbcConnectExcel2007("车险数据.xlsx")
ClaimData<-sqlFetch(MyConn,"Sheet1")
close(MyConn)
str(ClaimData)

################抓取网页数据
install.packages("XML")
library("XML")
url<-"http://data.mofcom.gov.cn/channel/gbsj/gbsj.shtml"
TableData<-readHTMLTable(url,trim=TRUE)
str(TableData)
head(TableData[[1]])


#####################R自带数据集的使用
data()
data("AirPassengers")

data(package=.packages(all.available=TRUE))


###############保存数据到数据文件中
write.table(ClaimData,file="数据.txt",sep=" ",quote=FALSE,append=FALSE,na="NA",row.names=FALSE,col.names=TRUE)


##############定义用户自定义函数
MyFun<-function(dataname1=x1,dataname2=x2,key=c){
 result<-merge(dataname1,dataname2,by=key)
 return(result)
}
#####调用用户自定义函数
ReportCard1<-read.table(file="ReportCard1.txt",header=TRUE)
ReportCard2<-read.table(file="ReportCard2.txt",header=TRUE)
MyData<-MyFun(dataname1=ReportCard1,dataname2=ReportCard2,key="xh")
MyData<-MyFun(ReportCard1,ReportCard2,"xh")

#####################################分支结构控制
Price<-scan()
if(Price<200) print("No discount!")
if(Price>=200 & Price<500)  print("off 3%")
if(Price>500 & Price<1000) print("off 5%")
if(Price>=1000 & Price<2500) print("off 8%")
if(Price>=2500 & Price<5000) print("off 10%")
if(Price>=5000) print("off 15%")


###if else 实现
if(Price<200) print("No discount!") else{
 if(Price>=200 & Price<500) print("off 3%") else{
  if(Price>=500 & Price<1000) print("off 5%") else{
   if(Price>=1000 & Price<2500) print("off 8%") else{
    if(Price>=2500 & Price<5000) print("off 10%") else
     print("off 15%")
   }
  }
 }
}

#######switch实现
if(Price<200) F<-1 else {
  if(Price>=200 & Price<500) F<-2 else {
  if(Price>500 & Price<1000)  F<-3 else {
   if(Price>=1000 & Price<2500)  F<-4 else{
    if(Price>=2500 & Price<5000) F<-5 else
     F<-6
   }
  }
 }
}
print(switch(F,"No discount","off 3%","off 5%","off 8%","off 10%","off 15%"))

##########switch
x=c(1,2,3,4,5)
Type<-"mean"
switch(Type,center=,mean=mean(x),sd=sd(x))
Type<-"center"
switch(Type,center=,mean=mean(x),sd=sd(x))  ##center和mean均计算mean(x)
Type<-"sd"
switch(Type,center=,mean=mean(x),sd=sd(x))

#########################################
###for循环
s=0
for(i in 1:100){
 s=s+i
}
print(s)

#########while循环
s<-0
i<-1
while(i<=100){
 s<-s+i 
 i<-i+1
}
print(s)

#####repeat循环
s<-0
i<-1
repeat{
  if(i<=100){ 
   s<-s+i
   i<-i+1} else
   break
}
print(s)

########################################综合应用一

#####计算基本描述统计量的用户自定义函数
Des.Fun<-function(x,...){
 Av<-mean(x,na.rm=TRUE)   
 Sd<-sd(x,na.rm=TRUE)  
 N<-length(x[!is.na(x)])      
 Sk<-sum((x[!is.na(x)]-Av)^3/Sd^3)/N  
 Ku<-sum((x[!is.na(x)]-Av)^4/Sd^4)/N-3    
 result<-list(avg=Av,sd=Sd,skew=Sk,kurt=Ku)      
 return(result)   
}
ClaimData<-read.table(file="车险数据.txt",header=TRUE,sep="	")
with(ClaimData,{
 Des1<-sapply(ClaimData[,c(1,5)],FUN=Des.Fun)  
 print(Des1)
 Des2<-tapply(nclaims,INDEX=vehiclegroup,FUN=mean,na.rm=TRUE)   
 print(Des2)
 cor(nclaims,holderage,use="complete.obs")        
})

#########计算各车型的平均理赔金额
for(i in unique(ClaimData$vehiclegroup)){  
   temp<-subset(ClaimData,vehiclegroup==i)   
   Des3<-sum((temp$claimamt*temp$nclaims),na.rm=TRUE)/sum(temp$nclaims,na.rm=TRUE) 
   print(paste(i,round(Des3,2),sep=":"))
 }

###########计算车型和车龄的交叉列联表
 CrossTable<-table(ClaimData$vehiclegroup,ClaimData$vehicleage)
 addmargins(CrossTable)    
 addmargins(prop.table(CrossTable,1)*100,2)  
 addmargins(prop.table(CrossTable,2)*100,1)  
 addmargins(prop.table(CrossTable)*100)   


######################################综合应用二：将汇总表转化为原始表数据
MyTable<-function(mytable){  
 rows<-dim(mytable)[1]
 cols<-dim(mytable)[2]
 DataTable<-NULL
 for(i in 1:rows){
  for(j in 1:mytable$freq[i]){
   RowData<-mytable[i,c(1:(cols-1))]
   DataTable<-rbind(DataTable,RowData)
  }
 }
 row.names(DataTable)<-c(1:dim(DataTable)[1])
 return(DataTable)
}
Grade<-rep(c("B","C","D","E"),times=2)  
Sex<-rep(c("M","F"),each=4)           
Freq<-c(2,11,12,5,2,13,10,3)   
Table<-data.frame(sex=Sex,grade=Grade,freq=Freq)
MyData<-MyTable(Table)   
head(MyData)
 


