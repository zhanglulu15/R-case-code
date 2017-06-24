
library(igraph)

########生成无向网络
par(mfrow=c(2,2))
set.seed(12345)
G1<-graph.formula(A--B,B--C)
G1<-add.edges(G1,c(2,2))   
V(G1)$label.cex<-0.7     
E(G1)$label<-c("e1","e2","e3")   
E(G1)$curved<-TRUE   
vcount(graph=G1)   
ecount(graph=G1)   
plot(G1,main="G1：无向网络(多重图)",layout=layout.fruchterman.reingold(G1))
G1<-simplify(graph=G1) 


G2<-graph.empty(n=4,directed=FALSE)
V(G2)$name<-V(G2)$label<-LETTERS[1:4]  
G2<-add.edges(G2, c(1,4,1,2,2,4,2,3))
E(G2)$label<-c("e1","e2","e3","e4")
E(G2)$curved<-TRUE
V(G2)$label.cex<-0.7
plot(G2,main="G2：无向连通网络",layout=layout.fruchterman.reingold(G2))

G3<-graph.empty(n=9,directed=FALSE)
V(G3)$name<-V(G3)$label<-LETTERS[1:9]
G3<-add.edges(G3, c(1,2,1,3,2,3,4,5,4,6,4,7,5,6,5,7,8,9))
E(G3)$label<-c("e1","e2","e3","e4","e5","e6","e7","e8","e9")
G3<-set.vertex.attribute(G3,name="discrete", 
   value=c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE))  
V(G3)[discrete]$shape<-"circle"
V(G3)[!discrete]$shape<-"square"
G3<-set.vertex.attribute(G3,name="color",value=c("red","green","yellow"))
E(G3)$curved<-TRUE
V(G3)$label.cex<-0.7
is.connected(graph=G3)  
sapply(V(G3),FUN=subcomponent,graph=G3)   
plot(G3,main="G3：无向不连通网络",layout=layout.fruchterman.reingold(G3))

G4<-k.regular.game(no.of.nodes=10,k=9,directed=FALSE,multiple=FALSE) 
#G4<-graph.ring(n=10)
V(G4)$name<-V(G4)$label<-LETTERS[1:10]
V(G4)$label.cex<-0.7
plot(G4,main="G4：完备图",layout=layout.fruchterman.reingold(G4))

########生成有向网络
library(igraph)
par(mfrow=c(2,2))
set.seed(12345)
G5<-graph.formula(A+-B,B-+C)
V(G5)$label.cex<-0.7
E(G5)$label<-c("e1","e2")
E(G5)$curved<-TRUE
plot(G5,main="G5：简单有向网络",layout=layout.fruchterman.reingold(G5))

G6<-graph.empty(n=4,directed=TRUE)
V(G6)$name<-V(G6)$label<-LETTERS[1:4]
G6<-add.edges(G6, c(1,4,2,1,2,3,3,2,3,3))
E(G6)$label<-c("e1","e2","e3","e4","e5")
E(G6)$curved<-TRUE
V(G6)$label.cex<-0.7
is.mutual(graph=G6)  
plot(G6,main="G6：有向网络（有环存在互惠关系）",layout=layout.fruchterman.reingold(G6))

G7<-graph.empty(n=5,directed=TRUE)
V(G7)$name<-V(G7)$label<-LETTERS[1:5]
G7<-add.edges(G7, c(1,2,1,3,1,5,2,3,3,4,5,4))
E(G7)$label<-c("e1","e2","e3","e4","e5","e6")
E(G7)$curved<-TRUE
V(G7)$label.cex<-0.7
is.dag(graph=G7)   
plot(G7,main="G7：有向不循环网络（弱连通）",layout=layout.fruchterman.reingold(G7))

G8<-G7
E(G8)$weight<-c(0.5,0.5,0.8,0.1,0.7,0.2)   
E(G8)$width<-1
E(G8)[weight>0.5]$width<-3
is.weighted(graph=G8)   
E(G8)$label<-c(0.5,0.5,0.8,0.1,0.7,0.2)
plot(G8,main="G8：有向加权网络",layout=layout.circle,edge.width=E(G8)$width)

################生成2-模网络
library(igraph)
par(mfrow=c(2,2))
set.seed(12345)
G9<-graph.full.bipartite(2,4,directed=TRUE,mode="out")   
V(G9)[!V(G9)$type]$name<-V(G9)[!V(G9)$type]$label<-LETTERS[1:2]  
V(G9)[V(G9)$type]$name<-V(G9)[V(G9)$type]$label<-letters[1:4]
plot(G9,main="G9：完备的有向2-模网络",layout=layout.circle)

G10<-graph.bipartite(types=c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE),
   edges=c(1,4,1,5,1,6,2,5,3,6,3,7,3,8),directed=FALSE)  
V(G10)[!V(G10)$type]$name<-V(G10)[!V(G10)$type]$label<-LETTERS[1:3] 
V(G10)[V(G10)$type]$name<-V(G10)[V(G10)$type]$label<-letters[1:5]
plot(G10,main="G10：无向2-模网络",layout=layout.circle)

####################网络的矩阵表示：邻接矩阵、关系矩阵、连接列表
adj.G2<-as.matrix(get.adjacency(graph=G2))  
adj.G7<-as.matrix(get.adjacency(graph=G7))   
adj.G8<-as.matrix(get.adjacency(graph=G8,attr="weight"))   

(get.incidence(graph=G10)) #G10(无向2-模)的关系矩阵
(get.incidence(graph=G9)) #G9(有向2-模)的关系矩阵

(G7.data.frame<-get.data.frame(x=G7,what="edges"))  

################网络的数据文件:依据邻接矩阵建立网络
library(igraph)
Data0<-read.table(file="购物篮数据.txt",header=TRUE,sep=",")
Data<-as.matrix(Data0[,-1:-7])
Data[1:5,1:10]  #浏览部分数据
Data.adj<- t(Data) %*% Data 
dim(Data.adj)   
colnames(Data.adj)<-colnames(Data)  
Data.adj[1:5,1:11]   
diag(Data.adj)<-0 
Basket.G<-graph.adjacency(adjmatrix=Data.adj,weight=TRUE,mode="undirected")  
set.seed(12345)
plot(Basket.G,main="商品的交叉购买网络",edge.width=E(Basket.G)$weight/1000*10,
  layout=layout.fruchterman.reingold(Basket.G))

###############网络的数据文件:依据关系矩阵建立2-模网络
library(igraph)
Data0<-read.table(file="购物篮数据.txt",header=TRUE,sep=",")
Data<-as.matrix(Data0[,-1:-7])
(Basket.2M<-graph.incidence(incidence=Data,directed=TRUE,mode="out")) 
set.seed(12345)
Rid<-igraph.sample(low=1,high=1000,length=20)  
Basket.2M.Sub<-graph.incidence(Basket.2M[Rid,1001:1011],directed=TRUE,mode="out") 
V(Basket.2M.Sub)[V(Basket.2M.Sub)$type]$label<-colnames(Data)
V(Basket.2M.Sub)[!V(Basket.2M.Sub)$type]$label<-as.vector(Data0[Rid,4])
V(Basket.2M.Sub)$size<-10
V(Basket.2M.Sub)[V(Basket.2M.Sub)$type]$size<-20
V(Basket.2M.Sub)$label.cex<-0.7
plot(Basket.2M.Sub,main="顾客与商品的2-模随机子网(性别喜好倾向)",
  layout=layout.fruchterman.reingold(Basket.2M.Sub),
  vertex.color=ifelse(V(Basket.2M.Sub)$label=="M","red",
  ifelse(V(Basket.2M.Sub)$label=="F","green","yellow")))

###########网络的数据文件:依据连接列表建立网络
library(igraph)
Data<-read.table(file="空手道俱乐部数据.txt",header=TRUE,sep=",")
head(Data)  
(Karate.G<-graph.data.frame(d=Data,directed=FALSE)) 
set.seed(12345)
plot(Karate.G,main="空手道俱乐部成员好友关系网",layout=layout.fruchterman.reingold(Karate.G))

tkplot(Karate.G)  

############网络节点个体重要性的测度：度，最短路径，直径
degree(graph=G2,v=V(G2),mode="all")  
degree(graph=G2,v=V(G2)[4],mode="all") 
degree(graph=G7,mode="in")    
adj.G8<-as.matrix(get.adjacency(G8,attr="weight"))
rowSums(adj.G8) 
graph.strength (graph=G8,mode="out")   

shortest.paths(graph=G2,v=V(G2),to=V(G2)$name=="A")  
diameter(graph=G2)  
shortest.paths(graph=G7,v=V(G7)[2],mode="out")  
diameter(graph=G7,directed=TRUE,unconnected=TRUE) 

###########网络节点个体重要性的测度：点度中心度和接近中心度
degree(graph=G2,normalized=TRUE)   
closeness(graph=G2,vids=V(G2),normalized=TRUE)  
degree(graph=G7,mode="all",normalized=TRUE)  
closeness(graph=G7,mode="all",normalized=TRUE)  
degree(graph=G7,mode="out",normalized=TRUE)  
closeness(graph=G8,mode="out",normalized=TRUE)  

##############网络节点个体重要性的测度：计算点度中心度和接近中心度的必要性
G10<-graph.empty(n=16,directed=FALSE)
G10<-add.edges(G10,c(1,2,1,3,1,4,1,5,1,6,6,7,7,8,7,9,7,10,7,11,11,12,12,13,12,14,12,15,12,16))
set.seed(12345)
plot(G10,main="G10网络",layout=layout.fruchterman.reingold(G10))
degree(graph=G10,normalized=TRUE)
closeness(graph=G10,normalized=TRUE)

##############网络节点个体重要性的测度：计算中间中心度（节点、连接）
betweenness(graph=G10,normalized=TRUE)   
edge.betweenness(graph=G10)  

#############网络节点个体重要性的测度：关节点和PageRank
articulation.points(graph=G10)   

adj.G10<-as.matrix(get.adjacency(G10))   
eigen(adj.G10)  
ev.G10<-evcent(graph=G10,scale=FALSE)  
ev.G10$vector

PR.G10<-page.rank(graph=G10,damping=0.85) 
order(PR.G10$vector,decreasing=TRUE)   

#########################子群分析:二元三元普查量
dyad.census(graph=G7)  
triad.census(graph=G7)  

#################子群分析:派系和k-核
G11<-graph.empty(n=12,directed=FALSE)
G11<-add.edges(G11,c(1,2,1,4,1,9,2,3,2,4,2,9,3,4,3,5,5,6,5,7,5,8,6,7,6,10,6,11,6,12,7,8,9,10,10,11,10,12,11,12))
par(mfrow=c(2,2))
set.seed(12345)
plot(G11,main="G11网络",layout=layout.fruchterman.reingold(G11),vertex.size=30)
maximal.cliques(graph=G11,min=3,max=4)    
largest.cliques(graph=G11) 
clique.number(graph=G11)   
set.seed(12345)
plot(G11,main="G11网络中的派系",layout=layout.fruchterman.reingold(G11),vertex.size=30,
  vertex.color=c(0,0,5,0,0,0,0,2,6,3,3,3))
graph.coreness(graph=G11)  
set.seed(12345)
plot(G11,main="G11网络中的k-核",layout=layout.fruchterman.reingold(G11),vertex.size=30,
  vertex.color=graph.coreness(graph=G11))

####发现社区的算法以及模块度计算
(com.G11<-edge.betweenness.community(graph=G11))  
length(x=com.G11)  
sizes(communities=com.G11)   
membership(communities=com.G11) 
modularity(x=G11,membership(com.G11))   
set.seed(12345)
plot(G11,main="G11网络中的社区",layout=layout.fruchterman.reingold(G11),vertex.size=30,
vertex.color=com.G11$membership+1)  
dendPlot(com.G11)  

leading.eigenvector.community(graph=G11) 
fastgreedy.community(graph=G11)  
spinglass.community(graph=G11)  
walktrap.community(graph=G11) 

#################子群分析:组件
clusters(G3)   
com.G3<-decompose.graph(G3)   
layout(matrix(1:3,nrow=1,ncol=3,byrow=TRUE))
sapply(com.G3,FUN=plot)  


################网络的全局分析：各种测度量
graph.density(graph=G11)  
average.path.length(graph=G11,directed=FALSE)  
(CL.n<-transitivity(graph=G11,type="local"))  
mean(CL.n,rm.na=TRUE)   
transitivity(graph=G11,type="global") 
ev.G11<-evcent(graph=G11,scale=FALSE)  
ev.G11$value
mean(degree(graph=G11),na.rm=TRUE)  

#############网络的全局分析：各种分布
d.G11<-degree(graph=G11)  
c.G11<-closeness(graph=G11,normalized=TRUE)   
b.G11<-betweenness(graph=G11,normalized=TRUE)  
sp.G11<-shortest.paths(graph=G11,v=V(G11),to=V(G11))   
sp.G11<-sp.G11[lower.tri(sp.G11)]
par(mfrow=c(2,2))
plot(table(d.G11),xlab="节点度",ylab="频数",main="节点的度分布")
plot(table(c.G11),xlab="接近中心度",ylab="频数",main="节点的接近中心度分布")
hist(b.G11,xlab="中间中心度",ylab="频数",main="节点的中间中心度分布")
plot(table(sp.G11),xlab="测地线距离",ylab="频数",main="节点间的测地线距离分布")

library("entropy")
entropy(y=table(d.G11),unit="log2") 


##############规则网络：k-规则网、星形网络和平衡2-叉树网络
layout(matrix(1:9,nrow=3,byrow=TRUE))
set.seed(12345)
G<-graph.full(n=10)   
plot(G,main=c("平均测地线距离",average.path.length(graph=G)))
G<-graph.ring(n=10)  
plot(G,main=c("平均测地线距离",round(average.path.length(graph=G),2)))
set.seed(12345)
G<-lapply(c(1,3:8),FUN=k.regular.game,no.of.nodes=10)  
sapply(G,FUN=function(x) 
  plot(x,vertex.label=NA,main=c("平均测地线距离",round(average.path.length(graph=x),2))))

par(mfrow=c(2,1))
set.seed(12345)
G<-graph.star(n=10,mode="undirected")  
entropy(y=table(degree(graph=G)),unit="log2")  
plot(G,vertex.label=NA,main=c("星形网络平均测地线距离",average.path.length(graph=G)))  
G<-graph.tree(n=15,children=2,mode="undirected") 
entropy(y=table(degree(graph=G)),unit="log2")
plot(G,vertex.size=30,main=c("2-叉树网络平均测地线距离",round(average.path.length(graph=G),2)))  #属性网络

##############随机网络：生成Erdos-Renyi随机网络
set.seed(12345)
par(mfrow=c(1,2))
ER<-erdos.renyi.game(n=100,p.or.m=200,type="gnm")  
barplot(table(degree(graph=ER))/100,xlab="度",ylab="频率",
  main=c("随机网络的度分布",paste("平均度:",mean(degree(graph=ER)),sep="")))  
lines(0:10,dpois(0:10,lambda=4),col=2)  

##############随机网络：熵
den.ER<-vector()
en.ER<-vector()
library("entropy")
set.seed(12345)
for(i in 100:4900){
 ER<-erdos.renyi.game(n=100,p.or.m=i,type="gnm")
 den.ER<-c(den.ER,graph.density(graph=ER))
 en.ER<-c(en.ER,entropy(y=table(degree(graph=ER)),unit="log2"))
}
plot(den.ER,en.ER,xlab="网络密度",ylab="网络熵",cex=0.5)

#########小世界网络
set.seed(12345)
par(mfrow=c(2,2))
G<-watts.strogatz.game(dim=1,size=50,nei=3,p=0)    
plot(G,vertex.label=NA,vertex.size=5,main="规则网络")
plot(degree(graph=G),main="规则网络的度序列",xlab="",ylab="节点度",type="l")
transitivity(graph=G,type="global")  
average.path.length(graph=G,directed=FALSE)  
G<-watts.strogatz.game(dim=1,size=50,nei=3,p=0.01)  
plot(G,vertex.label=NA,vertex.size=5,main="WS小世界网络(p=0.01)")  
plot(density(degree(graph=G)),main="WS小世界网络(p=0.01)的度分布",xlab="节点度",ylab="密度")
transitivity(graph=G,type="global")
average.path.length(graph=G,directed=FALSE)

########无标度网络:生成BA网络
set.seed(12345)
par(mfrow=c(2,2))
G<-barabasi.game(n=50,m=3,directed=FALSE)
plot(G,main="BA网络",vertex.label=NA,vertex.size=10)
d.G<-degree(graph=G)   
barplot(sort(table(d.G),decreasing=TRUE),xlab="节点度",ylab="频数",main="BA网络的度分布")
parm<-power.law.fit(x=d.G)  
parm$alpha  
x<-min(d.G):max(d.G)  
y<-x^-parm$alpha  
plot(x,y,type="l",main=c("幂率分布",paste("alpha=",round(parm$alpha,2),sep="")))
x1<-x*10   
y1<-y*(10^-parm$alpha)
plot(x1,y1,type="l",main="无标度特征")

G<-static.power.law.game(no.of.nodes=vcount(G),no.of.edges=ecount(G),exponent.out=parm$alpha)
plot(G,main="指定参数的无标度网络",vertex.label=NA,vertex.size=10)
barplot(sort(table(d.G),decreasing=TRUE),xlab="节点度",ylab="频数",main="无标度网络的度分布")

#########BA网络的密度和熵
library("entropy")
den.BA<-vector()
en.BA<-vector()
set.seed(12345)
for(i in 3:50){
 BA<-barabasi.game(n=50,m=i,directed=FALSE)
 den.BA<-c(den.BA,graph.density(graph=BA))
 en.BA<-c(en.BA,entropy(y=table(degree(graph=BA)),unit="log2"))
}
plot(den.BA,en.BA,xlab="网络密度",ylab="网络熵",cex=0.5,main="BA网络的密度与熵")



