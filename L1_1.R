a<-c(1,2,3)
print(a)
a<-a*4
print(a)

sink("MyOutput.txt",append=FALSE,split=FALSE)
a<-c(1,2,3)
print(a)
a<-a*4
print(a)
sink()