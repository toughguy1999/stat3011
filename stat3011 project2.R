data=NULL
n=0
N=10000
## generate N random vectors as the first generation
repeat{
rv=runif(10,0,10)
  if(prod(rv)>=0.75){
    data=rbind(data,rv)
    n=n+1
  }
  if(n==N)
    break
}  
##compute the fitness value of all the vectors
m=1:10
fitness=NULL
for(i in 1:N)
  fitness[i]=abs((sum(cos(data[i,])^4)-2*prod(cos(data[i,])^2))/sqrt(sum(m*data[i,m]^2)))


##repeat this part
n=0
repeat{
bad=which(fitness<=summary(fitness)[2])
good=which(fitness>=summary(fitness)[5])
for(i in good){
  for(j in 1:10){
    data[i,j]=data[i,j]+rnorm(1,0,0.001) ##the mutation should be smaller when the result closer to the maximum
  }
}
data1=data[good,]
data=data[-bad,]
data=rbind(data,data1)
fitness=NULL
for(i in 1:nrow(data)){
  if(prod(data[i,])>=0.75)
  fitness[i]=abs((sum(cos(data[i,])^4)-2*prod(cos(data[i,])^2))/sqrt(sum(m*data[i,m]^2)))
  else fitness[i]=0
}
n=n+1
if(n>=500)
  break
}
summary(fitness)
length(fitness)


##check the value
r=which(fitness==max(fitness))
prod(data[7215,])

fitness[i]=abs((sum(cos(data[128196,])^4)-2*prod(cos(data[128196,])^2))/sqrt(sum(j*data[128196,j]^2)))

q=NULL
for(i in good)
  for(j in j)
    q=c(q,data(data[i,j]<0 | data[i,j]>10))
