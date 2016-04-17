data<-read.csv("/Users/yueqizhang/Documents/hackathon/MyData.csv",header=T)
period<-data$period
n<-dim(data)[1]
new<-c()
for(i in 2:n)
{
  if(period[i-1]==4&&period[i]==1) new<-cbind(new,i)
}
news<-cbind(1,new,530922)
m<-length(news)
points<-data$points
pointsnew<-c()
t<-as.numeric(data$team)
time46<-c()
time48<-c()
num.timeout<-c()
num.foul<-c()
for(j in 1:(m-1))
{
  for(k in news[j]:(news[j+1]-1))
  {
    teamnum<-t[news[j]+1]
    if(t[k]==teamnum)
    {
      sub<-data[news[j]:k,]
      tnew<-t[news[j]:k]
      pointsnew[k]<-sum(sub[which(tnew==teamnum),]$points)
    } else {
      sub<-data[news[j]:k,]
      tnew<-t[news[j]:k]
      pointsnew[k]<-sum(sub[which(tnew!=teamnum),]$points)
    }
  }
  sub1<-data[news[j]:(news[j+1]-1),]
  order<-sub1$X[which((sub1$period==4)&(sub1$time_num<=120))]
  subpointsnew<-pointsnew[order]
  tnews<-t[order]
  scorea<-c(min(subpointsnew[which(tnews==teamnum)]),max(subpointsnew[which(tnews==teamnum)]))
  scoreb<-c(min(subpointsnew[which(tnews!=teamnum)]),max(subpointsnew[which(tnews!=teamnum)]))
  time46<-cbind(time46,rbind(scorea[1],scoreb[1]))
  diff46<-time46[1,]-time46[2,]
  time48<-cbind(time48,rbind(scorea[2],scoreb[2]))
  diff48<-time48[1,]-time48[2,]
  sub2<-data[order,]
  timeoutnum<-length(sub2$etype[which(sub2$etype=="timeout")])
  num.timeout<-cbind(num.timeout,timeoutnum)
  foulnum<-length(sub2$etype[which(sub2$etype=="foul")])
  num.foul<-cbind(num.foul,foulnum)
}




