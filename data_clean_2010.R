library(dplyr)
library(Matrix)
library(data.table)
library(readr)
library(stringr)
library(tidyr)

dir_list<-dir(path="2009-2010.regular_season/",all.files=F,full.names = T)

records<-read.csv(dir_list[1],fill=T,header=T)

for (i in 2:length(dir_list)){
 data<-read.csv(dir_list[i],fill=T,header=T)
 records<-rbind(records,data)
 print(dir_list[i])
}

records<-records[,-c(1:10,31,32)]
records<-records[,-(5:10)]
raw_record<-records[,c(1:4,9,11,12,14)]
raw_record[is.na(raw_record$points),]$points<-0
raw_record[which((raw_record$etype=="free throw") & (raw_record$result=="made") ),]$points<-1


raw_record<-separate(raw_record,time,c("minute","second"),sep=":",remove=F)
raw_record$minute<-as.numeric(raw_record$minute)
raw_record$second<-as.numeric(raw_record$second)

raw_record$time_num<-raw_record$minute*60+raw_record$second
save(raw_record,file="raw_record.RData")

timeout_index<-which((raw_record$etype=="timeout") & (raw_record$type!="official"))
timeout<-raw_record[timeout_index,]
timeout<-timeout[,c(1:6,11)]
n<-length(timeout_index)
score<-data.frame(sum_team=rep(0,n),sum_op=rep(0,n))
for (i in 1:n){
  index<-timeout_index[i]
  time<-timeout[i,]$time_num
  team<-timeout[i,]$team
  record<-raw_record[index,]
  sum_team<-0;sum_op<-0
  while ((time>=0) & (time>=(timeout[i,]$time_num-60)) & (time<=timeout[i,]$time_num)){
    if (record$team==team){
      sum_team<-sum_team+record$points
    } else {
      sum_op<-sum_op+record$points
    }
     index<-index+1
     record<-raw_record[index,]
     time<-record$time_num
  }
  score[i,1]<-sum_team
  score[i,2]<-sum_op
} 
save(timeout,score,timeout_index,file="timeout.RData")
timeout<-cbind(timeout,score$sum_team-score$sum_op)
names(timeout)[8]<-"diff"
write.csv(timeout,file="timeout.csv")
hist(timeout$diff,breaks=100)
# raw_record<-raw_record[,-(2:3)]


# LAL<-filter(records,team=="LAL")
# 
#      # filter(result=="made")
# lal_free_throw<-filter(LAL,etype=="free throw") %>%
#                 filter(result=="made")
# score_L<-sum(LAL$points,na.rm=T)
