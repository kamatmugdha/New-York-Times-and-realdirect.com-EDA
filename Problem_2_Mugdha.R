## Problem 2
## Create a new variable, age_group, that categorizes users as "<20", "20-29","30-39", "40-49", "50-59", "60-69", and "70+"
  
#getwd()
#setwd()

nyt<-c('nyt1','nyt2','nyt3')
for (nyt in nyt){
  
  data=read.csv(paste(nyt,".csv",sep=""))
  
  #categorization of Age  and Signed_In/Not
  data$age_group <- cut(data$Age, c(-Inf,20,29,39,49,59,69,Inf))
  levels(data$age_group)<-c("<20","20-29","30-39","40-49","50-59","60-69","70+")
  data$Signed_In<- factor(data$Signed_In)
  
  if (nyt=="nyt1"){ data1=data }
  else if (nyt=="nyt2"){data2=data}
  else {data3=data}
}

head(data1)
summary(data1)

data1$Gender_code[data1$Gender==0]<-"Female"
data1$Gender_code[data1$Gender>0]<-"Male"
data1$Gender_code[data1$Signed_In==0]<-"Unknown"
data1$Gender_code<- factor(data1$Gender_code)

data2$Gender_code[data2$Gender==0]<-"Female"
data2$Gender_code[data2$Gender>0]<-"Male"
data2$Gender_code[data2$Signed_In==0]<-"Unknown"
data2$Gender_code<- factor(data2$Gender_code)

data3$Gender_code[data3$Gender==0]<-"Female"
data3$Gender_code[data3$Gender>0]<-"Male"
data3$Gender_code[data3$Signed_In==0]<-"Unknown"
data3$Gender_code<- factor(data3$Gender_code)

summary(data1)
head(data3)

data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <- "Clicks"

data2$scode[data2$Impressions==0] <- "NoImps"
data2$scode[data2$Impressions >0] <- "Imps"
data2$scode[data2$Clicks >0] <- "Clicks"

data3$scode[data3$Impressions==0] <- "NoImps"
data3$scode[data3$Impressions >0] <- "Imps"
data3$scode[data3$Clicks >0] <- "Clicks"

data1$scode <- factor(data1$scode)
data2$scode <- factor(data2$scode)
data3$scode <- factor(data3$scode)

head(data3)

## For each day, Plot the distribution of number of impressions and click-through-rate (CTR =clicks/impressions) for these age categories

library(ggplot2)
ggplot(subset(data1, Impressions>0), aes(x=Impressions,fill=age_group))+
  geom_histogram(position="dodge",binwidth=1,colour="Black")+
  labs(title = "Day 1 :Impressions Distribution") +  facet_grid(age_group ~ .)

ggplot(subset(data2, Impressions>0), aes(x=Impressions,fill=age_group))+
  geom_histogram(position="dodge",binwidth=1,colour="Black")+
  labs(title = "Day 2 :Impressions Distribution") +  facet_grid(age_group ~ .)

ggplot(subset(data3, Impressions>0), aes(x=Impressions,fill=age_group))+
  geom_histogram(position="dodge",binwidth=1,colour="Black")+
  labs(title = "Day 3 :Impressions Distribution") +  facet_grid(age_group ~ .)
#Plotting the distribution of number of Clicks
ggplot(subset(data1, Clicks>0), aes(x=Clicks,fill=age_group))+
  geom_histogram(position="dodge",binwidth=1,colour="Black")+
  labs(title = "Day 1: Clicks Distribution")

ggplot(subset(data2, Clicks>0), aes(x=Clicks,fill=age_group))+
  geom_histogram(position="dodge",binwidth=1,colour="Black")+
  labs(title = "Day 2:Clicks Distribution") 

ggplot(subset(data3, Clicks>0), aes(x=Clicks,fill=age_group))+
  geom_histogram(position="dodge",binwidth=1,colour="Black")+
  labs(title = "Day 3:Clicks Distribution") 
#plotting CTR

ggplot(subset(data1, Clicks>0 ), aes(x=Clicks/Impressions,colour=age_group))+
  geom_density() + labs(title = "Day 1: CTR Distribution")

ggplot(subset(data2, Clicks>0 & Impressions>0), aes(x=Clicks/Impressions,colour=age_group))+
  geom_density() + labs(title = "Day 2: CTR Distribution")  +facet_grid(age_group ~ .)

ggplot(subset(data3, Clicks>0 & Impressions>0), aes(x=Clicks/Impressions,colour=age_group))+
  geom_density() +  labs(title = "Day 3: CTR Distribution")+ facet_grid(age_group ~ .)

## Extend your analysis across days. Visualize some metrics and distributions over time
  
  
data1$Day = 1
data2$Day = 2
data3$Day = 3

alldata<- rbind(data1,data2,data3)

alldata$Day <- factor(alldata$Day,labels=c("Day1","Day2","Day3"))
summary(alldata)

ggplot(subset(alldata,Clicks>0 & Impressions>0),aes(x=Clicks/Impressions, fill=Gender_code))+
  geom_bar()+  facet_grid(Day~Gender_code)

par("mar")
par(mar=c(6.5,6,4,1), mgp=c(5,1,0))
ggplot(subset(alldata, Clicks>0),aes(x=age_group, fill=Gender_code))+
  geom_bar(colour="Black") +facet_grid(Day~.)

dayImp = aggregate(alldata$Impressions, by=list(Day=alldata$Day),sum)
dayclk = aggregate(alldata$Clicks, by=list(Day=alldata$Day),sum)

dayImp
dayclk
par("mar")
#par(mar=c(5,5,5,5))
par(mar=c(6.5,6,4,1), mgp=c(5,1,0))

plot(dayImp, main="Trend of Days vs Impressions",xlab(""),ylab("Impressions"))
plot(dayclk ,main="Trend of Days vs Clicks",xlab(""),ylab("Clicks"))

summary(alldata$Impressions)
summary(alldata$Clicks)


