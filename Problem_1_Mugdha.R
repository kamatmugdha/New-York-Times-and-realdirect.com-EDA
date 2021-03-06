## Problem 1
## Load in and clean the data.
#getwd()
#setwd()
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_171')
library('plyr')
library('gdata')
library('ggplot2')
library('xlsx')
alldatasets<- c("bronx","brooklyn","manhattan","queens","statenisland")

for (alldatasets in alldatasets){
  df=read.xls(paste("rollingsales_",alldatasets,".xls",sep=""),perl = "C:\\Strawberry\\perl\\bin\\perl.exe", pattern="BOROUGH")
  names(df)=tolower(names(df))
  
  #Data cleaning with regular expressions
  df$sale.price.n <-as.numeric(gsub("[^[:digit:]]","",df$sale.price))
  df$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",df$gross.square.feet))
  df$land.sqft<- as.numeric(gsub("[^[:digit:]]","",df$land.square.feet))
  
  df$borough <- as.character(df$borough)
  
  df$sale.date <- as.Date(df$sale.date)
  
  df$year.built = as.numeric(as.character(df$year.built))
  if (alldatasets=="bronx"){ bx=df }
  else if (alldatasets=="brooklyn"){bk=df}
  else if (alldatasets=="manhattan"){mh=df}
  else if (alldatasets=="queens"){qn=df}
  else {si=df}
}
### **Combining all the boroughs data in one Dataframe.**

allboroughs= rbind(bx,bk,mh,qn,si)
summary(allboroughs)
str(allboroughs)

## Conduct exploratory data analysis in order to find out where there are outliers or missing values, decide how you will treat them, make sure the dates are formatted correctly, make sure values you think are numerical are being treated as such, etc.
  
par("mar")
par(mar=c(5,5,5,5))
#checking if the data for all boroughs is correct
hist(main = "Histogram of Allbroughs Saleprice",allboroughs$sale.price.n)

allboroughs.sale = allboroughs[allboroughs$sale.price.n!=0,]
plot(allboroughs.sale$gross.sqft,allboroughs.sale$sale.price.n, title(main="Allboroughs Gross sqft vs Sale price"))

plot(log(allboroughs.sale$gross.sqft),log(allboroughs.sale$sale.price.n),title(main="Allboroughs Gross sqft vs Sale price"))

allboroughs.homes = allboroughs.sale [which(grepl("FAMILY | CONDOS | COOPS",
                                              allboroughs.sale$building.class.category)),]
#checking the building class category
unique(allboroughs.homes$building.class.category)

plot(log(allboroughs.homes$gross.sqft),log(allboroughs.homes$sale.price.n),title(main="Allboroughs Gross sqft vs Sale price"))
#labeling the boroughs
allboroughs.homes$borough <- factor(allboroughs.homes$borough,labels = c("Manhattan","Bronx","Brooklyn","Queens","Staten Island"))

library("ggplot2")
ggplot(allboroughs.homes, aes(x=borough, fill=borough))+geom_bar(colour="black")+
  ggtitle("Actual sales of Family home, Condos, and Coops for all boroughs")


ggplot(subset(allboroughs.homes, gross.sqft>0), aes(x=log(gross.sqft),colour=borough))+geom_density()+
  ggtitle("Gross sqft for all boroughs")

ggplot(allboroughs.homes, aes(x=borough,y=log(sale.price.n),fill=borough))+geom_point()+geom_boxplot()+
  ggtitle("Sale price for all boroughs")

#Removing outliers
allboroughs.homes[which(allboroughs.homes$sale.price.n<100000),][order(allboroughs.homes[which(allboroughs.homes$sale.price.n<100000),]$sale.price.n),]

allboroughs.homes$outliers <- (log(allboroughs.homes$sale.price.n) <= 5) + 0
allboroughs.homesnooutliers <- allboroughs.homes[which(allboroughs.homes$outlier==0 & allboroughs.homes$gross.sqft>0),]

summary(allboroughs.homesnooutliers)
ggplot(allboroughs.homesnooutliers, aes(x=borough, fill=borough))+geom_bar(colour="black")+
  ggtitle("Actual sales of Family home, Condos, and Coops for all boroughs without Outliers")

ggplot(subset(allboroughs.homesnooutliers, gross.sqft>0), aes(x=log(gross.sqft),colour=borough))+geom_density()+
  ggtitle("Gross sqft for all boroughs without Outliers")

ggplot(allboroughs.homesnooutliers, aes(x=borough,y=log(sale.price.n),fill=borough))+geom_point()+geom_boxplot()+
  ggtitle("Sale price for all boroughs without Outliers")

plot(log(allboroughs.homesnooutliers$gross.sqft),log(allboroughs.homesnooutliers$sale.price.n),title(main="Allboroughs Gross sqft vs Sale price without Outliers"))

## Conduct exploratory data analysis to visualize and make comparisons for residential building category classes across boroughs and across time

summary(allboroughs.homes$year.built)

# For years >0 
allboroughs.homes<- allboroughs.homes[allboroughs.homes$year.built!=0,]
summary(allboroughs.homes$year.built)

allboroughs.homes$yearscat <- cut(allboroughs.homes$year.built,c(0,1850,1900,1950,2000,Inf),labels <-c("<1800","1850-1899","1900-1949","1950-1999","2000>"))
#allboroughs.homes$yearscat

means <- with(allboroughs.homes,aggregate(x=list(Y=sale.price.n),by=list(A=yearscat, B=borough),mean))
with(means,interaction.plot(x.factor=A, trace.factor=B, response=Y, type='l',col=B,main ="Average of sale prices by boroughs",xlab = "Year built",ylab = "Average sale price"))

allboroughs.homes$newbuildingcat = allboroughs.homes$building.class.category
tt = as.vector(allboroughs.homes$newbuildingcat)
tt[grep("COOPS", tt)] = "COOPS"
tt[grep("CONDOS",tt)] = "CONDOS"
tt[grep("ONE FAMILY",tt)] = "ONE"
tt[grep("TWO FAMILY",tt)] = "TWO"
tt[grep("THREE FAMILY",tt)] = "THREE"
allboroughs.homes$newbuildingcat = tt
homes =with(allboroughs.homes,aggregate(x=list(Y=sale.price.n),by=list(A=newbuildingcat, B=borough),mean))
with(homes,interaction.plot(x.factor=A, trace.factor=B, response=Y, type='l',
main ="Average of sale prices by buliding class category",xlab = "Building class",ylab = "Average sale price"))

par("mar")
par(mar=c(6,6,6,6))
ggplot(allboroughs.homes , aes(x=borough, fill=building.class.category))+ geom_bar(colour="black")

ggplot(allboroughs.homes , aes(x=yearscat, fill=borough))+ geom_bar(colour="black")

ggplot(allboroughs.homes , aes(x=yearscat, fill=building.class.category))+ geom_bar(colour="black")

