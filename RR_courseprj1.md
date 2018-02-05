---
title: "Reproducible Research - Course Project 1"
author: "Eduardo vanzeller"
date: "4 de Fevereiro de 2018"
output: html_document
---
##Step 01
##Code for reading in the dataset and/or processing the data
```{r, echo = TRUE}
activity<-read.csv("activity.csv")
```

## Step 02
##Histogram showing total number of steps taken each day
```{r, echo = TRUE}
library(ggplot2)
Z1<-data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
Z1$date<-rownames(Z1)
rownames(Z1)<-NULL
names(Z1)[[1]]<-"Total Steps"
png("plot1.png")
#Total Steps by date
ggplot(Z1,aes(y=Z1$`Total Steps`,x=Z1$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
dev.off()
ggplot(Z1,aes(y=Z1$`Total Steps`,x=Z1$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
#Histogram of total steps
qplot(Z1$`Total Steps`,geom="CP1-Histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Histogram")
png("plot1.1.png")
qplot(Z1$`Total Steps`,geom="CP1 - Histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Histogram")
dev.off()
```

##Step 03
##Mean and median number of steps taken per day
```{r, echo = TRUE}
library(dplyr)
Z3<-data.frame(round(tapply(activity$steps,activity$date,mean,na.rm=TRUE),2))
Z3$date<-rownames(Z3)
rownames(Z3)<-NULL
names(Z3)[[1]]<-"Mean Steps"
temp<-activity%>%select(date,steps) %>% group_by(date) %>% summarise(median(steps))
names(temp)[[2]]<-"Median Steps"
Z3$median<-temp$`Median Steps`
Z3<-Z3 %>% select(date,`Mean Steps`,median)
```

##Step 04
##Time series plot of the average number of steps taken
```{r, echo = TRUE}
Z4<-Z3
Z4$date<-as.Date(Z4$date,format="%Y-%m-%d")
ggplot(Z4,aes(x=Z4$date,y=Z4$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean Steps Every day")+xlab("Date")+ggtitle("Mean Steps by Date")
png("plot4.png")
ggplot(Z4,aes(x=Z4$date,y=Z4$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean Steps Every day")+xlab("Date")+ggtitle("Mean Steps by Date")
dev.off()
```


##Step 05
##The 5-minute interval that, on average, contains the maximum number of steps

```{r, echo = TRUE}
#
activity$interval<-factor(activity$interval)
Z5<-aggregate(data=activity,steps~date+interval,FUN="mean")
Z5<-aggregate(data=Z5,steps~interval,FUN="max")
```


##Step 06
Code to describe and show a strategy for inputing missing data
There are different ways to deal with missing values. Here was used the mean/mode value substitution technic to input missing values. That is, using the mean values to replace the missing values in the original data set, this will obvious affect the final results

```{r, echo = TRUE}
Z6<-activity
Z6$Missing<-is.na(Z6$steps)
Z6<-aggregate(data=Z6,Missing~date+interval,FUN="sum")
Z6.1<-data.frame(tapply(Z6$Missing,Z6$date,sum))
Z6.1$date<-rownames(Z6.1)
rownames(Z6.1)<-NULL
names(Z6.1)<-c("Missing","date")
Z6.1$date<-as.Date(Z6.1$date,format="%Y-%m-%d")

Z6.2<-data.frame(tapply(Z6$Missing,Z6$interval,sum))
Z6.2$date<-rownames(Z6.2)
rownames(Z6.2)<-NULL
names(Z6.2)<-c("Missing","Interval")

par(mfrow=c(1,2))
plot(y=Z6.1$Missing,x=Z6.1$date,main="Missing Value Distribution by Date")
plot(y=Z6.2$Missing,x=Z6.2$Interval,main="Missing Value Distribution by Interval")
table(activity$date)
```



```{r, echo = TRUE}
#Dates that have missing values 
library(lubridate)
Z6.3<-as.data.frame(Z6.1) %>% select(date,Missing) %>% arrange(desc(Missing))
Z6.3<-Z6.3[which(Z6.3$Missing!=0),]
Z6.3$Weekday<-wday(Z6.3$date,label=TRUE)
Z6.4<-activity
Z6.4$weekday<-wday(Z6.4$date,label=TRUE)
#Mean of steps every monday, and every interval
Z6.5<-aggregate(data=Z6.4,steps~interval+weekday,FUN="mean",na.rm=TRUE)
#Merge the pre-imputation table Z6.4 table with the average table Z6.5
Z6.6<-merge(x=Z6.4,y=Z6.5,by.x=c("interval","weekday"),by.y=c("interval","weekday"),all.x=TRUE)
Z6.6$Steps.Updated<-0
for (i in 1:dim(Z6.6)[[1]]){
if(is.na(Z6.6[i,3])){Z6.6[i,6]=Z6.6[i,5]}
else {Z6.6[i,6]=Z6.6[i,3]}
}
Z6.6 <-Z6.6  %>% select(date,weekday,interval,Steps.Updated)
names(Z6.6)[[4]]<-"Steps"

```


## Step 07
Histogram of the total number of steps taken each day after missing values (MV) are replaced

```{r, echo = TRUE}
png("plot7.png")
qplot(Z6.6$Steps,geom="CP1-Histogram",main="Total steps taken histogram post (MV) inputation",xlab="Steps",ylab="Count")
dev.off()
qplot(Z6.6$Steps,geom="CP1-Histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")

```




## Step 8
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, echo = TRUE}
Z8<-Z6.6
levels(Z8$weekday)<-c(1,2,3,4,5,6,7)
Z8$WDWE<-Z8$weekday %in% c(1,2,3,4,5)
Z8.1<-aggregate(data=Z8,Steps~interval+WDWE,mean,na.rm=TRUE)
Z8.1$WDWE<-as.factor(Z8.1$WDWE)
levels(Z8.1$WDWE)<-c("Weekend","Weekday")
png("plot8.png")
ggplot(data=Z8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")
dev.off()
ggplot(data=Z8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")

#Producing the panel plot
Z8.1$interval<-as.numeric(as.character(Z8.1$interval))
library(lattice)
xyplot(data=Z8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
library(hexbin)
hexbinplot(data=Z8.1,Steps~interval|WDWE, aspect = 1, bins=50)
png("plott8.1.png")
xyplot(data=Z8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
dev.off()

png("plot8.2.png")
hexbinplot(data=Z8.1,Steps~interval|WDWE, aspect = 1, bins=50)
dev.off()
```




