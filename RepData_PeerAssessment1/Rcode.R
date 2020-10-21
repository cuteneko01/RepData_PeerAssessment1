unzip("activity.zip")
activity<-read.csv("activity.csv")
str(activity)
summary(activity)
activity$date<-as.Date(activity$date,"%Y-%m-%d")
library(magrittr)
library(dplyr)

##task1
dailysteps<-with(activity,aggregate(steps,by=list(date),FUN=sum))
hist(dailysteps[,2],main="Histogram of total number of steps taken per day",
     xlab="Steps",ylim=c(0,30))
m<-as.numeric(gsub(".*:","",summary(dailysteps[,2])))

##task2
activity$interval<-sprintf("%04d",activity$interval)%>%strptime("%H%M")%>%as.POSIXlt("%H:%M")
stepsByInterval<-activity%>%group_by(interval)%>%summarize(avgSteps=mean(steps,na.rm=TRUE))
plot(stepsByInterval$interval,stepsByInterval$avgSteps,type="l",
     xlab="Time",ylab="Average number of steps",main="Average daily activity pattern over Oct and Nov in 2012")
max<-stepsByInterval[stepsByInterval$avgSteps==max(stepsByInterval$avgSteps),]

##task3
activity<-read.csv("activity.csv")
sum(is.na(activity$steps))
activity$interval<-sprintf("%04d",activity$interval)%>%strptime("%H%M")%>%format("%H:%M")
stepsByInterval<-activity%>%group_by(interval)%>%summarize(avgSteps=mean(steps,na.rm=TRUE))
imputed<-merge(activity,stepsByInterval,by=intersect(names(activity), names(stepsByInterval)))
# for (i in 1:nrow(imputed)){
#     if (is.na(imputed[i,"steps"])){
#         imputed[i,"steps"]<-imputed[i,"avgSteps"]
#     }
# }
imputed<-within(imputed, steps[is.na(steps)]<-avgSteps[is.na(steps)])
# dailysteps<-with(imputed,aggregate(steps,by=list(date),FUN=sum))
# hist(dailysteps[,2],main="Histogram of total number of steps taken per day",
#      xlab="Steps",ylim=c(0,40))

##task4
imputed$weekday<-weekdays(as.Date(imputed$date))
imputed$weekday<-as.factor(if_else(imputed$weekday %in% c("Monday","Tuesday",
                                                          "Wednesday","Thursday",
                                                          "Friday"), "Weekday", "Weekend"))
imputed<-select(imputed,-avgSteps)
imputed$interval<-as.POSIXct(strptime(imputed$interval,"%H:%M"),"%H:%M")
stepsByInterval<-imputed%>%group_by(weekday,interval)%>%summarize(avgSteps=mean(steps))
# par(mfrow=c(1,2))
# plot(stepsByInterval[stepsByInterval$weekday=="Weekday",]$interval,
#      stepsByInterval[stepsByInterval$weekday=="Weekday",]$avgSteps,
#      type = "l",
#      xlab="Time",
#      ylab="Average steps",
#      ylim=c(0,250))
# title(main=list("Average steps taken over weekday", cex=1))
# plot(stepsByInterval[stepsByInterval$weekday=="Weekend",]$interval,
#      stepsByInterval[stepsByInterval$weekday=="Weekend",]$avgSteps,
#      type = "l",
#      xlab="Time",
#      ylab="Average steps",
#      ylim=c(0,250))
# title(main=list("Average steps taken over weekend", cex=1))
# par(mfrow=c(1,1))
# plot(stepsByInterval$interval, stepsByInterval$avgSteps, type = "n", xlab="Time", 
#      ylab="Average steps", main="Average steps taken per day", ylim=c(0,250))
# lines(stepsByInterval[stepsByInterval$weekday=="Weekday",]$interval, 
#       stepsByInterval[stepsByInterval$weekday=="Weekday",]$avgSteps,
#       col="red")
# lines(stepsByInterval[stepsByInterval$weekday=="Weekend",]$interval, 
#       stepsByInterval[stepsByInterval$weekday=="Weekend",]$avgSteps, 
#       col="blue")
# legend("topright",lwd=1,col=c("red","blue"),legend=c("Weekday","Weekend"))
ggplot(aes(interval,avgSteps),data=stepsByInterval)+geom_line()+
  scale_x_datetime(date_labels="%H:%M")+
  facet_wrap(~weekday,nrow=2,ncol=1)+
  xlab("Time")+ylab("Average steps")+
  ggtitle("Average steps taken over a day")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))