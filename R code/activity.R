library(dplyr)
library(ggplot2)
library(ggthemes)
library(timeDate)

# Loading and preprocessing the data
activity <- read.csv("D:/Coursera/Reproductible Research/Devoir/Week1/Data/activity.csv", stringsAsFactors=FALSE)
str(activity)

head(activity)

# Test if is the Na values for steps

NbNaSteps<- length(activity$steps[is.na(activity$steps)])
if (NbNaSteps > 0) {sprintf("There are NA values") }
ixNa <-which(is.na(activity$steps))

# date conversion
activity$date <-as.Date(activity$date)

# Question: What is mean total number of steps taken per day?

ldays <- length(activity$date)
nbdays<- as.numeric(activity$date[ldays] -activity$date[1])
maxsteps <- max(Activity.byDays$sumSteps)


# 

group_by(activity,date) %>% summarize(sumSteps  = sum(steps,na.rm =TRUE) ) -> Activity.byDay
head(Activity.byDay)

meansteps <- mean(Activity.byDay$sumSteps)
mediansteps <- median(Activity.byDay$sumSteps) 

sprintf("The Mean of the total steps by days is: %s", meansteps)
sprintf("The Median of the total steps by days is: %s", mediansteps)

# plotting Histogram 
g1 <- ggplot(Activity.byDay, aes(x= sumSteps)) + xlab("Total Steps") + ylab("Frequencies") + ggtitle("Mean total number of steps taken per day")
g1 <- g1 + geom_histogram(bins  = nbdays,fill="blue",alpha=.5 )

png(file="Figures/plotg1.png")
g1
dev.off()

# plotting Histogram with density line added
ggplot(Activity.byDay, aes(x= sumSteps, y= ..density..)) + geom_histogram(bins  = nbdays) +geom_density(colour= "red")

# plotting total steps by days with mean and median added
g2 <- ggplot(Activity.byDay, aes(x= date, y= sumSteps)) 
g2 <- g2 +  geom_line(colour="deepskyblue1") + geom_hline(aes(yintercept=meansteps), colour="violetred2", linetype="dashed") 
g2 <- g2 + geom_hline(aes(yintercept=mediansteps), colour="blue4" , linetype="dashed")+ ggtitle("Mean total number of steps taken per day")

png(file="Figures/plotg2.png")
g2
dev.off()
# question: What is the average daily activity pattern?

group_by(activity,interval) %>% summarize(meanSteps  = mean(steps,na.rm =TRUE) ) -> Activity.byInterval
head(Activity.byInterval)

# plotting the average

g3 <- ggplot(Activity.byInterval, aes(x= interval, y= meanSteps)) 
g3 <- g3 + geom_line(colour = "green")

g3

# computing the max interval of the average day
maxsteps<-max(Activity.byInterval$meanSteps)
maxinter<- filter(Activity.byInterval, meanSteps == max(meanSteps))$interval
hour <- 835 %/% 60
mns <- 835 %% 60
sprintf("The 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps: %s", maxinter)
sprintf("Which corresponds to: %sh %smns", hour,mns)

g3 <- g3 + geom_vline(aes(xintercept=maxinter), colour="red", linetype="dashed")

png(file="Figures/plotg3.png")
g3
dev.off()
# Imputing missing values
# the total missing value is  NbNaSteps
sprintf("The total of missing value is: %s", NbNaSteps)
#
# For the missing values we 
activity2 <- activity
t1 <- activity$interval[ixNa] 
tz <-as.numeric(rep(0,length(t1)))
for (i in 1:length(Activity.byInterval$interval)){tz[which(t1 %in% Activity.byInterval$interval[i])] <- round(Activity.byInterval$meanSteps[i])}
activity2$steps[ixNa] <- tz
head(activity2)

group_by(activity2,date) %>% summarize(sumSteps  = sum(steps,na.rm =TRUE) ) -> Activity2.byDay
head(Activity2.byDay)

mean2steps <- mean(Activity2.byDay$sumSteps)
median2steps <- median(Activity2.byDay$sumSteps) 

# plotting Histogram 
g4 <- ggplot(Activity2.byDay, aes(x= sumSteps)) 
g4 <- g4 + geom_histogram(bins  = nbdays,fill = "red",alpha=.2)
g4
png(file="Figures/plotg4.png")
g4
dev.off()

# Comparing with the first histogram
g5 <- g1 + geom_histogram(data=Activity2.byDay, aes(x= sumSteps) ,bins  = nbdays,fill="red",alpha=.2 )
g5
png(file="Figures/plotg5.png")
g5
dev.off()

# plotting total steps by days with mean and median added
g6 <- ggplot(Activity2.byDay, aes(x= date, y= sumSteps)) 
g6 <- g6 +  geom_line(colour) + geom_hline(aes(yintercept=mean2steps), colour="orange", linetype="dashed") 
g6 <- g6 + geom_hline(aes(yintercept=median2steps), colour="yellow")
g6

png(file="Figures/plotg6.png")
g6
dev.off()
# Comparing with the first time serie plot
g7 <- g2 + geom_line(data=Activity2.byDay, aes(x= date, y= sumSteps),colour="indianred1") + geom_hline(data=Activity2.byDay,aes(yintercept=mean2steps), colour="red4") 
g7 <- g7 +  geom_hline(data=Activity2.byDay,aes(yintercept=median2steps), colour="black", linetype="dashed")
g7
png(file="Figures/plotg7.png")
g7
dev.off()


# Questions Are there differences in activity patterns between weekdays and weekends?

week <- isWeekday(activity2$date)
activity2$weekdays <- factor(week,levels=c(FALSE, TRUE), labels=c('weekend', 'weekday') )
names(activity2$weekdays)<-NULL

group_by(activity2,weekdays,interval) %>% summarize(meanSteps  = mean(steps,na.rm =TRUE) ) -> Activity2.byInterval

g8 <-ggplot(Activity2.byInterval, aes(x=interval,y=meanSteps, color = weekdays)) +  geom_line() 
g8 <- g8+ scale_colour_tableau()+ ggtitle("average number of steps taken, averaged across all weekday days or weekend days") +theme(plot.title = element_text(size=20, face="bold", margin = margin(10, 0, 10, 0)))
g8 <- g8 + facet_wrap(~weekdays,ncol = 1)
g8

png(file="Figures/plotg8.png")
g8
dev.off()
