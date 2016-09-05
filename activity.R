library(lattice)

act<- read.csv("activity.csv",sep = ',',header = TRUE)
act$date<-as.Date.factor(act$date)
steps_date <- aggregate(steps ~ date, act, sum)

hist(step_date$steps, col = 'maroon',xlab ="Number of Steps",main = "Total Steps per Day")

rmean <-mean(step_date$steps)
rmedian<-median(step_date$steps)

plot(steps_interval$steps,type = 'l',xlab = "Interval", ylab = 'No. of Steps', main = "Average Number of Steps per Day by Interval",col="darkblue")
steps_interval[which.max(steps_interval$steps),]

sum(!complete.cases(act))
act_clean <- transform(act, steps = ifelse(is.na(act$steps), steps_interval$steps[match(act$interval, steps_interval$interval)], act$steps))
steps_date_clean <- aggregate(steps ~ date, act_clean, sum)

hist(steps_date_clean$steps, main = "Total Steps Each Day", col="darkblue", xlab="Number of Steps")
hist(step_date$steps, main = "Total Steps Each Day", col="maroon", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("darkblue", "maroon"), lwd=10)

mean_diff <- rmean_cl- rmean
med_diff  <- rmedian_cl- rmedian
