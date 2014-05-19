rm(list=ls())

#load the data
data <- read.csv(unz(description=file.path(getwd(),"activity.zip"),
                        filename="activity.csv"), header=TRUE,sep=",")

# get rid of missing values to make plots
tempdata <- data[complete.cases(data),]

##work out the total number of steps for each day (and the sum is not normally zero) 
##do the histogram of that set and calculate the mean and median.


sp <- split(tempdata,as.factor(tempdata$date))
res <- sapply(sp, function(x){
    sum(x$steps)
})

hist(res)
mean(res)
median(res)

sp2<- split(data,as.factor(data$interval))
avgsteps <- sapply(sp2, function(x){
    mean(x$steps,na.rm=TRUE)
})
avgstepsdf<- data.frame(interval=names(avgsteps), avgsteps=avgsteps)



with(avgstepsdf,
    plot(interval,avgsteps,type="l",xlab="5 minute interval",
         ylab="average no of steps averaged across all days", col="blue")
)

avgstepsdf[which(avgstepsdf$avgsteps==max(avgstepsdf$avgsteps)),]



nadata <- data[!complete.cases(data),]
for (i in 1:nrow(nadata)){
    int <- nadata$interval[i]
    print(int)
    print(avgstepsdf[which(avgstepsdf==int),]$avgsteps)
    nadata$steps[i] <- avgstepsdf[which(avgstepsdf==int),]$avgsteps
}

nadata$steps <- apply(nadata, 1, function(x){
    int <- as.numeric(x["interval"])
    avgstepsdf[match(int,avgstepsdf$interval),"avgsteps"]
})


