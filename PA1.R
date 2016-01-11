#This function loads the data into a dataframe, and adds a factor column for the intervals values
loadActivityDF = function(){
  if (!exists("activityDF"))
  {
    message("Loading new data table")
    if (!file.exists("activity.csv"))
    {
      message("Unzipping source file")
      unzip("activity.zip")
    }
    activityDF = read.csv("activity.csv")
    activityDF$intervals = as.factor(as.character(activityDF$interval))
  }
  else {message("Using cached data")}
  
  activityDF
}

#This function aggregates the step data into a "per day" step count, and creates (and saves) a histogram of the resulting data.
summarizeSteps = function(dataset, fileName){
  summarySteps = aggregate(steps~date, data=dataset, sum, na.rm=TRUE)
  png(file=paste(".\\figures\\", fileName, ".png", sep=""), width=480, height=480)  
  hist(summarySteps$steps, breaks=20, xlab="Total Daily Steps", main="Total steps taken per day")
  dev.off()
  summarySteps
}

#This function calculates the mean number of taken per day
meanDailySteps = function(dataset){
  
  meanSteps = mean(dataset$steps, na.rm = TRUE)

  meanSteps
}

#This function calculates the median number of steps taken per day
medianDailySteps = function(dataset){
  medianSteps = median(dataset$steps, na.rm = TRUE)
  
  medianSteps
}

#This function calculates the mean number of steps for each time interval, across all days in the study.  It then produces (and saves) a line plot of the data
plotIntervalSteps = function(dataset, fileName){
  meanIntervalSteps = aggregate(steps~intervals, data=dataset, mean, na.rm=TRUE)
  intervals = data.frame(intervals=as.numeric(levels(dataset$intervals)), meanIntervalSteps)
  intervals = intervals[order(intervals$intervals), ]
  
  labels = c("00:00", "05:00", "10:00", "15:00", "20:00")
  labels.at = seq(0, 2000, 500)
  png(file=paste(".\\figures\\", fileName, ".png", sep=""), width=480, height=480)  
  plot(intervals$intervals, intervals$steps, type="l", main="Average steps @ 5 min intervals",
       ylab = "Average Steps", xlab = "Time of Day", xaxt = "n")
  axis(side = 1, at = labels.at, labels = labels)
  dev.off()
  intervals
}

maxIntervalSteps = function(dataset){
  sortedIntervalSteps = dataset[order(dataset$steps, decreasing = TRUE), ]
  maxIntervalSteps = sortedIntervalSteps$intervals[1]
  maxIntervalSteps
}

#This function calculates the number of missing step values (i.e. "NA" in source data) across all days and intervals in the study
calculateMissingData = function(dataset){
  dim(dataset[is.na(dataset$steps), ])[1]
}

#This function will imput the mean step count for a given interval in all cases where a step count is missing.  
#This value was chosen as the best way to reduce the calculation bias.  
#This function also calculates the day type (Weekend or Weekday) of each day in the study for
#futher reporting breakdowns
imputMissingData = function(dataset, intervalDF){
  completeData = dataset
  
  for (i in 1:dim(dataset)[1]){
    if(is.na(dataset$steps[i])) {
      completeData$steps[i] = intervalDF$steps[intervalStepsDF$intervals == dataset$interval[i]] 
    }
  }
  completeData$dayType = as.factor(c("Weekend", "Weekday", "Weekday", 
                           "Weekday", "Weekday", "Weekday", "Weekend")[as.POSIXlt(completeData$date)$wday + 1])
  
  completeData
}

#This function calculates the mean number of steps based on the intervals and day type (weekday or weekend)
#The resulting data is plotted (and saved) in a two panel line graph to show the difference
plotDayType = function(dataset, fileName){
  library(ggplot2)
  meanIntervalSteps = aggregate(steps~intervals+dayType, data=dataset, mean)
  intervals = data.frame(intervals=as.numeric(levels(dataset$intervals)), meanIntervalSteps)
  intervals = intervals[order(intervals$intervals), ]
  
  png(file=paste(".\\figures\\", fileName, ".png", sep=""), width=480, height=480)  
  plot = ggplot(data=intervals, aes(x=intervals, y = steps, colour = factor(dayType)), environment = environment())
  plot = plot + geom_line() 
  plot = plot + facet_grid(dayType ~ .)
  
  print(plot)
  dev.off()
}

message("Starting processing")
activityDF = loadActivityDF()
summaryDF = summarizeSteps(activityDF, "rawHistogram")
meanSteps = meanDailySteps(summaryDF)
medianSteps = medianDailySteps(summaryDF)
intervalStepsDF = plotIntervalSteps(activityDF, "intervalSteps")
maxInterval = maxIntervalSteps(intervalStepsDF)
countMissingData = calculateMissingData(activityDF)
completeDataDF = imputMissingData(activityDF, intervalStepsDF)
completeSummaryDF = summarizeSteps(completeDataDF, "completeHistogram")
meanCompleteSteps = meanDailySteps(completeSummaryDF)
medianCompleteSteps = medianDailySteps(completeSummaryDF)
plotDayType(completeDataDF, "intervalStepsByDayType")
message("Processing complete")
