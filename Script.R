setwd("./RepData_PeerAssessment1/")
unzip("activity.zip")

ActData <- read.csv("activity.csv", header = TRUE, sep = ",")
# StepData <- na.omit(ActData)

attach(ActData)

Day_1_Step <- sum(ActData[which(ActData$date == as.Date("2012-10-02", format = "%Y-%m-%d"))], na.rm = TRUE)

sum()

attach(StepData)
plot(date, steps, type = "l")

hist(steps)
barplot(steps)



detach()
