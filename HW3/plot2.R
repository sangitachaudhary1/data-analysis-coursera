require(graphics)

setwd(dir="/home/hindol/Workspace/DataAnalysis/HW3/")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

outcome[, c(11, 17, 23)] <- sapply(outcome[, c(11, 17, 23)], as.numeric)

r <- range(outcome[, c(11, 17, 23)], na.rm=TRUE)

par(mfrow = c(3, 1))

m1 <- mean(outcome[, 11], na.rm=TRUE)
hist(outcome[, 11], prob=TRUE, main="Heart Attack", xlab="30-day Death Rate", xlim=r)
abline(v=median(outcome[, 11], na.rm=TRUE))
lines(density(na.omit(outcome[, 11])))

hist(outcome[, 17], main="Heart Failure", xlab="30-day Death Rate", xlim=r)
abline(v=median(outcome[, 17], na.rm=TRUE))

hist(outcome[, 23], main="Pneumonia", xlab="30-day Death Rate", xlim=r)
abline(v=median(outcome[, 23], na.rm=TRUE))
