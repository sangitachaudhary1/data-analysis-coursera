require(graphics)

par(mfrow=c(1, 1))
par(las=2)

setwd(dir="/home/hindol/Workspace/DataAnalysis/HW3/")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

sf <- names(table(outcome$State)[table(outcome$State) >= 20])

outcome2 <- outcome[outcome$State %in% sf, ]

death <- as.numeric(outcome2[, 11])
state <- factor(outcome2$State, ordered=T)
state <- reorder(state, death, function(x){ median(x, na.rm=T) }, order=T)

boxplot(death ~ state,
        main="Heart Attack 30-day Death Rate by State",
        ylab="30-day Death Rate")
