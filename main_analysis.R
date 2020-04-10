#Load packages required

library(sqldf)
library(dplyr)
library(ggplot2)

#Create directory

if (!file.exists("./data")) {
        dir.create("./data")
}

#Set variables for download

downloadURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
downloadFile <- "./data/storm.data.csv.bz2"

#Download file 

if (!file.exists(downloadFile)) {
        download.file(downloadURL, downloadFile, method = "curl")
        unzip(downloadFile, overwrite = T, exdir = "./data")
}        

#Read in csv file

stormData <- read.csv("./data/storm.data.csv", sep = ",", header = TRUE)

#1. Across the United States, which types of events (EVTYPE) are most harmful with respect to population health?

#Look at Fatalities and Injuries; which EVTYPE has the highest amount of each?

#Using aggregate - 2 methods        

totalInjuries <- aggregate(INJURIES ~ EVTYPE, stormData, sum)
#totalInjuries <- aggregate(stormData$INJURIES, by = list(stormData$EVTYPE), sum)

#Using SQL

totalInjuries <- sqldf("select sum(INJURIES),EVTYPE from totalInjuries group by INJURIES order by INJURIES DESC")

#Using tapply
totalInjuries <- tapply(stormData$INJURIES, stormData$EVTYPE, max)
totalFatalities <- tapply(stormData$FATALITIES, stormData$EVTYPE, max)

#Arrange/order event types by the largest, only display the Top 15

#totalInjuries <- totalInjuries[order(-totalInjuries$INJURIES), ][1:15, ]
totalInjuries <- arrange(totalInjuries, desc(INJURIES), EVTYPE) [1:15, ]

#Order the events
totalInjuries$EVTYPE <- factor(totalInjuries$EVTYPE, levels = totalInjuries$EVTYPE)

#Make plot
ggplot(totalInjuries, aes(x = EVTYPE, y = INJURIES)) + 
        geom_bar(stat = "identity", fill = "red") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event Type") + ylab("Injuries") + ggtitle("Number of injuries per Top 15 event types")


#FATALITIES

totalFatalities <- aggregate(FATALITIES ~ EVTYPE, stormData, sum)

totalFatalities <- arrange(totalFatalities, desc(FATALITIES), EVTYPE) [1:15, ]
totalFatalities$EVTYPE <- factor(totalFatalities$EVTYPE, levels = totalFatalities$EVTYPE)

ggplot(totalFatalities, aes(x = EVTYPE, y = FATALITIES)) + 
        geom_bar(stat = "identity", fill = "red") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event Type") + ylab("Fatalities") + ggtitle("Number of fatalities per Top 15 event types")


#2. Across the United States, which types of events have the greatest economic consequences?

#Figures for crop and property need to be merged as there is a max of 3 plots per report

stormData$PROPDMGNUM = 0
stormData[stormData$PROPDMGEXP == "H", ]$PROPDMGNUM = stormData[stormData$PROPDMGEXP == "H", ]$PROPDMG * 10^2
stormData[stormData$PROPDMGEXP == "K", ]$PROPDMGNUM = stormData[stormData$PROPDMGEXP == "K", ]$PROPDMG * 10^3
stormData[stormData$PROPDMGEXP == "M", ]$PROPDMGNUM = stormData[stormData$PROPDMGEXP == "M", ]$PROPDMG * 10^6
stormData[stormData$PROPDMGEXP == "B", ]$PROPDMGNUM = stormData[stormData$PROPDMGEXP == "B", ]$PROPDMG * 10^9

stormData$CROPDMGNUM = 0
stormData[stormData$CROPDMGEXP == "H", ]$CROPDMGNUM = stormData[stormData$CROPDMGEXP == "H", ]$CROPDMG * 10^2
stormData[stormData$CROPDMGEXP == "K", ]$CROPDMGNUM = stormData[stormData$CROPDMGEXP == "K", ]$CROPDMG * 10^3
stormData[stormData$CROPDMGEXP == "M", ]$CROPDMGNUM = stormData[stormData$CROPDMGEXP == "M", ]$CROPDMG * 10^6
stormData[stormData$CROPDMGEXP == "B", ]$CROPDMGNUM = stormData[stormData$CROPDMGEXP == "B", ]$CROPDMG * 10^9


economicImpact <- aggregate(PROPDMGNUM + CROPDMGNUM ~ EVTYPE, stormData, sum)

names(economicImpact) = c("EVTYPE", "TOTALDAMAGE")

#economicImpact <- arrange(economicImpact, desc(economicImpact$TOTALDAMAGE), EVTYPE) [1:15, ]
economicImpact <- economicImpact[order(-economicImpact$TOTALDAMAGE), ][1:15, ]


economicImpact$EVTYPE <- factor(economicImpact$EVTYPE, levels = economicImpact$EVTYPE)

ggplot(economicImpact, aes(x = EVTYPE, y = TOTALDAMAGE)) + 
        geom_bar(stat = "identity", fill = "green") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Event Type") + ylab("Damages") + ggtitle("Number of property damage for the top 10 weather events")

