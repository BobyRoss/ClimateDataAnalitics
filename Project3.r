climateData <- read.csv("C:\\Users\\sougou.tojima\\Documents\\ClimateData.csv")
#print(summary(climateData))
#Printing a data will just print the whole excel spreadsheet on the terminal.
#Printing a summary prints all the important data informations such as the min/max or the mean/medians.

#FG represets the number of foggy days.

#empty values are represented as NA in the list.
i <- 0
for(isna in climateData) {
   i<-i+1
}
print(i);

#In total, there are 12 "NA"s in the data

#a)
#animals[2] <- "german shepard"
animals <- c("cat", "dog", "giraffe", "bunny")
animals[2] <- "german shepard"
#cat(animals)
#b)
#They represent the columns (the vertical part)

yearTF <- duplicated(climateData$"Year")
print(which(yearTF == TRUE))
climateData <- climateData[-c(which(yearTF == TRUE)), ]

climateData <- climateData[-c(which(climateData$Year == 2013)), ]
climateData <- climateData[-c(which(climateData$Year == 1962)), ]
climateData <- climateData[-c(which(climateData$Year == 1979)), ]

print(summary(climateData))
#     Mean  Median
#Year 1986  1987
#T    11.45 11.00
#TM   16.26 16.30
#Tm   6.725 6.7
#PP  1021.1 1029.3
#V   13.7   13.4
#RA  189.0  187.3
#SN  16.16  14.00
#TS  6.239    5
#FG  112.4  146.0
#TN  0.01493  0
#GR  0.806    1

#The standard deviation for T is NA because its data includes an NA, 
#and so when we compute it, we will be doing math with an NA (a non-number).
#since we cant do any mathematical operations on non-numbers, it will result in an NA.
print(sd(climateData$T, na.rm = TRUE))