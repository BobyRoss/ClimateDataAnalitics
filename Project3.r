climateData <- read.csv("C:\\Users\\sougou.tojima\\Documents\\ClimateData.csv")
print(climateData)
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
climateData <- climateData[-c(19, 33, 67), ]
print(climateData)
