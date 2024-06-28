climateData <- read.csv("C:\\Users\\sougou.tojima\\Documents\\ClimateData.csv")
graphics.off()
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

climateData <- climateData[-c(which(climateData$Year == 1962)), ]
climateData <- climateData[-c(which(climateData$Year == 1979)), ]
climateData <- climateData[-c(which(climateData$Year == 2002)), ]
climateData <- climateData[-c(which(climateData$Year == 2005)), ]

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

#the 70's had little precipitation because the percipitation amounts were not recorded in the data.
#most of the data for the 70's were NA, and so got skipped, resulting in low numbers.


print(sd(climateData$T, na.rm = TRUE))
vec1 <- seq(from = 1939, to = 2029 , 10)
listOfYears <- c("1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2029")
yearsCut <- cut(climateData$Year, breaks=vec1, labels = listOfYears)
tappedPP <- tapply(climateData$PP, yearsCut, sum, na.rm = TRUE)
totalP <- sum(climateData$PP, na.rm = TRUE)
PPPercentage <- c(tappedPP[1]/totalP, tappedPP[2]/totalP, tappedPP[3]/totalP, tappedPP[4]/totalP, tappedPP[5]/totalP, tappedPP[6]/totalP, tappedPP[7]/totalP, tappedPP[8]/totalP, tappedPP[9]/totalP)
Colors <- c("white", "antiquewhite4", "aliceblue", "antiquewhite3", "antiquewhite", "antiquewhite2", "burlywood4", "burlywood", "burlywood3")
combined <- paste(listOfYears, paste(round(PPPercentage*100, 2), "%"))


pie(PPPercentage,combined, col=Colors, main="Percipitation by decade")

dev.new()
climateData$extreme <- c(climateData$SN+climateData$TS+climateData$TN+climateData$GR)
print(climateData)

boxplot(climateData$extreme)
small <- which(climateData$extreme == min(climateData$extreme))
big<- which(climateData$extreme == max(climateData$extreme))
boxplot(climateData$extreme)
text(x=1, y = 7, labels = climateData$Year[small])
text(x=1, y = 51, labels = climateData$Year[big])