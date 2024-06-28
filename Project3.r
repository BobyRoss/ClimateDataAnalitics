climateData <- read.csv("C:\\Users\\sougou.tojima\\Documents\\ClimateData.csv")
print(climateData)
print(summary(climateData))
#Printing a data will just print the whole excel spreadsheet on the terminal.
#Printing a summary prints all the important data informations such as the min/max or the mean/medians.

#FG represets the number of foggy days.

#empty values are represented as NA in the list.
i <- 0
for(isna in climateData) {
   i<-i+1
}
print(i);