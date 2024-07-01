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

#pie chart below
print(sd(climateData$T, na.rm = TRUE))
vec1 <- seq(from = 1939, to = 2029 , 10)
listOfYears <- c("1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2029")
yearsCut <- cut(climateData$Year, breaks=vec1, labels = listOfYears)
tappedPP <- tapply(climateData$PP, yearsCut, sum, na.rm = TRUE)
totalP <- sum(climateData$PP, na.rm = TRUE)
PPPercentage <- c(tappedPP[1]/totalP, tappedPP[2]/totalP, tappedPP[3]/totalP, tappedPP[4]/totalP, tappedPP[5]/totalP, tappedPP[6]/totalP, tappedPP[7]/totalP, tappedPP[8]/totalP, tappedPP[9]/totalP)
Colors <- c("white", "antiquewhite4", "aliceblue", "antiquewhite3", "antiquewhite", "antiquewhite2", "burlywood4", "burlywood", "burlywood3")
combined <- paste(listOfYears, paste(round(PPPercentage*100, 2), "%"))


##################pie(PPPercentage,combined, col=Colors, main="Percipitation by decade")

#Box plot below
climateData$extreme <- c(climateData$SN+climateData$TS+climateData$TN+climateData$GR)
print(climateData)

small <- which(climateData$extreme == min(climateData$extreme))
big<- which(climateData$extreme == max(climateData$extreme))
###########################boxplot(climateData$extreme, ylab = "number of extreme weather", main="average amount of extreme weather")
#text(x=1, y = 7, labels = climateData$Year[small])
#text(x=1, y = 51, labels = climateData$Year[big])

#histogram below
hist_values<-hist(x = climateData$V, breaks=10, ylim = c(0, 30), xlab="Avg. windspeed", ylab="frequency", main="Histogram of windspeed frequency")
text(hist_values$mids,                                      # Add values of histogram on top of bars
     hist_values$counts,
     labels = hist_values$counts,
     adj = c(0.5, - 0.5))
#the 13-14 bin is the most frequent

#bargraph below
#ColorsBP<-c(rbg(255, 255, climateData$RA, 1))
print(rgb(1, 1, (climateData$RA/1000)*3))
#############################barplot(climateData$RA, ylim=c(0, 250), ylab="Rain days", xlab="year", main="Histogram of rain days to years", col=rgb(1, 1-(climateData$Year-1948)/74, (climateData$Year-1948)/74)) #col=rgb(1, (climateData$RA/1000)*4, (climateData$RA/1000)*4))
mtext(1948, 1, at =0)
mtext(2022, 1, at =81)

#scatter plot
print(climateData$Year)
#############################plot(y = climateData$T,x=climateData$Year,  xlab="years", ylab="temp",main="temperture throughout the years", ylim=c(0, 20), xlim=c(1948, 2022))

#Yes the annual temperature does seem to be increasing.

#You cannot say that there is a causation between the 
#years and the temperature as although the two seem corelated, 
#there are no facts that can back this up. This trend is most likely
#due to technology evolving through the years, causing more global polution, 
#leading to the increased temperatures.The two are corelated, but not causational.

#fizzbuzz

nums <- 1: 100

for(x in nums){
   s <- x
   if(x%%2 == 0){
      s = paste(s, "fizz", sep=" ")
   }
   if(x%%3 == 0){
      s = paste(s, "buzz", sep=" ")
   }
   cat(s, fill=TRUE)
}

#fibonacci sequence
fibonacci <- function(n){
   fibed <- c(0, 1)
   print(0);
   if(n>=2){
      print(1)
   }
   if(n>=3){
      count <- 3: n
      for(x in count){
         print(fibed[x-1] + fibed[x-2])
         fibed <- append(fibed, fibed[x-1] + fibed[x-2])
      }
   }
}

fibonacci(10)

#primes
prime <- function(n){
   primesvect <- c(2, 3, 5, 7)
   lim <- 7:n
   for(x in lim){
      printThis <- TRUE
      if(x%%2!=0 && x%%5!=0){
         for(n in primesvect){
            if(x%%n == 0){
               printThis <- FALSE
               break
            }
            if(n>sqrt(x)){
               break
            }
         }
         if(printThis){
            primesvect <- c(primesvect, x)
         }
      }
      #if((x!=2 && x%%2==0) || (x!=5 && x%%5==0) || rec(x)%%3==0){
      #   printThis<-FALSE
      #}
      

      #for(m in primesvect){
      #   if((sqrt(x)-m)<0){
      #      break
      #   }else if(sqrt(x)-m < close){
     #       close<-sqrt(x) 
      #   }
      #}

      #for(n in primesvect){
      #   if(n > close){
      #      break
      #   }else if(x%%n==0){
      #      printThis<-FALSE
      #   }
      #}

   }
   return(primesvect)
}

#rec <- function(n){
#   if(n/10 == 0){
#      return(as.integer(n))
#   }
#   return(as.integer((n%%10)+rec(n/10)))
#}

print("Primes")
timeStart <- Sys.time()
print(prime(10000))
timeEnd <- Sys.time()
print(timeEnd-timeStart)


#sorting
sort <- function(vect){
   lim <- 1:length(vect)
   for(x in lim){
      for(f in lim){
         if(vect[f]>vect[x]){
            temp <- vect[f]
            vect[f]<-vect[x]
            vect[x]<-temp
         }
      }
   }
   return(vect)
}

#print("sorting")
#print(sort(c(5, 2, 3, 1, 4)))

#Matrix transpose
transpose <- function(mat){
   row <- length(mat[1, ])
   col <- length(mat)/length(mat[1, ])
   rMat <- matrix(nrow=row, ncol=col)

   for(r in (1:row)){
      for(c in (1:col)){
         rMat[c, r]<- mat[c, r]
      }
   }
   return(rMat)
}

thisMat <- matrix(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), nrow=3, ncol=3)
#print(transpose(thisMat))