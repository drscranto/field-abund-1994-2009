library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
getwd()
ccbug <- read.csv("~/Downloads/ccbug.csv", stringsAsFactors = FALSE)

#the columns are being read as characters
##if you didnt use 'stringsasfactors = false', convert the column into characters first and then into numeric

#converting the required columns into numeric form so as to make them available to plot 
ccbug[ ,5] <- as.numeric(ccbug[ ,5])
#head(ccbug)

ccbug[ ,6] <- as.numeric(ccbug[ ,6])
#head(ccbug)

ccbug[ ,7] <- as.numeric(ccbug[ ,7])
#head(ccbug)

ccbug[ ,8] <- as.numeric(ccbug[ ,8])
#head(ccbug)

ccbug[ ,9] <- as.numeric(ccbug[ ,9])
#head(ccbug)

ccbug[ ,10] <- as.numeric(ccbug[ ,10])
#head(ccbug)

ccbug[ ,11] <- as.numeric(ccbug[ ,11])
head(ccbug)

#ccbug1994 <- ccbug[ccbug$year == 1995]
ccbug1994 <- subset(ccbug, year == 1994)

##Assigning a numeric value to each month 
ccbug1994$month <- c(rep(5,18), 
                     rep(6,18), 
                     rep(7,18),
                     rep(8,18),
                     rep(9,18),
                     rep(10,18),
                     rep(11,18),
                     rep(12,18))
#We used this as the dates are convinently split in the data as such. If that wasnt the case,
#We will have to create a function that uses 'if( "May" %in% strsplit(ccbug1994$date,"-") = TRUE, replace with 5 and so on'
View(ccbug1994)

plot(ccbug1994$month, ccbug1994$mned)
points(ccbug1994$month, ccbug1994$mn1d)

#Just using boxplot
#x <- cbind(ccbug1994$month,ccbug1994$month,ccbug1994$month,ccbug1994$month,ccbug1994$month,ccbug1994$month)
#y <- cbind(ccbug1994$mned, ccbug1994$mn1d, ccbug1994$mn2d,ccbug1994$mn3d, ccbug1994$mn4d, ccbug1994$mn5d)

#boxplot(log(y)~x)


ccbugnew <- ccbug1994 %>% gather( key = "mean",
                      value = "Densities",
                      madd, mn1d, mn2d, mn3d, mn4d, mn5d, mned) 

ccbugnew[ ,14] <- as.numeric(ccbugnew[ ,14])
                      
ccbugnew <- subset(ccbugnew, Densities < 5)

ccbugnew %>% ggplot() + geom_boxplot(aes( x = month, y = Densities, group = month)) + facet_wrap(~mean,
                                                                                                 ncol = 9
                                                                                                 )

ccbugnew %>% ggplot(aes(x = month,
                        y = Densities,
                        group = 1)) + geom_point()+ geom_line() + facet_wrap(~mean, ncol = 7)

str(ccbugnew)

typeof(ccbugnew$Densities)
typeof(ccbugnew$month)





