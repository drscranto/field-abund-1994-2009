library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(grid)
library(gridExtra)

getwd()
ccbug <- read.csv("~/Downloads/ccbug.csv", stringsAsFactors = FALSE)

#the columns are being read as characters
##if you didnt use 'stringsasfactors = false', convert the column into characters first and then into numeric

str(ccbug)

#converting the required columns into numeric form so as to make them available to plot 
ccbug[ ,5:11] <- sapply(ccbug[ ,5:11], as.numeric)

ccbug1 <- ccbug %>% gather( key = "mean",
                            value = "Densities",
                            madd, mn1d, mn2d, mn3d, mn4d, mn5d, mned) 
ccbug2 <- ccbug %>% gather( key = "mean",
                  value = "Densities",
                  madd, mn1d, mn2d, mn3d, mn4d, mn5d, mned) 

ccbug1[ ,13] <- as.numeric(ccbug1[ ,13])
ccbug2[ ,13] <- as.numeric(ccbug2[ ,13])

#Subsetting data to just contain "madd"
ccbug1 <- subset(ccbug1 , mean == "madd")
ccbug2 <- subset(ccbug2 , mean == "madd")
ccbug1$patch <- as.character(ccbug1$patch)
ccbug2$patch <- as.character(ccbug2$patch)
#Checking the number of entries
ccbug1 <- subset(ccbug1, Densities > 1)



#Function to separate the string in month and assigning a numeric value to each month
date2month <- function(xstr){
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  ans <- NA
  for (i in 1:12){
    if (months[i] %in% strsplit(xstr,"-")[[1]]){
      ans <- i
      break
    }
  }
  ans
}

ccbug1$month <- sapply(ccbug1$date,date2month)
ccbug2$month <- sapply(ccbug2$date, date2month)

ccbug1 <- ccbug1[c(1,3,13,14)]
ccbug2 <- ccbug2[c(1,3,13,14)]

my.year <- 1994
ccbug_1994 <- subset(ccbug1, year == 1994)


a <- as.numeric()
b <- c()
for(i in unique(ccbug_1994$month)){
  a <- max(ccbug_1994$Densities[ccbug_1994$month == i])
  b <- c(b,a)
}
b

plot1 <- ggplot() + geom_line( aes(x = unique(ccbug_1994$month),
                                   y = b)) + stat_smooth(se = FALSE)

my.year <- 1995
ccbug_1995 <- subset(ccbug1, year == 1995)


c <- as.numeric()
d <- c()
for(i in unique(ccbug_1995$month)){
  c <- max(ccbug_1995$Densities[ccbug_1995$month == i])
  d <- c(d,c)
}
d

plot2 <- ggplot() + geom_line( aes(x = unique(ccbug_1995$month),
                                   y = d)) + stat_smooth(se = FALSE)


my.year <- 1996
ccbug_1996 <- subset(ccbug1, year == 1996)

e <- as.numeric()
f <- c()
for(i in unique(ccbug_1996$month)){
  e <- max(ccbug_1996$Densities[ccbug_1996$month == i])
  f <- c(f,e)
}
f

plot3 <- ggplot() + geom_line( aes(x = unique(ccbug_1996$month),
                                   y = f)) + stat_smooth(se = FALSE)

my.year <- 1997
ccbug_1997 <- subset(ccbug1, year == 1997)

g <- as.numeric()
h <- c()
for(i in unique(ccbug_1997$month)){
  g <- max(ccbug_1997$Densities[ccbug_1997$month == i])
  h <- c(h,g)
}
h

plot4 <- ggplot() + geom_line( aes(x = unique(ccbug_1997$month),
                                   y = h)) + stat_smooth(se = FALSE)

my.year <- 1998
ccbug_1998 <- subset(ccbug1, year == 1998)

i <- as.numeric()
j <- c()
for(i in unique(ccbug_1998$month)){
  i <- max(ccbug_1998$Densities[ccbug_1998$month == i])
  j <- c(j,i)
}
j

plot5 <- ggplot() + geom_line( aes(x = unique(ccbug_1998$month),
                                   y = j)) + stat_smooth(se = FALSE)

my.year <- 1999
ccbug_1999 <- subset(ccbug1, year == 1999)

k <- as.numeric()
l <- c()
for(i in unique(ccbug_1999$month)){
  k <- max(ccbug_1999$Densities[ccbug_1999$month == i])
  l <- c(l,k)
}
l

plot6 <- ggplot() + geom_line( aes(x = unique(ccbug_1999$month),
                                   y = l)) + stat_smooth(se = FALSE)

my.year <- 2000
ccbug_2000 <- subset(ccbug2, year == 2000)
#ccbug_2000 <- subset(ccbug2, month == c(3,4,5,6,7,8,9))

m <- as.numeric()
n <- c()
for(i in unique(ccbug_2000$month)){
  m <- max(ccbug_2000$Densities[ccbug_2000$month == i])
  n <- c(n,m)
}
n
n[c(1,2)] <- NA
plot7 <- ggplot() + geom_line( aes(x = unique(ccbug_2000$month),
                                   y = n)) + stat_smooth(se = FALSE)

my.year <- 2001
ccbug_2001 <- subset(ccbug1, year == 2001)

o <- as.numeric()
p <- c()
for(i in unique(ccbug_2001$month)){
  o <- max(ccbug_2001$Densities[ccbug_2001$month == i])
  p <- c(p,o)
}
p

plot8 <- ggplot() + geom_line( aes(x = unique(ccbug_2001$month),
                                   y = p)) + stat_smooth(se = FALSE)

my.year <- 2002
ccbug_2002 <- subset(ccbug1, year == 2002)

q <- as.numeric()
r <- c()
for(i in unique(ccbug_2002$month)){
  q <- max(ccbug_2002$Densities[ccbug_2002$month == i])
  r <- c(r,q)
}
r

plot9 <- ggplot() + geom_line( aes(x = unique(ccbug_2002$month),
                                   y = r)) + stat_smooth(se = FALSE)

my.year <- 2003
ccbug_2003 <- subset(ccbug1, year == 2003)

s <- as.numeric()
t <- c()
for(i in unique(ccbug_2003$month)){
  s <- max(ccbug_2003$Densities[ccbug_2003$month == i])
  t <- c(t,s)
}
t

plot10 <- ggplot() + geom_line( aes(x = unique(ccbug_2003$month),
                                   y = t)) + stat_smooth(se = FALSE)

my.year <- 2004
ccbug_2004 <- subset(ccbug2, year == 2004)


u <- as.numeric()
v <- c()
for(i in unique(ccbug_2004$month)){
  u <- max(ccbug_2004$Densities[ccbug_2004$month == i])
  v <- c(v,u)
}
v

plot11 <- ggplot() + geom_line( aes(x = unique(ccbug_2004$month),
                                   y = v)) + stat_smooth(se = FALSE)

my.year <- 2005
ccbug_2005 <- subset(ccbug1, year == 2005)

w <- as.numeric()
x <- c()
for(i in unique(ccbug_2005$month)){
  w <- max(ccbug_2005$Densities[ccbug_2005$month == i])
  x <- c(x,w)
}
x

plot12 <- ggplot() + geom_line( aes(x = unique(ccbug_2005$month),
                                   y = x)) + stat_smooth(se = FALSE)

my.year <- 2006
ccbug_2006 <- subset(ccbug1, year == 2006)

y <- as.numeric()
z <- c()
for(i in unique(ccbug_2006$month)){
  y <- max(ccbug_2006$Densities[ccbug_2006$month == i])
  z <- c(z,y)
}
z

plot13 <- ggplot() + geom_line( aes(x = unique(ccbug_2006$month),
                                   y = z)) + stat_smooth(se = FALSE)

my.year <- 2007
ccbug_2007 <- subset(ccbug1, year == 2007)

ab <- as.numeric()
bc <- c()
for(i in unique(ccbug_2007$month)){
  ab <- max(ccbug_2007$Densities[ccbug_2007$month == i])
  bc <- c(bc,ab)
}
bc

plot14 <- ggplot() + geom_line( aes(x = unique(ccbug_2007$month),
                                   y = bc)) + stat_smooth(se = FALSE)

my.year <- 2008
ccbug_2008 <- subset(ccbug1, year == 2008)

cd <- as.numeric()
ef <- c()
for(i in unique(ccbug_2008$month)){
  cd <- max(ccbug_2008$Densities[ccbug_2008$month == i])
  ef <- c(ef,cd)
}
ef

plot15 <- ggplot() + geom_line( aes(x = unique(ccbug_2008$month),
                                   y = ef)) + stat_smooth(se = FALSE)

my.year <- 2009
ccbug_2009 <- subset(ccbug1, year == 2009)

gh <- as.numeric()
ij <- c()
for(i in unique(ccbug_2009$month)){
  gh <- max(ccbug_2009$Densities[ccbug_2009$month == i])
  ij <- c(ij,gh)
}
ij

plot16 <- ggplot() + geom_line( aes(x = unique(ccbug_2009$month),
                                   y = ij)) + stat_smooth(se = FALSE)


#Arranging the plots into one frame

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
             plot11, plot12, plot13, plot14, plot15, plot16, ncol = 3)

#I have tried to subset data by cleaning them individually (one reason as to why the code is so long).
#Barring 2009, 2007 (in which there isn't enough data to analyze trends), all other years seem to reach
#their max during the months 5,6, or 7 (mostly 6). This is interesting as July is generally considered to be the hottest month

#What next?