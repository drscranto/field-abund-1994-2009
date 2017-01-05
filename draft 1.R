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

ccbug1[ ,13] <- as.numeric(ccbug1[ ,13])
ccbug1 <- subset(ccbug1, Densities < 20)

#Subsetting data to just contain "madd"
ccbug1 <- subset(ccbug1 , mean == "madd")

#Checking the number of entries
#ccbug1$Densities[ccbug1$Densities < 20]


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

#######################Plotting for all patches to find out if the pattern for madd changes over years

my.year <- 1994
ccbug_my.year <- subset(ccbug1, year == my.year)

plot1 <- ggplot( ccbug_my.year, aes (x = month,
                     y = Densities,
                     group = patch,
                     colour = factor(patch)
                     )) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (1994)")

my.year <- 1995
ccbug_my.year <- subset(ccbug1, year == my.year)

plot2 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (1995)")

my.year <- 1996
ccbug_my.year <- subset(ccbug1, year == my.year)

plot3 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE)  +
  xlab("months (1996)")

my.year <- 1997
ccbug_my.year <- subset(ccbug1, year == my.year)

plot4 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (1997)")

my.year <- 1998
ccbug_my.year <- subset(ccbug1, year == my.year)

plot5 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (1998)")

my.year <- 1999
ccbug_my.year <- subset(ccbug1, year == my.year)

plot6 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (1999)")

my.year <- 2000
ccbug_my.year <- subset(ccbug1, year == my.year)

plot7 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2000)")

my.year <- 2001
ccbug_my.year <- subset(ccbug1, year == my.year)

plot8 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2001)")

my.year <- 2002
ccbug_my.year <- subset(ccbug1, year == my.year)

plot9 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2002)")

my.year <- 2003
ccbug_my.year <- subset(ccbug1, year == my.year)

plot10 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2003)")

my.year <- 2004
ccbug_my.year <- subset(ccbug1, year == my.year)

plot11 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2004)")

my.year <- 2005
ccbug_my.year <- subset(ccbug1, year == my.year)

plot12 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2005)")

my.year <- 2006
ccbug_my.year <- subset(ccbug1, year == my.year)

plot13 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2006)")

my.year <- 2007
ccbug_my.year <- subset(ccbug1, year == my.year)

plot14 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2007)")

my.year <- 2008
ccbug_my.year <- subset(ccbug1, year == my.year)

plot15 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2008)")

my.year <- 2009
ccbug_my.year <- subset(ccbug1, year == my.year)

plot16 <- ggplot( ccbug_my.year, aes (x = month,
                                     y = Densities,
                                     group = patch,
                                     colour = factor(patch)
)) + 
  geom_point(size = 0.0001) + 
  stat_smooth(se = FALSE) +
  xlab("months (2009)")

####################Attempt to create a function that simplifies the plot making process
#i <- as.numeric()
#plot <- c()
#for( i in unique(ccbug1$year)){


##KATIE: TRY USING assign()
## as a test say: i <- 1; x1 <- 4; assign(paste0("x",i),7); x1


#plot[i] <- ggplot( subset(ccbug1, year == i), aes (x = month,
#                                       y = Densities,
#                                       group = patch,
#                                       colour = factor(patch)
#                                       )) + 
#                     geom_point(size = 0.0001) + 
#                     stat_smooth(se = FALSE) 
# plot <- c(plot, plot[i])
#}
#plot
######################################################################################################

#Arranging the plots into one frame

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
             plot11, plot12, plot13, plot14, plot15, plot16, ncol = 3)



#Metrics:
i <- as.numeric()
b <- as.numeric()
n <- c(unique(ccbug_my.year$patch))
x <- c()
y <- c()
for(i in n) {
  b <- mean(ccbug_my.year$Densities[ccbug_my.year$patch == i]) 
  c <- median(ccbug_my.year$Densities[ccbug_my.year$patch == i])
  x <- c(x, b)
  y <- c(y, c)
}
x
y
mean(x)
median(y)

#Looking at the graphs, one specific patch always seems to resemble the graph that we're looking for.
#Am not sure about what to do next! Should I be focussing on each patch or that one patch thats got a clear curve?
#Should I further divide each year into specific patches?
#Am I doing it right with the metrics?

##KATIE:
# The plots look great. Yes, you are calculating the metrics correctly, x ends up being a vector of all of the patch means for one year.
# It's good to keep an eye on the one patch that seems to have a clear pattern but lets stick with all of the patches for right now. Sometimes the visuals can be tricky and patterns are sometimes revealed only by the statistics.
# Its always good to take a step back and look at the bigger question. We are interested in the way the pattern of adult density (or a metric representing it in someway) is changing over time. So to do that we need a dataset of the metric for each patch in each year. Then we need to estimate the relationship of the metric ~ year, after accounting for random differences between patches.
# So we need a matrix (or data.frame) with a column for patch, one for year, and a column for each metric. So maybe instead of saving just b, you can save (c(i, b)) and loop through all the the ccbug_my.year data.frames? Or you could start with the large data set and have two nested loops, one for year, then one for patch? If you initialize a matrix before the loop (instead of c()) then you can use rbind() to add rows to it as you loop.
# After we make the data set we can use a mixed-effects regression (lmer in package lme4) to estimate the strength of the relationship of density and year






