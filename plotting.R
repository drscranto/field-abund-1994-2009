setwd("Dropbox/Office/Mentoring/Ashwin")

library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)


rm(list=ls())

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

ccbug <- read.csv("ccbug_94-09_patch-density_output_FINAL.csv", stringsAsFactors = FALSE)

ccbug[ ,5:11] <- sapply(ccbug[ ,5:11],as.numeric)

ccbug$month <- sapply(ccbug$date,date2month)
ccbug.1994 <- ccbug[ccbug$year==1994,]







