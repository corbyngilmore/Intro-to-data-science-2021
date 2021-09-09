#' Loading R libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(rio)
library(pander)
#' Create data set
dat0 <- import("covid19cleanedamericas.csv", stringsAsFactors=F);
#' base R scatterplot matrix
select(dat0,-'Country/Region') %>% plot()
#' show first 10 lines of dat0
head(dat0) %>% pander(split.tables=Inf)
#' gg plot scatterplot matrix
select(dat0,-'Country/Region') %>% ggpairs()
#' how to assign value to object and variable
foo <- 3
#' How to select columns 2 through 5:
dat0[,c(2:5)] %>% head %>% pander(split.tables=Inf)
#' How to select columns by name:
dat0[,c('Long','Confirmed','Date')] %>% head %>% pander(split.tables=Inf)
#' This is equivalent to...

head(dat0[,c('Long','Confirmed','Date')])%>% pander(split.tables=Inf)
#' To see what type each column is
sapply(dat0, class) %>% pander(split.tables=Inf)
#' Selecting rows instead of columns:
#'
#' The old fashioned (base R) way
dat0[c(1,3,9),] %>% head %>% pander(split.tables=Inf)
#' The subset() command (also base R)
subset(dat0,Lat<1&Active>0)
#' now using OR and equals
subset(dat0,(Lat<1&Active>0)|`Country/Region`=="Brazil") %>% pander(split.tables=Inf)
#' using OR and excluding
subset(dat0,(Lat<1&Active>0)|`Country/Region`!="Brazil") %>% pander(split.tables=Inf)
#' Including 
subset(dat0,(Lat<1&Active>0)|`Country/Region` %in% c("Chile", "Peru")) %>% pander(split.tables=Inf)
#' Exclusion of "in" operator
subset(dat0,(Lat<1&Active>0)|!`Country/Region` %in% c("Chile", "Peru")) %>% pander(split.tables=Inf)
# vignette('dplyr')(tutorial)