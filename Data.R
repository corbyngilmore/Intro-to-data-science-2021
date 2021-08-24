#' Loading R libraries
library(ggplot2)
library(dplyr)
library(GGally)
#' Create data set
dat0 <- survival::veteran;
#' base R scatterplot matrix
plot(dat0)
#' show first 10 lines of dat0
head(dat0)
#' gg plot scatterplot matrix
ggpairs(dat0)
#' how to assign value to object and variable
foo <- 3
