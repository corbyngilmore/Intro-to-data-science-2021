library(ggplot2)
library(dplyr)
library(GGally)
dat0 <- survival::veteran;
plot(dat0)
head(dat0)
pairs(dat0)
ggpairs(dat0)
foo <- 3
