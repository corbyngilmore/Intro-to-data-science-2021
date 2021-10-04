#' Loading R libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(rio)
library(pander)
library(maps)
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
vignette("dplyr")
# filter allows you to select a subset of rows in a data frame
dat0 %>% filter(Confirmed == "1", Active == "1")
# arrange reorders selected rows
dat0 %>% arrange(Confirmed, Recovered)
# puts it in descending order
dat0 %>% arrange(desc(Lat))
# slice lets you index rows by their integer location
dat0 %>% slice(5:10)
# can use head or tail to start from top or bottom
dat0 %>% slice_head(n=4)
dat0 %>% slice_tail(n=10)
# slice sample randomly selects rows
dat0 %>% slice_sample(n=8)
# slice min or max select based on highest or lowest values
dat0 %>% slice_max(Confirmed, n=5)
# select lets you zoom in on useful subsets/ columns use ?select for more filters
dat0 %>% select(`Country/Region`, Confirmed, Recovered)
# can rename with select but rename command is better
dat0 %>% rename(Country = `Country/Region`)
# mutate adds new columns that are functions of old columns
dat0 %>% mutate(DeathRate = Deaths / Confirmed)
dat0 %>% 
  mutate(DeathRate = Deaths / Confirmed) %>% slice_tail(n=20)
# Transmute lets you keep the new variable
dat0 %>% transmute(DeathRate = Deaths / Confirmed)
# Relocate changes column order
dat0 %>% relocate(Confirmed:Active, .before = `Province/State`)
# Summarise collapses a data frame to a single row
dat0 %>% summarise(Confirmed = mean(Confirmed, na.rm = TRUE))
# can use pipe(%>%) to string multiple commands together
dat0 %>% group_by(`Country/Region`) %>% select(Confirmed:Deaths) %>% summarise( Confirmed = mean(Confirmed, na.rm = TRUE), Deaths = mean(Deaths, na.rm = TRUE))
#How to concatenate Strings
paste(month.name)    
paste(month.abb, month.name)
# sep allows you to add in things between the strings
paste(month.abb, "1", month.name, sep = ";")

paste(month.abb, month.name, collapse = "','")
# \n and \t add newline and tab respectively, theyre escape keys
# cat is useful to show messages on the screen, message is almost identical to cat except its easier to filter
paste(month.abb, month.name, collapse = "\t") %>% cat 
dat0<- mutate(dat0, Country = paste0(`Country/Region`,", ", paste0(`Country/Region`, ifelse(`Province/State` == "", "", paste(",", `Province/State`)))))


table(dat0$Country)
#dat0[,'FOO']
#dat0[['FOO']]
{length(letters)}
sqrt('a')
sqrt({'a'; 4})
sqrt({ls(all=T); 4})
sqrt({print(ls(all=T)); 4})

# ls()
# with(dat0,ls())
# with(dat0,browser())
# `Province/State` == ""
# ifelse(`Province/State` == "", "", paste(",", `Province/State`))
# paste0(`Country/Region`, ifelse(`Province/State` == "", "", paste(",", `Province/State`)))

#` GGPlot
ggplot(dat0, aes(x=Date,y=Confirmed))
ggplot(dat0, aes(x=Date,y=Confirmed, color=`Country/Region`, group=`Country`))+geom_line()+geom_line(aes(y=`Active`), lty=2)+scale_y_log10()
ggplot(dat0, aes(x=`Country/Region`, weight=`Confirmed`, fill=`Country/Region`, group=`Country`))+geom_bar(position="stack")+ guides(col = guide_legend(ncol = 1))+scale_y_log10()
ggplot(dat0, aes(x=`Lat`, y=`Long`, size=`Confirmed`))+geom_point()
map_data("world")$subregion %>%head

#new command
intersect(map_data("world")$subregion,dat0$`Province/State`)
dat1 <- group_by(dat0,`Country/Region`,Date) %>% select(c('Confirmed','Deaths','Recovered','Active')) %>% summarise(across(.funs=sum,na.rm=T),.groups = 'keep')

#ggplot2
