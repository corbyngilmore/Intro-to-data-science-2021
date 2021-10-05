#'---
#' title: "[TRIPOD 1] Identify the study as developing and/or validating a multivariable prediction model, the target population, and the outcome to be predicted."
#' author: 'Author One ^1,✉^, Author Two ^1^'
#' abstract: |
#'  | [TRIPOD 2] Provide a summary of objectives, study design, setting, participants, sample size, predictors, outcome, statistical analysis, results, and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' css: production.css
#' output:
#'   html_document:
#'    code_folding: hide
#'    toc: true
#'    toc_float: true
#' ---
#'
#' Loading R libraries
#+ init, echo=FALSE, message=FALSE, warning=FALSE
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, max.print=42);
library(ggplot2)
library(dplyr)
library(GGally)
library(rio)
library(pander)
library(maps)
library(printr)
options(max.print=42)
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
#' Create data set
dat0 <- import("covid19cleanedamericas.csv", stringsAsFactors=F);
#' base R scatterplot matrix
#+ scatterplot, cache=TRUE
select(dat0,-'Country/Region') %>% plot()
#' show first 10 lines of dat0
head(dat0) 
#' gg plot scatterplot matrix
#+ ggplot_splom, cache=TRUE
select(dat0,-'Country/Region') %>% ggpairs()
#' how to assign value to object and variable
foo <- 3
#' How to select columns 2 through 5:
dat0[,c(2:5)] %>% head
#' How to select columns by name:
dat0[,c('Long','Confirmed','Date')] %>% head 
#' This is equivalent to...

head(dat0[,c('Long','Confirmed','Date')])
#' To see what type each column is
sapply(dat0, class) %>% pander
#' Selecting rows instead of columns:
#'
#' The old fashioned (base R) way
dat0[c(1,3,9),] %>% head()
#' The subset() command (also base R)
subset(dat0,Lat<1&Active>0) %>% head()
#' now using OR and equals
subset(dat0,(Lat<1&Active>0)|`Country/Region`=="Brazil") %>% head()
#' using OR and excluding
subset(dat0,(Lat<1&Active>0)|`Country/Region`!="Brazil") %>% head()
#' Including 
subset(dat0,(Lat<1&Active>0)|`Country/Region` %in% c("Chile", "Peru")) %>% head()
#' Exclusion of "in" operator
subset(dat0,(Lat<1&Active>0)|!`Country/Region` %in% c("Chile", "Peru")) %>% head()
# vignette('dplyr')(tutorial)
#vignette("dplyr")
#' filter allows you to select a subset of rows in a data frame
dat0 %>% filter(Confirmed == "1", Active == "1") %>% head
#' arrange reorders selected rows
dat0 %>% arrange(Confirmed, Recovered) %>% head
#' puts it in descending order
dat0 %>% arrange(desc(Lat)) %>% head
#' `slice` lets you index rows by their integer location
dat0 %>% slice(5:10) 
#' can use `head` or `tail` to start from top or bottom
dat0 %>% slice_head(n=4) 
dat0 %>% slice_tail(n=10)
#' `slice_sample` randomly selects rows
dat0 %>% slice_sample(n=8)
#' `slice_min` or `slice_max` select based on highest or lowest values
dat0 %>% slice_max(Confirmed, n=5)
#' `select` lets you zoom in on useful subsets/ columns use ?select for more filters
dat0 %>% select(`Country/Region`, Confirmed, Recovered) %>% head
#' can rename with `select` but `rename` command is better
dat0 %>% rename(Country = `Country/Region`) %>% head
#' `mutate` adds new columns that are functions of old columns
dat0 %>% mutate(DeathRate = Deaths / Confirmed) %>% head
dat0 %>% 
  mutate(DeathRate = Deaths / Confirmed) %>% slice_tail(n=20)
#' `transmute` lets you keep the new variable
dat0 %>% transmute(DeathRate = Deaths / Confirmed) %>% head
#' `relocate` changes column order
dat0 %>% relocate(Confirmed:Active, .before = `Province/State`) %>% head
#' `summarise` collapses a data frame to a single row
dat0 %>% summarise(Confirmed = mean(Confirmed, na.rm = TRUE))
#' can use pipe (`%>%`) to string multiple commands together
dat0 %>% group_by(`Country/Region`) %>% select(Confirmed:Deaths) %>% 
  summarise( Confirmed = mean(Confirmed, na.rm = TRUE)
             , Deaths = mean(Deaths, na.rm = TRUE)) %>% head
#' How to concatenate Strings
paste(month.name) %>% pander
paste(month.abb, month.name) %>% pander
#' sep allows you to add in things between the strings
paste(month.abb, "1", month.name, sep = ";") %>% pander

paste(month.abb, month.name, collapse = "','") %>% pander
#' `\n` and `\t` add newline and tab respectively, theyre escape keys
#' `cat` is useful to show messages on the screen, `message` is almost identical 
#' to `cat` except its easier to filter
paste(month.abb, month.name, collapse = "\t") %>% cat 

#' Create a new column called `Country` out of `Country/Region` and 
#' `Province/State` then count up the frequency of each value:
#' 
dat0<- mutate(dat0, Country = paste0(`Country/Region`,", ", paste0(`Country/Region`, ifelse(`Province/State` == "", "", paste(",", `Province/State`)))))
table(dat0$Country) %>% cbind

#dat0[,'FOO']
#dat0[['FOO']]
# {length(letters)}
# sqrt('a')
# sqrt({'a'; 4})
# sqrt({ls(all=T); 4})
# sqrt({print(ls(all=T)); 4})

# ls()
# with(dat0,ls())
# with(dat0,browser())
# `Province/State` == ""
# ifelse(`Province/State` == "", "", paste(",", `Province/State`))
# paste0(`Country/Region`, ifelse(`Province/State` == "", "", paste(",", `Province/State`)))

#' GGPlot
#+ ggplot1, cache=TRUE
ggplot(dat0, aes(x=Date,y=Confirmed))
ggplot(dat0, aes(x=Date,y=Confirmed, color=`Country/Region`, group=`Country`))+geom_line()+geom_line(aes(y=`Active`), lty=2)+scale_y_log10()
ggplot(dat0, aes(x=`Country/Region`, weight=`Confirmed`, fill=`Country/Region`, group=`Country`))+geom_bar(position="stack")+ guides(col = guide_legend(ncol = 1))+scale_y_log10()
ggplot(dat0, aes(x=`Lat`, y=`Long`, size=`Confirmed`))+geom_point()
map_data("world")$subregion %>%head 

#' new command
intersect(map_data("world")$subregion,dat0$`Province/State`)
dat1 <- group_by(dat0,`Country/Region`,Date) %>% select(c('Confirmed','Deaths','Recovered','Active')) %>% summarise(across(.funs=sum,na.rm=T),.groups = 'keep')
