#'---
#' title: "Model Selection"
#' author: 'Author One ^1^, Author Two ^1^'
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
#' # Initialization
#' 
#' Loading R libraries
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# Intro ----
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);
library(ggplot2)
library(dplyr)
library(GGally)
library(rio)
library(pander)
library(printr)
options(max.print=42)
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);

# Multy Variant Data Sets ----
#' Dat1
#'
dat1 <- import('https://www.ahrq.gov/sites/default/files/wysiwyg/sdohchallenge/data/SDOH_ZCTA_2011.xlsx');
head(dat1) %>% pander()
outcomes <- import("data/SIM_ZCTA_outcomes.csv")
Zipdat <- left_join(dat1, outcomes)
Gotcha <- sapply(Zipdat, function(xx){na.omit(xx)%>%unique%>%length()})>1
Zipdat <- Zipdat[,Gotcha]
Zipdat$sample <- sample(c("Train", "Test"), nrow(Zipdat), replace = TRUE, prob = c(0.05, 0.95))
Ziptrain <- subset(Zipdat, sample=="Train") %>% select(-c("sample", "REGION", "STATE"))
Gotcha2 <- sapply(Ziptrain, function(xx){na.omit(xx)%>%unique%>%length()})>1
names(Gotcha2)[!Gotcha2]
Gotcha3 <- !sapply(Ziptrain, is.numeric)
names(Gotcha3)[Gotcha3] 
select(Ziptrain, c("STATE", "REGION")) %>% table()
table(Gotcha3) 
#+ TrainTest
# unique(Ziptrain$sample)
# table(Ziptrain$sample)
# table(Zipdat$sample)

#' Fitting the intial linear model
#+ Lm
POPDat <- lm("Outcome~ACS_TOTAL_POP_WT + CEN_POPDENSITY_2010 + CEN_AREALAND_SQM", Ziptrain)
summary(POPDat)%>% pander
#+ Step
POPall <- lm(Outcome~., Ziptrain)
POPStep <- step(POPDat, scope = list(lower = ~1, upper = POPall), k = log(nrow(Ziptrain)))
