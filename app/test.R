library(DT)
library(dplyr)

library(devtools)
# install_github("esm-ispm-unibe-ch/reportingbias")
library(reportingbias)
# install_github("esm-ispm-unibe-ch/flow_contribution")
library(contribution)
library(netmeta)
require(tidyverse)

rm(list=ls())
dataraw <- read.csv("../griselda.csv", header = TRUE)
alldata <- as.data.frame(dataraw)
      alldata <- alldata %>%
        mutate(n = as.integer(n) ) %>%
         mutate(r = as.integer(r) )
directslong <- alldata %>%
  filter(!is.na(r))
otherOutcomeslong <- alldata %>%
  filter(is.na(r))

treatments <- unique(directslong$t) %>% sort()

  combinations <- t(combn(treatments,2))
  View(combinations)
  allcomparisons <- as.tibble(combinations) %>% 
  mutate(comparison =
           mapply(function(x,y){comp = sort(c(x,y)); return(paste(comp[1],comp[2],sep=":"))}, V1, V2)
  ) %>%
  rename(treat1=V1) %>% rename(treat2=V2) %>%
  select(comparison,treat1,treat2)

getTable1Rows <- function(longdata){
  pw <- as.tibble(pairwise(treat=t, event=r, n=n, data = longdata , studlab = id)) %>% 
    mutate(comparison =
             mapply(function(x,y){comp = sort(c(x,y)); return(paste(comp[1],comp[2],sep=":"))}, treat1, treat2)
    )
  dirspw = pw %>% group_by(comparison) %>% summarise(ns=length(study),n=n1+n2) %>%
    group_by(comparison,ns) %>% summarise(n = sum(n))
  return(dirspw) 
}
groupALabel = "Group A: 
observed for this outcome" 
groupBLabel = "Group B:
observed for other outcomes
"
groupCLabel = "Group C:
Unobserved"

groupAcolumn1 = getTable1Rows(directslong) 
groupAcolumn1 = groupAcolumn1 %>% rename(numstudies=ns, samplesize=n)

allDirectComparisons = getTable1Rows(alldata) %>% rename(total.numstudies=ns, total.samplesize=n)
groupA = groupAcolumn1 %>% left_join(allDirectComparisons, by="comparison") %>% mutate(group=groupALabel)

groupB = allDirectComparisons %>% anti_join(groupAcolumn1, by="comparison") %>% 
  filter(comparison %in% allcomparisons$comparison) %>% mutate(group=groupBLabel)
# rename(numstudies=total.numstudies, samplesize=total.samplesize)

groupC <- allcomparisons %>% select(comparison) %>%
  anti_join(groupA, by="comparison") %>%
  anti_join(groupB, by="comparison") %>% mutate(group=groupCLabel)


table1DropDown = function(column,treat1,treat2,group){
  choices = c()
  if(column == "known_uknowns"){
    if(group != groupCLabel){
      choices = c("Undetected bias"
             ,paste("Suspected bias favouring ",treat1,sep="")
             ,paste("Suspected bias favouring ",treat2,sep="")
             ,"Unclear")
    }else{
      choices = c()
    }
  }else{
    choices = c("Undetected bias"
           ,paste("Suspected bias favouring ",treat1,sep="")
           ,paste("Suspected bias favouring ",treat2,sep="")
           )
  }
  if(length(choices)>0){
        res = as.character(selectInput(paste0("sel_",column,treat1,treat2,sep=":"), ""
                                       , choices = choices
                                       , selected = NULL
                                       , selectize = T
                                       ))
  }else{
    res=""
  }
return(res)
}

table1 <- rbind(groupA,groupB,groupC)%>% left_join(allcomparisons, by="comparison") %>%
  mutate(known_unknowns = table1DropDown("known_uknowns",treat1,treat2,group)) %>%
  mutate(unknown_unknowns = table1DropDown("unknown_uknowns",treat1,treat2,group)) %>%
  mutate(overall_bias = table1DropDown("overall_bias",treat1,treat2,group)) %>%
  mutate(column1= paste(numstudies," (",samplesize,")",sep="")) %>%
  mutate(column2= paste(total.numstudies," (",total.samplesize,")",sep="")) %>%
  select(comparison,
         group,
         column1,
         column2,
         known_unknowns,
         unknown_unknowns,
         overall_bias
  )

table1Header = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, '-'),
      th(rowspan = 2, 'Comparisons'),
      th(rowspan = 2, 'group'),
      th(colspan = 2, 'Number of studies in each comparison'),
      th(colspan = 1, 'known unknowns'),
      th(colspan = 1, 'unknowns unknowns'),
      th(colspan = 1, 'overall bias'),
    ),tr(
      th(colspan = 1, 'Reporting this outcome (sample size)'),
      th(colspan = 1, 'Total identified in the SR (total sample size)'),
      th(colspan = 1, 'Classification system (e.g. ORBIT)'),
      th(colspan = 1, 'Qualitative signals and quantitative considerations'),
      th(colspan = 1, 'Algorithm for merging of previous assessments')
    )
  )
))

DT::datatable(table1,
              container = table1Header,
              escape = F,
              extensions = c('RowGroup', 'Buttons'),
              options = list(rowGroup = list(dataSrc = 2)
                             , paging = F
                             , columnDefs = list(list(visible=FALSE, targets=c(2)))
                             , dom = 'Bfrtip'
                             , buttons = c('copy', 'csv', 'excel')
              ),
              # callback = callback_js,
              selection = 'none' )

