#' # Introduction
#' 
#' This document catalogues and archives the ways our class worked through using classical test theory (CTT) to make sense
#' of items 47: a-d on the Year 1 teacher survey from the study of Instructional Improvement.  

#' # loading DAta
knitr::opts_chunk$set(echo = FALSE)
require(tidyverse)
require(psych)
require(GPArotation)
require(reshape2)
source('C:/Users/mrkwht/Dropbox/Research/R libraries/DefaultScriptMark.R')

load(file.path("..","data","Teacher_Questionnaire.rData"))

Teacher01 <- Teacher01 %>% 
  select(respid, dstrctid,schoolid,y1status, 
         InASP=tq1_46a, InAC=tq1_46b, InRnW=tq1_46c, InSFA=tq1_46d, InOther=tq1_46e,
         PlanImpr=tq1_47a, StepsSeq=tq1_47b, StepsClear=tq1_47c, ClearGoal=tq1_47d,
         ExWork  =tq1_47e, ExTeach =tq1_47f, ExCSR     =tq1_47g,
         Prg_Exp =tq1_48b, Prg_Eff =tq1_48a, Prg_Value =tq1_48d, Prg_Change=tq1_48c
         ) 
Teacher01 <- lapply(Teacher01, function(x) {
  attr(x, "label") <- NULL
  attr(x, "class") <- grep("labelled",attr(x,"class") ,invert=T,value=T)
  x
} ) %>% as.data.frame
rm(Teacher02,Teacher03,Teacher04)

Vars <- Teacher01 %>% 
  select(PlanImpr,StepsSeq,StepsClear,ClearGoal,ExWork,ExTeach,ExCSR,Prg_Exp,Prg_Eff,Prg_Value,Prg_Change) 
#' First graph of data 
Vars[,1:4] %>%  melt %>% 
  ggplot(aes(x=value,fill=variable,group=variable,y=..prop..)) + geom_bar(position = position_dodge() ) + coord_flip()
Vars[,5:7] %>%  melt %>% 
  ggplot(aes(x=value,fill=variable,group=variable,y=..prop..)) + geom_bar(position = position_dodge() ) + coord_flip()
Vars[,8:11] %>%  melt %>% 
  ggplot(aes(x=value,fill=variable,group=variable,y=..prop..)) + geom_bar(position = position_dodge() ) + coord_flip()

#' Roughly 60% of responses are value 3.  Except for changes called for by my program, whcih has mean 2

#' # Start of class analysis

ParallelAnalysis(Vars)

#' We can start by running number of factors.

fa(Vars, nfactors=1:3 )





