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

load(file.path("..","data","Teacher_Questionnaire.rData"))

Teacher01 <- Teacher01 %>% 
  select(respid, dstrctid,schoolid,dc_year,y1status,rsltdate, 
         InASP=tq1_46a, InAC=tq1_46b, InRnW=tq1_46c, InSFA=tq1_46d, InOther=tq1_46e,
         PlanImpr=tq1_47a, StepsSeq=tq1_47b, StepsClear=tq1_47c, ClearGoal=tq1_47d,
         ExWork  =tq1_47e, ExTeach =tq1_47f, ExCSR     =tq1_47g) 
Teacher01 <- lapply(Teacher01, function(x) {
  attr(x, "label") <- NULL
  attr(x, "class") <- grep("labelled",attr(x,"class") ,invert=T,value=T)
  x
} ) %>% as.data.frame
rm(Teacher02,Teacher03,Teacher04)

Specificity <- Teacher01 %>% select(PlanImpr,StepsSeq,StepsClear,ClearGoal)
summary(Clarity)

#' First graph of data 
Specificity %>% melt %>% 
  ggplot(aes(x=value,fill=variable)) + geom_bar(position = position_dodge() ) + coord_flip()

#' # Start of class analysis

#' First we standardize items to make items more parallel by equating total variance in items.
 
Specificity_Std <- apply(Specificity, 2, function(x) scale(x) %>% c)

describe(Specificity)
describe(Specificity_Std)

#' MIssing data makes means more useful than sums because it is more robust to item data that is missing.
VIM::aggr(Specificity)         

#' take mean of items into scale we store in variable specificity_M
Specificity_m <- apply(Specificity, 1, mean, na.rm=T)

#' correlation matrix
cor(Specificity, use="pair")
#' Alpha
alpha(Specificity)


#' look at means by programs

describe(Specificity_m[Teacher01$InSFA  ==1])
describe(Specificity_m[Teacher01$InAC   ==1])
describe(Specificity_m[Teacher01$InASP  ==1])
describe(Specificity_m[Teacher01$InOther==1])
