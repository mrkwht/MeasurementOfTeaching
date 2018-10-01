knitr::opts_chunk$set(echo = FALSE)
require(tidyverse)
require(psych)
require(GPArotation)
require(reshape2)

load(file.path("data","Teacher_Questionnaire.rData"))

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

summary(Teacher01)

## Graph of Variable Distributions
Teacher01 %>% select(PlanImpr,StepsSeq,StepsClear,ClearGoal,
                     ExWork  ,ExTeach, ExCSR) %>%
  melt %>% 
  ggplot(aes(x=value,fill=variable)) + geom_bar(position = position_dodge() ) + coord_flip()
Teacher01 %>% select(ExWork  ,ExTeach, ExCSR) %>% melt %>% 
  ggplot(aes(x=value,fill=variable)) + geom_bar(position = position_dodge() ) + coord_flip()
Teacher01 %>% select(PlanImpr,StepsSeq,StepsClear,ClearGoal) %>% melt %>% 
  ggplot(aes(x=value,fill=variable)) + geom_bar(position = position_dodge() ) + coord_flip()

## 



