source("C:/Users/mrkwht/Dropbox/Research/R libraries/DefaultScriptMark.R")
require(data.table,quietly=T,warn.conflicts=F)
require(gdata     ,quietly=T,warn.conflicts=F)
require(FactoMineR,quietly=T,warn.conflicts=F)
library(Hmisc     ,quietly=T,warn.conflicts=F)
require(fpc       ,quietly=T,warn.conflicts=F)
require(clValid   ,quietly=T,warn.conflicts=F)

## Setup ####
odir<-"O:/Education and Well Being/Projects/NCEE BES Evaluation Study/15 Data Collection & Analysis/03 Analytic Files/Student Level/Y2 Student Survey File"
data<-read.csv(file.path(odir,"y2StudSurvey_StateData_11012013.csv"))
data<-data[,!(names(data) %in% matchcols(data,"^QC|^KY|^CAM|^MS|^AZ"))]
data<-data[,!(names(data) %in% c("LastName","FirstName","St_DOB","St_SPED_Label","Grade_in_sy1213","YumaRandExam","SSurvey_GradPlansText"))]
data<-data.table(data)
setnames(data,names(data),sub("^SSurvey_","",names(data)))
summary(data)
data[,':='(Control2=4-Control2,
           Control3=4-Control3,
           Clarify3=4-Clarify3,
           Control4=4-Control4,
           Captivate1=4-Captivate1)]
data[,St_Gender:=Factor(St_Gender)]
data[,St_FRL:=Factor(St_FRL)]
data[,St_SPED:=Factor(St_SPED)]
data[,St_ELL:=Factor(St_ELL)]
data<-data[!is.na(ProgKnow_RetakeExams),]
Tri<-matchcols(data,"^Care|^Consolidate|^Challenge|^Control|^Captivate|^Clarify|^Confer")
TriC<-matchcols(data,"^Control")
TriO<-matchcols(data,"^Care|^Consolidate|^Challenge|^Captivate|^Clarify|^Confer")
TriH<-c("Confer5","Care1","Care2","Challenge6","Consolidate4")
data[,Tri:=apply(.SD,1,mean,na.rm=T),.SDcols=Tri]

## Cluster Vars ####
plot(clust <- varclus(data.matrix(data)[,Tri]))

# Cluster - Too Many variables
# cl1<-clValid::clValid(na.omit(data.matrix(data)[,c(TriC,TriO)]),2:6,
#                       c("hierarchical","diana","fanny","model","sota","pam"),
#                       c("internal", "stability"),method="ward",verbose=T)
# summary(cl1)
# plot(cl1)

require(Factoshiny)
PCAshiny(na.omit(data.frame(data)[,Tri]))
PCAshiny(na.omit(data.frame(data)[,TriC]))
PCAshiny(na.omit(data.frame(data)[,TriO]))
# HCPCshiny((  data.frame(data)[,Tri] ))

require(mclust)
clus<-Mclust(na.omit(data.frame(data)[,Tri]),G=1:5)
plot(clus,what="BIC")



# Code from elsewhere ####
ParallelAnalysis(as.data.frame(data)[,c(Vars$Infl)])
fact<-fa(as.data.frame(data)[,Tri],nfactors=2,covar=F,rotate="promax",fm="minres",impute="median")


factO<-fa(as.data.frame(data)[,TriO],nfactors=2,covar=F,rotate="promax",fm="minres",impute="median")
factC<-fa(as.data.frame(data)[,TriO],nfactors=2,covar=F,rotate="promax",fm="minres",impute="median")



psych::alpha(as.data.frame(data)[,c(Vars$Infl)])
omega(as.data.frame(data)[,c(Vars$Infl)])
ScaleDistribution(data.frame(data1)[,c("TeacherID",Vars$Infl)],"TeacherID",ALPHA=0.01)
source("C:/Users/mrkwht/Dropbox/Research/R libraries/DefaultScriptMark.R")
require(data.table)
require(gdata)
require(FactoMineR)
odir<-"O:/Education and Well Being/Projects/NCEE BES Evaluation Study/15 Data Collection & Analysis/03 Analytic Files/Student Level/Y2 Student Survey File"
data<-read.csv(file.path(odir,"y2StudSurvey_StateData_11012013.csv"))
data<-data[,!(names(data) %in% matchcols(data,"^QC|^KY|^CAM|^MS|^AZ"))]
data<-data[,!(names(data) %in% c("LastName","FirstName","St_DOB","St_SPED_Label","Grade_in_sy1213","YumaRandExam","SSurvey_GradPlansText"))]
data<-data.table(data)
setnames(data,names(data),sub("^SSurvey_","",names(data)))
summary(data)
Tri<-matchcols(data,"Care|Consolidate|Challenge|Control|Captivate|Clarify|Confer")

# Cluster
# require(fpc)
# require(clValid)
# cl1<-clValid::clValid(na.omit(data.matrix(data)[,Tri]),2:6,
#              c("hierarchical","diana","fanny","model","sota","pam"),
#              c("internal", "stability"),method="ward",verbose=T)
# summary(cl1)
# plot(cl1)


PCAshiny(na.omit(data.frame(data)[,Tri]))

# require(mclust)
# clus<-Mclust(na.omit(data.frame(data)[,Tri]),G=1:5)
# plot(clus,what="BIC")



# Code from elsewhere ####
ParallelAnalysis(as.data.frame(data)[,c(Vars$Infl)])
fact<-fa(as.data.frame(data)[,c(Vars$Infl)],nfactors=2,covar=F,rotate="promax",fm="minres",impute="median")
psych::alpha(as.data.frame(data)[,c(Vars$Infl)])
omega(as.data.frame(data)[,c(Vars$Infl)])
ScaleDistribution(data.frame(data1)[,c("TeacherID",Vars$Infl)],"TeacherID",ALPHA=0.01)
