setwd("C:/Users/mrkwht/Dropbox/Classes/Measurement of Teaching")
source("../../Research/R libraries/DefaultScriptMark.R")
require(data.table)
require(mirt)
require(plyr)
require(Hmisc)
require(polycor)
require(clValid)
require(lme4)
require(FactoMineR)
options(digits=2)

data<-data.table(read.csv("r/DRISData_reduced.csv"))
data1<-copy(data[Sample==1,])
data4<-copy(data[Sample==4,])

VarPer <- function(x) { 
  x <- summary(x)$varcor
  out <- c(unlist(x),attr(x,"sc")^2)
  names(out)[length(out)] <- "Resid"
  round(out / sum(out)*100,1)
}
SEM <- function(sd,r) sd*sqrt(1-r)
CI <- function(x,mean,r) (mean+r*(x-mean))
Prophecy <- function(rd,r0) rd*(1-r0)/(r0*(1-rd))
AttenCor <- function(cor, r11,r22) cor/sqrt(r11*r22)
require(Factoshiny)


# Variables ####
Vars <- list(Cov  =list(Loc  =c("region","Urbanicity","State"),
                        Teach=c("Role_Main","T_YrsExp","Gender","Race","Age","Degree","AltCert","Cert",
                                "Major_Read","Major_Engl","Major_Eng","Major_Ed","Endorse_Speech",
                                "Endorse_Read","Endorse_ELL","Endorse_SPED","Endorse_ZA","Endorse_Other"),
                        Class=c("NumKids","NumIEP","NumELL_bool","NumELL","NumLang","NumMin"),
                        Role =c("Role_ManyClasses", "Role_ManySection" ),
                        Survey=c("SchoolDay","TaughtToday","TimeOfYear","ReportDate")  ),
             Basal="Basal",
             Infl =c("Influence_StateStand","Influence_DistrStand","Influence_StateTest",
                     "Influence_DistrTest" ,"Influence_CurricFA"  ,"Influence_AYP","Influence_RTI",
                     "Influence_Basal","Influence_Princ","Influence_ReadCoach","Influence_Teacher"),
             Mat  =c("MatUseFreq_Required","MatUseFreq_Pacing","MatUseFreq_Scripted","MatUseFreq_Comp"),
             PD   =c("PD_WatchLeader","PD_LeaderWatch","PD_Meeting"),
             Stud =c("Stud_GradeK","Stud_Grade1","Stud_Grade2","Stud_Grade3","Stud_Grade4","Stud_Grade5","Stud_Grade6"),
             Abil =c("LevelBelow","LevelAt","LevelAbove","LevelThe","LevelAdd","LevelNever","LevelSpell","LevelBottle","LevelGiant","LevelDecrease","LevelBravery"),
             Disrupt=c("Disrupt_Behave","Disrupt_Announce","Disrupt_Tardy","Disrupt_Leave"),
             Supp=list(Str =c("Supports_Pace_Str"  ,"Supports_Review_Str" ,"Supports_Reduce_Str" ,"Supports_Pair_Str" ,"Supports_Aide_Str" ,"Supports_AddSpecial_Str" ,"Supports_ReplaceSpecial_Str" ,"Supports_OutSpecial_Str" ,"Supports_Other_Str" ,"Supports_NA_Str" ),
                       ELL =c("Supports_Pace_ELL"  ,"Supports_Review_ELL" ,"Supports_Reduce_ELL" ,"Supports_Pair_ELL" ,"Supports_Aide_ELL" ,"Supports_AddSpecial_ELL" ,"Supports_ReplaceSpecial_ELL" ,"Supports_OutSpecial_ELL" ,"Supports_Other_ELL" ,"Supports_NA_ELL" ),
                       SPED=c("Supports_Pace_SPED" ,"Supports_Review_SPED","Supports_Reduce_SPED","Supports_Pair_SPED","Supports_Aide_SPED","Supports_AddSpecial_SPED","Supports_ReplaceSpecial_SPED","Supports_OutSpecial_SPED","Supports_Other_SPED","Supports_NA_SPED")),
             Resource=c("Resource_Unplanned","Resource_Basal","Resource_Supplement","Resource_StateMand",
                        "Resource_DistMand" ,"Resource_CurricAssess","Resource_Screening","Resource_Professional",
                        "Resource_Self","Resource_OtherT","Resource_Other"),
             Group=list(Time  =c("Group_Whole","Group_Small","Group_Pair","Group_Ind"),
                        Reason=c("GroupReason_None","GroupReason_Ability","GroupReason_Mixed",
                                 "GroupReason_Interest","GroupReason_ELL","GroupReason_Targeted"),
                        AdultW=c("AdultW_Teacher","AdultW_Aide","AdultW_StudT","AdultW_OtherT","AdultW_None","AdultW_NA",
                                 "AdultS_Teacher","AdultS_Aide","AdultS_StudT","AdultS_OtherT","AdultS_None","AdultS_NA",
                                 "AdultP_Teacher","AdultP_Aide","AdultP_StudT","AdultP_OtherT","AdultP_None","AdultP_NA",
                                 "AdultI_Teacher","AdultI_Aide","AdultI_StudT","AdultI_OtherT","AdultI_None","AdultI_NA"),
                        SmGrp =c("SmallGr_NA","SmallGr_Groups","SmallGr_Group","SmallGr_Individual",
                                 "SmallGr_Circulated","SmallGr_Graded","SmallGr_None")),
             Materials=c("MatUse_Big","MatUse_Decode","MatUse_Trade","MatUse_Leveled","MatUse_Cards","MatUse_Anthology","MatUse_BasalWksht","MatUse_Software","MatUse_Other"),
             WD =c("WD_Inst","WD_Time","WD_Purp_New","WD_Purp_Review","WD_Purp_Guide","WD_Purp_Practice","WD_Purp_Monitor","WD_Purp_Other"),
             FL =c("FL_Inst","FL_Time","FL_Purp_New","FL_Purp_Review","FL_Purp_Guide","FL_Purp_Practice","FL_Purp_Monitor","FL_Purp_Other"),
             VO =c("VO_Inst","VO_Time","VO_Purp_New","VO_Purp_Review","VO_Purp_Guide","VO_Purp_Practice","VO_Purp_Monitor","VO_Purp_Other"),
             RC =c("RC_Inst","RC_Time","RC_Purp_New","RC_Purp_Review","RC_Purp_Guide","RC_Purp_Practice",
                   "RC_Purp_Knowledge","RC_Purp_Write","RC_Purp_Monitor","RC_Purp_Interest","RC_Purp_Other"),
             WR =c("WR_Inst","WR_Time","WR_Purp_New","WR_Purp_Review","WR_Purp_Practice","WR_Purp_Monitor","WR_Purp_Respond","WR_Purp_Other"),
             CT =c("CT_Inst") )
                               

# CLuster ####
require(fpc)
cl1<-clValid(na.omit(data.matrix(data1)[,Vars$Infl]),2:6,
            c("hierarchical","diana","fanny","model","sota","pam"),
            c("internal", "stability"),method="ward",verbose=T)
summary(cl1)
plot(cl1)
cl1<-diana(na.omit(data.matrix(data1)[,Vars$Infl]),metric="ward")
plotcluster(na.omit(data.matrix(data1)[,Vars$Infl]),cutree(cl1,2))
describeBy(na.omit(data1[,.SD,.SDcols=Vars$Infl]),list(cutree(cl1,2)))

cl4<-clValid(na.omit(data.matrix(data4)[,Vars$Infl]),2:6,
            c("hierarchical","diana","fanny","model","sota","pam"),
            c("internal", "stability"),method="ward",verbose=T)
summary(cl4)
plot(cl4)
cl4<-diana(na.omit(data.matrix(data4)[,Vars$Infl]),metric="ward")
plotcluster(na.omit(data.matrix(data4)[,Vars$Infl]),cutree(cl4,2))
describeBy(na.omit(data4[,.SD,.SDcols=Vars$Infl]),list(cutree(cl4,2)))

# PCA ####
with(na.omit(data[,c(Vars$Infl,"Sample"),with=F]), 
     plot(prcomp(data.frame(Influence_StateStand,Influence_DistrStand,Influence_StateTest,Influence_DistrTest,Influence_CurricFA,Influence_AYP,Influence_RTI,Influence_Basal,Influence_Princ,Influence_ReadCoach,Influence_Teacher))$x[,1:2],
          col=rainbow(4)[Sample]))
PCAinfl<-PCA(data[,c(Vars$Infl,"Sample"),with=F],scale.unit=T, quali.sup=12) # ,quanti.sup=1
summary(PCAinfl)

pcaInfl1 <- PCA(data1[,c(Vars$Infl),with=F],scale.unit=T) # ,quanti.sup=1, quali.sup=2:6
plot(pcaInfl,choix="ind",habillage=2,label="var",col.hab=heat.colors(6))



pcaInfl1 <- PCA(data1[,c(Vars$Infl),with=F],scale.unit=T) # ,quanti.sup=1, quali.sup=2:6
pcaInfl4 <- PCA(data4[,c(Vars$Infl),with=F],scale.unit=T) # ,quanti.sup=1, quali.sup=2:6
summary(pcaInfl1)
HCPCshiny(na.omit(  as.data.frame(data1)[,c(Vars$Infl)] ))

summary(pcaInfl4)
HCPCshiny(na.omit(  as.data.frame(data4)[,c(Vars$Infl)] ))

barplot(pcaInfl$eig[,1],main="Eigenvalues",names.arg=1:nrow(pcaInfl$eig))



# EFA ####
ParallelAnalysis(as.data.frame(data)[,c(Vars$Infl)])
fact<-fa(as.data.frame(data)[,c(Vars$Infl)],nfactors=2,covar=F,rotate="promax",fm="minres",impute="median")

fact1<-fa(as.data.frame(data1)[,c(Vars$Infl)],nfactors=2,covar=F,rotate="promax",fm="minres",impute="median")
fact4<-fa(as.data.frame(data4)[,c(Vars$Infl)],nfactors=2,covar=F,rotate="promax",fm="minres",impute="median")

(factpoly<-fa(r=cor.poly$correlations,nfactors=2,rotate="promax",fm="minres"))

# Alpha ####

psych::alpha(as.data.frame(data)[,c(Vars$Infl)])
omega(as.data.frame(data)[,c(Vars$Infl)])
ScaleDistribution(data.frame(data1)[,c("TeacherID",Vars$Infl)],"TeacherID",ALPHA=0.01)
ScaleDistribution(data.frame(data4)[,c("TeacherID",Vars$Infl)],"TeacherID",ALPHA=0.01)

# Correlations/Polychoric ####

cor.mat <-cor(na.omit(data[,c(Vars$Infl),with=F]))
cor.poly<-hetcor(apply(na.omit(data[,c(Vars$Infl),with=F]),2,ordered))
iclust(cor.mat)
iclust(cor.poly$correlations)

# IRT ####
mirtCluster(2)
ConvertCoef <- function(coef) {
  ldply(coef,function(L) { 
    L=as.data.frame(L)
    ret <- data.frame(par.a1=L["par","a1"], CI_2.5.a1=L["CI_2.5","a1"], CI_97.5.a1=L["CI_97","a1"], 
                      par.d1=L["par","d1"], CI_2.5.d1=L["CI_2.5","d1"], CI_97.5.d1=L["CI_97","d1"], 
                      par.d2=L["par","d2"], CI_2.5.d2=L["CI_2.5","d2"], CI_97.5.d2=L["CI_97","d2"], 
                      par.d3=L["par","d3"], CI_2.5.d3=L["CI_2.5","d3"], CI_97.5.d3=L["CI_97","d3"])
  })
}
datam<-na.omit(data.matrix(data)[,c(Vars$Infl)])
(mod1F<-mirt(datam,2,SE=T,verbose=F))

(mod1.dist <-mirt(datam[,c("Influence_StateStand","Influence_DistrStand","Influence_StateTest" ,"Influence_DistrTest")],1,SE=T,verbose=F))
(mod1.close<-mirt(datam[,c("Influence_Princ","Influence_ReadCoach","Influence_Teacher")],1,SE=T,verbose=F))

ConvertCoef(coef(mod1.dist))
ConvertCoef(coef(mod1.close))
describe(scores<-fscores(mod1.close,full.scores.SE=T))

M2(mod1F)
residuals(mod1.dist,df.p=1)
plot(mod1.close,type="infoSE",theta_lim=c(-4,4))
plot(mod1.close,type="rxx",main="Test Reliability Function",MI=100,theta_lim=c(-4,4))


hist(fscores(mod1.close),main="Person-Item Map",xlim=c(-4,4))
rug(-ConvertCoef(coef(mod1.close))[,"par.d1" ]/ConvertCoef(coef(mod1.close))[,"par.a1" ],col="red"  ,lwd=1.5,ticksize=0.1)
rug(-ConvertCoef(coef(mod1.close))[,"par.d2" ]/ConvertCoef(coef(mod1.close))[,"par.a1" ],col="blue" ,lwd=1.5,ticksize=0.1)
rug(-ConvertCoef(coef(mod1.close))[,"par.d3" ]/ConvertCoef(coef(mod1.close))[,"par.a1" ],col="green",lwd=1.5,ticksize=0.1)


plot(mod1.dist,type="trace",facet_items=F,theta_lim=c(-4,4))
plot(mod1.dist,type="trace",facet_items=T,theta_lim=c(-4,4))
plot(mod1.close,type="infotrace",theta_lim=c(-4,4))
plot(mod1.close,type="info",theta_lim=c(-4,4))

scores<-fscores(mod1.close,full.scores=T,full.scores.SE=T)
scores<-data.frame(N=1:nrow(scores),scores[order(scores[,"F1"]),])
ggplot(scores,aes(y=F1,x=N,ymin=F1-0.954*SE_F1,ymax=F1+0.954*SE_F1))+
  geom_errorbar(width=0.08,alpha=0.1)+
    geom_errorbar(aes(y=scores$F1,x=scores$N,ymin=scores$F1-0.954*0.42,ymax=F1+0.954*0.42),width=0.08,alpha=0.1,color=blue)+
    geom_point(col="red")+coord_flip()

itemfit(mod1.dist)
plot(itemGAM(datam[,c("Influence_StateStand")], fscores(mod1.dist)),sub="Influence_StateStand") # Best Fitting Item
plot(itemGAM(datam[,c("Influence_DistrStand")], fscores(mod1.dist)),sub="Influence_DistrStand") # Best Fitting Item
hist.rug(pf<- personfit(mod1.dist,theta=fscores(mod1.dist))$Zh )
BottomFive <-head(sort(pf))


(modRaschNest <- mixedmirt(as.data.frame(dataimpute),
                           as.data.frame(data.SD,.SDcols=CovVars])  ,
                           model=1, parallel=T,
                           fixed=~0+items+FFT_ObserverID,      SE=F,
                           random=list(~1|TeacherID) ,verbose=F))
cor(randef(modRaschNest)$Theta,randef(mod1Nest)$Theta) 

for (i in Dims) {
  dat=data.frame(x=THETA,y=FFTimpute[,i],color=data$FFT_ObserverID)
  ggplot(dat,aes(x=x,y=y,color=color)) + geom_point()+
    geom_smooth(se=F,span=0.7) + 
    labs(title=expression(atop(
      "Empirical Smoothed Relationship between Theta and Observation",
      i)),x="Theta",y="Observed")
}    
mirtCluster(remove = TRUE)

# Testing ####
PCAshiny(na.omit(data.frame(data)[,c(Vars$Infl)]))

sc<-kernlab::specc(na.omit(data.matrix(Teacher04[,Q18])),centers=15)
plot(aa$scores[,1:2],col=sc)

require(mclust)
clus<-Mclust(na.omit(Teacher04[,Q38]),G=1:5)
plot(clus,what="BIC")

require(fpc) # flexible procedures for clustering
#
# outlier test by looking at densities 

# dbscan -> kNNdistplot to find eps (part where becomes vertical)
# can be used for outliers too... 

#silhouette

#Mass Cluster Data


# fan <-fanny(na.omit(Teacher04[,Q18]),2,memb.exp=1.1)
# summary(silhouette(fan))
# panelplots(2,1)
# lapply(1:2,function(x) hist.rug(fan$membership,
#                                 main=paste0("Membership Probabiltities of Cluster ",x)))
# print("Percentage of Cases with Propabilities between 20% and 80% in a cluster")
# apply(fan$membership,2,function(x) mean(x>0.2 & x<0.8))
