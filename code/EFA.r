rm(list=ls())
source("C:/Users/mrkwht/Dropbox/Research/R libraries/DefaultScriptMark.R")
# source("C:/Users/Buffning/Documents/My Dropbox/Research/R libraries/DefaultScriptMark.R")
require(gdata)
require(data.table)
require(Hmisc)
require(polycor)
require(lme4)
require(FactoMineR)
options(digits=2)
if (!require("RColorBrewer")) {
    install.packages("RColorBrewer")
    library(RColorBrewer)
}
setwd("C:/Users/mrkwht/Google Drive/Measurement of Teaching")
# setwd("C:/Users/Buffning/Google Drive/Measurement of Teaching")
# 
load(file="data/TeacherSurveyY4.rData")

# Create Variables and Partition Data -------------------------------------

VarPer <- function(x) { 
  x <- summary(x)$varcor
  out <- c(unlist(x),attr(x,"sc")^2)
  names(out)[length(out)] <- "Resid"
  round(out / sum(out)*100,1)
}
names(Teacher04) <- sub("Domain","Dmn",names(Teacher04))

FOM.R   <- c("R.Expectancy"  ,"R.Efficacy"  ,"R.Value"  )
FOM.M   <- c("M.Expectancy"  ,"M.Efficacy"  ,"M.Value"  )
FOM.CSR <- c("CSR.Expectancy","CSR.Efficacy","CSR.Value", "CSR.RequiresChange")
Cov.R <- c("R.Grade","R.ClassSize","R.Achievement","R.Minutes","R.Group.Whole","R.Group.Ability","R.Group.Mixed","R.Group.Individual")
Cov.M <- c("M.Grade","M.ClassSize","M.Achievement","M.Minutes","M.Group.Whole","M.Group.Ability","M.Group.Mixed","M.Group.Individual")
Q18 <- c("R.Focus.RC.PriorKnowledge"    ,"R.Focus.RC.GeneratingQuestions","R.Focus.RC.Summarizing","R.Focus.RC.Analyzing"        ,
         "R.Focus.RC.LiteraryTechniques","R.Focus.RC.AuthorPurpose"      ,"R.Focus.RC.ConceptMaps","R.Focus.RC.ExplicitQuestions",
         "R.Focus.RC.ImplicitQuestions"  )
Q19 <- c("R.TestRC.WriteBrief","R.TestRC.WriteExtended","R.TestRC.ThinkAloud","R.TestRC.Project")
Q20 <- c("R.Focus.WR.Proofing","R.Focus.WR.WordUse","R.Focus.WR.Elaborating","R.Focus.WR.Reorganizing")
Q38 <- c("M.Dmn.Counting","M.Dmn.Integer"    ,"M.Dmn.Fractions",
         "M.Dmn.Addition","M.Dmn.Subtraction","M.Dmn.Multiplication",
         "M.Dmn.Division","M.Dmn.Patterns"   ,"M.Dmn.Functions",
         "M.Dmn.Geometry","M.Dmn.Measurement","M.Dmn.Graphs")
Q39 <- c("M.Focus.Op.Properties"           ,"M.Focus.Op.Strategies"  ,"M.Focus.Op.PracticeFacts",
         "M.Focus.Op.WhyProcedure"         ,"M.Focus.Op.Procedure"   ,"M.Focus.Op.PracticingProcedures",
         "M.Focus.Op.NonCoventionalMethods","M.Focus.Op.WordProblems","M.Focus.Op.Estimating")
Q40 <- c("M.Focus.Lecture"          ,"M.Focus.Practice","M.Focus.ChooseMethod",
         "M.Focus.DiscoverProcedure","M.Focus.Explain" ,"M.Focus.Compare",
         "M.Focus.Proof"            ,"M.Focus.MultAns" ,"M.Focus.Discuss",
         "M.Focus.Write"            ,"M.Focus.Project" )
Q67 <- c("TCourses.ELA","TCourses.ELAMethods","TCourses.Math","TCourses.MathMethods")
Teacher04$PROGRAM <- sub("'","",Teacher04$PROGRAM)
Teacher04$PROGRAM<-Factor(recode(Teacher04$PROGRAM,"'AMERICAS CHOICE'='AC'; 'ACCELERATED SCHOOLS PROJECT'='ASP'; 'COMPARISON'='C'; 'SUCCESS FOR ALL'='SFA';"))
Teacher04$PROGRAM<-factor(Teacher04$PROGRAM,levels=c("C","AC","ASP","SFA"))
Teacher04$R.Grade<-Factor(recode(Teacher04$R.Grade,"'KINDERGARTEN'='K';"))

for (i in Q67 )
    Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'NONE'              =0; 
                                                    '1-3 CLASSES'       =1; 
                                                    '4-6 CLASSES'       =2;
                                                    '7-9 CLASSES'       =3;
                                                    '10-15 CLASSES'     =4;
                                                     '16 OR MORE CLASSES'=5;"))
for (i in c(Q38,Q39) ) 
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'Did not teach this topic'=0;
                                                  '1-2 LESSONS'         =1;
                                                  '3-5 LESSONS'         =2;
                                                  '6-10 LESSONS'        =3;
                                                  '11-15 LESSONS'       =4;
                                                  'MORE THAN 15 LESSONS'=5;"))
for (i in c(Q40) ) 
  Teacher04[,i] <- unFactor(recode(Teacher04[,i],"'NEVER'                 =0;
                                                  'LESS THAN ONCE A MONTH'=1;
                                                  '1-3 TIMES PER MONTH'   =2;
                                                  '1-2 TIMES PER WEEK'    =3;
                                                  '3-4 TIMES PER WEEK'    =4;
                                                  'EVERY DAY'             =5"))

# English Q18-20 -------------------------------------------------

Teacher04 <- subset(Teacher04, R.Grade %in% c("K","1ST","2ND","3RD","4TH","5TH"))
Teacher04$R.Grade <-factor(Teacher04$R.Grade, levels=c("K","1ST","2ND","3RD","4TH","5TH"))

summary(sapply(Teacher04[,c("PROGRAM","R.Grade","R.Achievement","TCourses.ELAMethods","TCourses.ELA")],factor),12)

describe(Teacher04[,c(Q18,Q19,Q20)])

for (i in c(Q18,Q19,Q20) ) {
  print(i)
  barplot(xtabs(~Teacher04[,i]))
  print(MakeRegPretty(lm( as.formula(paste0(i,"~PROGRAM+R.Grade+R.Achievement+TCourses.ELAMethods")),Teacher04)))
  print(VarPer(lmer(as.formula(paste0(i,"~(1|schoolid)+(1|dstrctid)"    )),Teacher04)))
  print("Enter for Next Variable")
  a<-readline()
}

summary(Q18pca <-PCA(Teacher04[,c("R.Minutes","TCourses.ELA","TCourses.ELAMethods","R.Achievement","PROGRAM","R.Grade",Q18)],
                     scale.unit=T,quanti.sup=1,quali.sup=2:6))
summary(Q18pca <-PCA(Teacher04[,c(Q18,Q19,Q20)],scale.unit=T))
barplot(Q18pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(Q18pca$eig))
plot(Q18pca,choix="ind",habillage=2,label="var",col.hab=heat.colors(6))
plot(Q18pca,choix="ind",habillage=3,label="var",col.hab=heat.colors(6))
plot(Q18pca,choix="ind",habillage=4,label="var",col.hab=heat.colors(6))
plot(Q18pca,choix="ind",habillage=5,label="var")
plot(Q18pca,choix="ind",habillage=6,label="var")#select="cos2 0.5",unselect=1
Q18pca$eig
# summary(Q18pca2 <- princomp(covmat=hetcor(sapply(na.omit(Teacher04[,Q18]),ordered),std.err=F)$correlations))

ParallelAnalysis(Teacher04[,c(Q18,Q19,Q20)])
fa(Teacher04[,c(Q18,Q19,Q20)],nfactors=2,covar=F,rotate="none",fm="minres",impute="median")

# Math Q 38-40 ------------------------------------------------------------

Teacher04 <- subset(Teacher04, M.Grade %in% c("K","1ST","2ND","3RD","4TH","5TH"))
Teacher04$M.Grade <-factor(Teacher04$M.Grade, levels=c("K","1ST","2ND","3RD","4TH","5TH"))

describe(Teacher04[,c(Q38,Q39)]) 


for (i in c(Q38,Q39) ) { # Q40
  print(i)
  barplot(xtabs(~Teacher04[,i]),main=i)
  print(MakeRegPretty(lm( as.formula(paste0(i,"~PROGRAM+M.Grade+M.Achievement")),Teacher04)))
  print(VarPer(lmer(as.formula(paste0(i,"~(1|schoolid)+(1|dstrctid)"    )),Teacher04)))
  print("Enter for Next Variable")
  a<-readline()
}

cor(na.omit(Teacher04[,c(Q38)]))
hetcor(apply(na.omit(Teacher04[,c(Q38)]),2,ordered))
Teacher04$AddSub <- apply(Teacher04[,c("M.Dmn.Addition","M.Dmn.Subtraction")],1,mean,na.rm=T)

m <- manova(as.formula(paste0("cbind(",paste(Q38,collapse=","),")~M.Grade+M.Minutes+M.Achievement")),Teacher04)
summary(m)
summary(lm(as.formula(paste0("cbind(",paste(Q38,collapse=","),")~M.Grade+M.Minutes+M.Achievement")),Teacher04))


ScaleDistribution(Teacher04[,c("respid",Q38)],"respid",ALPHA=0.02)

summary(Q38pca <-PCA(Teacher04[,c("M.Minutes","M.Achievement","PROGRAM","M.Grade",Q38)],
                     scale.unit=T,quanti.sup=1,quali.sup=2:4))
plot(Q38pca$eig[,1],main="Eigenvalues",type="o")
plot(Q38pca,choix="ind",habillage=2,label="var",col.hab=heat.colors(6))
plot(Q38pca,choix="ind",habillage=4,label="var",col.hab=brewer.pal(10,"Spectral"))


ParallelAnalysis(Teacher04[,c(Q38)])
ParallelAnalysis(Teacher04[,c("M.Dmn.Counting","M.Dmn.Integer","M.Dmn.Fractions","AddSub","M.Dmn.Multiplication","M.Dmn.Division","M.Dmn.Patterns","M.Dmn.Functions","M.Dmn.Geometry","M.Dmn.Measurement","M.Dmn.Graphs")])
fnum=3
(Q38fa<-fa(Teacher04[,c(Q38)],covar=F,fm="minres",nfactors=fnum,rotate="promax"))
print(Q38fa$loadings,cut=0.3)

samp <- sample(0:1,nrow(Teacher04),replace=T)
print(fa(Teacher04[samp==0,c(Q38)],covar=F,fm="minres",nfactors=fnum,rotate="promax")$loadings,cut=0.3)
print(fa(Teacher04[samp==1,c(Q38)],covar=F,fm="minres",nfactors=fnum,rotate="promax")$loadings,cut=0.3)


require(clValid)
a<-clValid(na.omit(Teacher04[,Q38]),2:6,clMethods=c("hierarchical","fanny","model","clara"), validation=c( "internal", "stability"))
summary(a)
plot(a)
c<-clusters(a,"hierarchical")
describeBy(na.omit(Teacher04[,Q18]),list(cutree(c,2)))


# Teaching Variables ------------------------------------------------------

describeBy(Teacher04[Teacher04$R.Grade %in% c("1ST","2ND","3RD","4TH","5TH"),matchcols(Teacher04,"R.Focus.RC")],
           Teacher04[Teacher04$R.Grade %in% c("1ST","2ND","3RD","4TH","5TH"),"R.Grade"])

ScaleDistribution(Teacher04[,c("respid",matchcols(Teacher04,"R.Focus.RC"))],"respid",ALPHA=0.02)
ScaleDistribution(Teacher04[,c("respid",matchcols(Teacher04,"R.TestRC"  ))],"respid",ALPHA=0.02)
ScaleDistribution(Teacher04[,c("respid",matchcols(Teacher04,"R.Focus.WR"))],"respid",ALPHA=0.02)

ParallelAnalysis(Teacher04[,c(matchcols(Teacher04,"R.Focus.RC"),matchcols(Teacher04,"R.TestRC"),matchcols(Teacher04,"R.Focus.WR"))])
fa(Teacher04[,c(matchcols(Teacher04,"R.Focus.RC"),matchcols(Teacher04,"R.TestRC"),matchcols(Teacher04,"R.Focus.WR"))],2)

# ScaleDistribution(Teacher04[,c("respid",matchcols(Teacher04,"R.Write"   ))],"respid",ALPHA=0.01)

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

require(mclust)
clus<-Mclust(na.omit(Teacher04[,Q38]),G=1:5)
plot(clus,what="BIC")



# Graph -------------------------------------------------------------------
# 
# hist.rug(outlier(na.omit(Teacher04[,Q18])))
# knn.index in package FNN returns the k nearest neighbors to each point
# MakeEdges <- function(x) {
#   D <- as.matrix(dist(x,method="euclid"))
#   D <- D/max(D)
#   DIST <- min(apply(D,1,max)) # Smallest maximum distance in row
#   D[D >= DIST] <- NA
#   
#   Edges <-do.call(rbind.data.frame,
#                   lapply(1:(ncol(D)-1), function(x) data.frame(From  =colnames(D)[x],
#                                                                To    =colnames(D)[(x+1):ncol(D)],
#                                                                Weight=D[x, (x+1):ncol(D)]) ) )
#   Edges <- Edges[!is.na(Edges$Weight) & Edges$Weight>0,]
#   Edges <- Edges[order(Edges$Weight),]
#   Edges <- Edges[1:min(nrow(Edges),100000),]
#   
#   return(Edges)
# }
# write.csv(MakeEdges(na.omit(Teacher04[,Q18])),file="graph.csv")

    

# Spectral Analysis -------------------------------------------------------

sc<-kernlab::specc(na.omit(data.matrix(Teacher04[,Q18])),centers=15)
plot(aa$scores[,1:2],col=sc)


require(Factoshiny)
HCPCshiny(na.omit(Teacher04[,Q18]))
PCAshiny(na.omit(Teacher04[,Q18]))



