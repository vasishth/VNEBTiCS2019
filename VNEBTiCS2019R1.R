## ----setup,include=FALSE,cache=FALSE,echo=FALSE--------------------------
library(MASS)
library(knitr)
library(xtable)
library(papaja)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
#library(sjPlot)
library(rstan)
library(brms)
library(gridExtra)
library(bayesplot)
library(ggridges)
library(lme4)
library(reshape2)

theme_set(theme_apa())

# set global chunk options, put figures into folder
options(warn=-1, replace.assign=TRUE)
opts_chunk$set(fig.path='figures/figure-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=75)
opts_chunk$set(dev='postscript')
opts_chunk$set(echo = TRUE)
source("R/multiplot.R")
source("R/magnifytext.R")
source("R/createStanDat.R")
source("R/stan_results.R")



## ----raceprocessillustration,echo=FALSE,fig.height=4---------------------
nsim<-1000000
process_A<-rnorm(nsim,mean=400,sd=75)
process_B<-rnorm(nsim,mean=420,sd=75)
winner<-ifelse(process_A<process_B,process_A,process_B)

raceprocess<-data.frame(process=rep(c("process A","process B","winner"),each=nsim),time=c(process_A,process_B,winner))

ggplot(raceprocess, 
       aes(x = time, y = process, fill = process, height = ..density..)) +
  geom_density_ridges(scale = 4, stat = "density") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = "PuBuGn") +
  theme_ridges() + theme(legend.position = "none")+
  xlab("finishing time (ms)")+
  ylab("race processes")+
  geom_vline(xintercept=mean(winner))+
  geom_vline(xintercept=mean(process_A),linetype='dashed')+
  geom_vline(xintercept=mean(process_B),linetype='dashed')+
  #ggtitle("An illustration of a race process")+
  magnifytext(sze=20)


## ----modelpredictions,echo=FALSE-----------------------------------------
## model predictions See Vasishth and Engelmann book
load("data/match_reduced.Rda")
load("data/mismatch_reduced.Rda")
ACTRpredictionmatch<-quantile(match_reduced$Effect,prob=c(0.025,0.5,0.975))
ACTRpredictionmatch[2]<-mean(match_reduced$Effect)
ACTRpredictionmismatch<-quantile(mismatch_reduced$Effect,prob=c(0.025,0.5,0.975))
ACTRpredictionmismatch[2]<-mean(mismatch_reduced$Effect)


## ----VDykeEstimates, echo=FALSE, eval=FALSE------------------------------
## ## table for rt and tft (crit);
## ## see extraction of tft effects in IntEffsVanDykePlots.Rmd
## 
## Publication <- c('VanDykeEtal03E4', 'VanDykeEtAl06', 'VanDyke07E1LoSem',  'VanDyke07E1LoSyn',  'VanDyke07E2LoSem', 'VanDyke07E2LoSyn', 'VanDyke07E3LoSyn',  'VanDyke07E3LoSem', 'VanDykeEtAl11E1bPro', 'VanDykeEtAl11E1bRetro', 'VanDykeEtAl11E2bPro', 'VanDykeEtAl11E2bRetro')
## 
## Lang <- rep('EN', 12)
## 
## Method_Measure <- c('SPR_RT', 'SPR_RT', 'SPR_RT', 'SPR_RT', 'ET_TFT', 'ET_TFT', 'ET_TFT', 'ET_TFT', 'ET_TFT', 'ET_TFT', 'ET_TFT', 'ET_TFT')
## 
## Effect <- c(56, 38, 13, 54, 108, 23, 37, 7, 20, 81, 40, 2)
## 
## SE <- c(25, 20, 30, 34, 42, 35, 28, 25, 20, 33, 22, 22)
## 
## MatchNonAgrmtTFT <- data.frame(Publication, Lang, Method_Measure, Effect, SE)
## MatchNonAgrmtTFT <- MatchNonAgrmtTFT[with(MatchNonAgrmtTFT,order(Effect)),]
## 
## # add columns for CIlower, CIupper
## MatchNonAgrmtTFT$lower <- MatchNonAgrmtTFT$Effect - 2*MatchNonAgrmtTFT$SE
## MatchNonAgrmtTFT$upper <- MatchNonAgrmtTFT$Effect + 2*MatchNonAgrmtTFT$SE
## 
## #intTFTdata <- MatchNonAgrmtTFT
## 
## ## add LV05 pred to dataframe
## Publication <- 'lv05pred'
## Lang <- 'EN'
## Method_Measure <- 'lv05pred'
## Effect <- ACTRpredictionmatch[2]
## SE <- 'NA'
## lower <- ACTRpredictionmatch[1]
## upper <- ACTRpredictionmatch[3]
## 
## lv05pred<-data.frame(Publication,Lang,Method_Measure,Effect,SE,lower,upper)
## 
## 
## d <- rbind(MatchNonAgrmtTFT,lv05pred)
## d$Publication <- reorder(d$Publication, d$Effect)
## d$col <- c('0', '0', '0', '0','0', '0', '0', '0', '0', '0','0', '0','1')
## d$Publication<-factor(d$Publication,levels=c('lv05pred', 'VanDykeEtAl11E2bRetro',                                                 'VanDyke07E3LoSem', 'VanDyke07E1LoSem','VanDykeEtAl11E1bPro', 'VanDyke07E2LoSyn', 'VanDyke07E3LoSyn', 'VanDykeEtAl06',  'VanDykeEtAl11E2bPro',  'VanDyke07E1LoSyn', 'VanDykeEtal03E4', 'VanDykeEtAl11E1bRetro', 'VanDyke07E2LoSem'))
## 
## ## reorder by increasing y:
## MatchNonAgrmtTFT <- MatchNonAgrmtTFT[with(MatchNonAgrmtTFT,order(Effect)),]
## ## isolate relevant columns:
## MatchNonAgrmtTFT<-MatchNonAgrmtTFT[,c(1,2,4,5)]
## dat <- MatchNonAgrmtTFT
## 
## stan_dat<-list(y=dat$Effect,
## s=dat$SE,
## n=dim(dat)[1])
## 
## ma_model <- stan(file='StanModels/rema2.stan', data=stan_dat,
##             iter=2000, chains=4, seed=987654321,
##             control = list(adapt_delta = 0.99))
## 
## postsummary<-summary(ma_model,pars="mu")$summary[c(1,4,8)]
## 
## paramnames<-c("mu")
## 
## ma_mean<-round(postsummary[1])
## ma_lower<-round(postsummary[2])
## ma_upper<-round(postsummary[3])
## 
## # add ma estimate to dataframe
## Publication <- 'meta'
## Lang <- 'EN'
## Method_Measure <- 'RT_TFT'
## Effect <- 34
## SE <- NA
## 
## meta <- data.frame(Publication, Lang, Method_Measure, Effect, SE)
## 
## meta$lower <- ma_lower
## meta$upper <- ma_upper
## meta$col <- '2'
## 
## d <- rbind(d, meta)
## 
## d$Publication <- reorder(d$Publication, d$Effect)
## d$Publication<-factor(d$Publication,levels=c('lv05pred', 'meta', 'VanDykeEtAl11E2bRetro','VanDyke07E3LoSem', 'VanDyke07E1LoSem','VanDykeEtAl11E1bPro','VanDyke07E2LoSyn','VanDyke07E3LoSyn','VanDykeEtAl06','VanDykeEtAl11E2bPro','VanDyke07E1LoSyn','VanDykeEtal03E4','VanDykeEtAl11E1bRetro','VanDyke07E2LoSem'))


## ----plotVDTFTestimates,echo=FALSE,eval=FALSE----------------------------
## ## plot tft estimates
## 
## pd<-position_dodge(0.6)
## 
## p<-ggplot(d, aes(y=Effect, x=Publication,
##                 # shape=Method_Measure,
##                  colour=col,
##                  ymax=upper, ymin=lower)) +
##  # coord_flip()+
##   geom_hline(yintercept=lower, linetype="dashed", colour="#666666") +
##   geom_hline(yintercept=upper, linetype="dashed", colour="#666666") +
##   scale_color_manual(values = c('0'='black','1'='#666666', '2'='red'))+
##  scale_x_discrete(labels=c('LV05 predictions', 'Meta analysis (SPR,ET)', 'VDMcElree11, E2bRetro (ET)', 'VD07, E3LoSem (ET)', 'VD07, E1LoSem (SPR)', 'VDMcElree11, E1bPro (ET)', 'VD07, E2LoSyn (ET)',  'VD07, E3LoSyn (ET)',  'VDMcElree06 (SPR)', 'VDMcElree11, E2bPro (ET)', 'VD07, E1LoSyn (SPR)',  'VDLewis03, E4 (SPR)', 'VDMcElree11, E1bRetro (ET)','VD07, E2LoSem (ET)'))+
##   geom_point(position=pd, size=3)+
##   geom_errorbar(aes(ymin=lower, ymax=upper),
##                 width=.25, size=.5, position=pd)+
##   theme_bw() +
##   xlab('') +
##   ylab('(total) reading time estimates in ms')+
##   labs(title='Inhibitory interference effects \n in reading studies') +
##   theme(legend.title=element_blank(), legend.position = 'none') +
##   theme(plot.title = element_text(size=18, hjust = 0.5),
##        axis.text.y=element_text(size=18),
##        axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=18))+
##   theme(axis.title.y = element_text(size = 18))


## ----agrmtattrnanalyses,echo=FALSE,eval=FALSE,message=FALSE,warning=FALSE,include=FALSE,results='asis',cache=TRUE----
## ## Dillon et al 2013 Expt 1
## DillonE1<-read.table("data/DillonE1.txt",header=T)
## ## Lago et al 2015 data (all expts):
## Lago<-read.csv("data/Lago.csv",header=T)
## ##Wagers et al 2009 data (all expts):
## load("data/Wagers.Rdata")
## load("data/Tucker.RData")
## 
## ## Dillon E1:
## DillonE1$cond<-factor(DillonE1$cond)
## DillonE1Mism<-subset(DillonE1,fixationtype=="tt" & cond%in%c(3,4) & value!="NA")
## DillonE1Mism$cond<-factor(DillonE1Mism$cond)
## DillonE1Mism$int<-ifelse(DillonE1Mism$cond==3,"low","high")
## DillonE1Mism$x<-ifelse(DillonE1Mism$cond==3,-1,1)
## dillonE1<-DillonE1Mism[,c(1,3,4,14,15)]
## dillonE1$expt<-factor("dillonE1")
## colnames(dillonE1)[3]<-"rt"
## 
## nsubj_dillonE1<-length(unique(dillonE1$subj))
## 
## ##Lago:
## dat<-Lago
## ## critical region: not used because published paper found
## ## significant effects in postcrit region only
## e1<-subset(dat,Experiment=="Experiment1" & Region=="06v1")
## e2<-subset(dat,Experiment=="Experiment2" & Region=="06aux")
## e3a<-subset(dat,Experiment=="Experiment3A" & Region=="06aux")
## e3b<-subset(dat,Experiment=="Experiment3B" & Region=="aux")
## 
## nsubj_lagoe1<-length(unique(e1$Subject))
## nsubj_lagoe2<-length(unique(e2$Subject))
## nsubj_lagoe3a<-length(unique(e3a$Subject))
## nsubj_lagoe3b<-length(unique(e3b$Subject))
## 
## 
## ## postcritical region:
## poste1<-subset(dat,Experiment=="Experiment1" & Region=="07prep")
## poste2<-subset(dat,Experiment=="Experiment2" & Region=="07adv")
## poste3a<-subset(dat,Experiment=="Experiment3A" & Region=="07a")
## poste3b<-subset(dat,Experiment=="Experiment3B" & Region=="a")
## 
## ##e1: a,b
## #-(a) Ungram , singular attractor (interference condition)
## #La *nota* que la chica escribieron en la clase alegró a su amiga
## #The note that the girl wrotepl during class cheered her friend up
## #-(b) Ungram , plural attractor (baseline condition)
## #Las *notas* que la chica escribieron en la clase alegraron a su amiga
## #The notes that the girl wrotepl during class cheered her friend up
## poste1<-subset(poste1,Condition%in%c("a","b"))
## poste1$Condition<-factor(poste1$Condition)
## poste1$x<-ifelse(poste1$Condition=="a",-1,1)
## poste1$int<-ifelse(poste1$Condition=="a","low","high")
## poste1<-poste1[,c(1,3,8,15,14)]
## poste1$expt<-factor("lagoE1")
## lagoE1<-poste1
## colnames(lagoE1)<-c("subj","item","rt","int","x","expt")
## 
## ## e2: c,d
## poste2<-subset(poste2,Condition%in%c("c","d"))
## poste2$Condition<-factor(poste2$Condition)
## poste2$x<-ifelse(poste2$Condition=="c",-1,1)
## poste2$int<-ifelse(poste2$Condition=="c","low","high")
## #head(poste2)
## poste2<-poste2[,c(1,3,8,15,14)]
## poste2$expt<-factor("lagoE2")
## lagoE2<-poste2
## colnames(lagoE2)<-c("subj","item","rt","int","x","expt")
## 
## ## e3a: e,f
## poste3a<-subset(poste3a,Condition%in%c("e","f"))
## poste3a$Condition<-factor(poste3a$Condition)
## #-(e) Ungram, singular attractor (interference condition)
## #La *nota* que la chica van a escribir en la clase alegrará a su amiga
## #The note that the girl are going to write during class will cheer her friend up
## #-(f) Ungram, plural attractor (baseline condition)
## #Las *notas* que la chica van a escribir en la clase alegrarán a su amiga
## #The notes that the girl are going to write during class will cheer her friend up
## #boxplot(RT~Condition,poste3a)
## poste3a$x<-ifelse(poste3a$Condition=="e",-1,1)
## poste3a$int<-ifelse(poste3a$Condition=="e","low","high")
## poste3a<-poste3a[,c(1,3,8,15,14)]
## poste3a$expt<-factor("lagoE3a")
## lagoE3a<-poste3a
## colnames(lagoE3a)<-c("subj","item","rt","int","x","expt")
## 
## ## e3b: e,f
## poste3b<-subset(poste3b,Condition%in%c("e","f"))
## poste3b$Condition<-factor(poste3b$Condition)
## #-(e) Ungram, singular attractor (baseline condition)
## #The player that the coach were always praising very enthusiastically decided to     leave the team
## #-(f) Ungram, plural attractor (interference condition)
## #The players that the coach were always praising very enthusiastically decided to     leave the team
## poste3b$x<-ifelse(poste3b$Condition=="e",-1,1)
## poste3b$int<-ifelse(poste3b$Condition=="e","low","high")
## poste3b<-poste3b[,c(1,3,8,15,14)]
## poste3b$expt<-factor("lagoE3b")
## lagoE3b<-poste3b
## colnames(lagoE3b)<-c("subj","item","rt","int","x","expt")
## 
## ## Wagers:
## E2postcrit<-subset(Experiment2,Region==7)
## nsubj_wagerse2<-length(unique(E2postcrit$Subj))
## #E2$intr.au<-ifelse(E2$rchead=="pl" & E2$gramm=="ungram",1/2,
## #                   ifelse(E2$rchead=="sg" & E2$gramm=="ungram",-1/2,
## #                          0))
## ## d (sing),h (plu)
## #unique(subset(E2postcrit,gramm=="ungram")$Condition)
## E2postcrit<-subset(E2postcrit,Condition%in%c("d","h"))
## E2postcrit$Condition<-factor(E2postcrit$Condition)
## E2postcrit$x<-ifelse(E2postcrit$Condition=="d",-1,1)
## E2postcrit$int<-ifelse(E2postcrit$Condition=="d","low","high")
## #colnames(E2postcrit)
## E2postcrit<-E2postcrit[,c(4,3,8,13,12)]
## E2postcrit$expt<-factor("wagersE2")
## wagersE2<-E2postcrit
## colnames(wagersE2)<-c("subj","item","rt","int","x","expt")
## 
## ## E3
## E3postcrit<-subset(Experiment3,Region==7)
## nsubj_wagerse3<-length(unique(E3postcrit$Subj))
## #E3crit$intr.au.pl<-ifelse(E3crit$gramm=="ungram" & E3crit$rcsubj=="sg" &
## #                            E3crit$rchead=="pl",1/2,
## #                         ifelse(E3crit$gramm=="ungram" & E3crit$rcsubj=="sg" &
## #                                   E3crit$rchead=="sg",-1/2,0))
## 
## #E3crit$intr.au.sg<-ifelse(E3crit$gramm=="ungram" & E3crit$rcsubj=="pl" &
## #                            E3crit$rchead=="sg",1/2,
## #                          ifelse(E3crit$gramm=="ungram" & E3crit$rcsubj=="pl" &
## #                                   E3crit$rchead=="pl",-1/2,0))
## 
## E3postcrit_pl<-subset(E3postcrit,gramm=="ungram" & rcsubj=="sg")
## E3postcrit_pl$Condition<-factor(E3postcrit_pl$Condition)
## E3postcrit_sg<-subset(E3postcrit,gramm=="ungram" & rcsubj=="pl")
## E3postcrit_sg$Condition<-factor(E3postcrit_sg$Condition)
## 
## #unique(E3postcrit_pl$Condition) ## b,f
## #unique(E3postcrit_sg$Condition) ## c,g
## 
## #head(subset(E3postcrit_sg,rchead=="sg"))
## #head(E3postcrit_pl)
## 
## ## plural:
## E3postcrit_pl$x<-ifelse(E3postcrit_pl$Condition=="b",-1,1)
## E3postcrit_pl$int<-ifelse(E3postcrit_pl$Condition=="b","low","high")
## E3postcrit_pl<-E3postcrit_pl[,c(4,3,8,15,14)]
## E3postcrit_pl$expt<-factor("wagersE3pl")
## colnames(E3postcrit_pl)<-c("subj","item","rt","int","x","expt")
## wagersE3pl<-E3postcrit_pl
## 
## ## singular:
## E3postcrit_sg$x<-ifelse(E3postcrit_sg$Condition=="c",-1,1)
## E3postcrit_sg$int<-ifelse(E3postcrit_sg$Condition=="c","low","high")
## E3postcrit_sg<-E3postcrit_sg[,c(4,3,8,15,14)]
## E3postcrit_sg$expt<-factor("wagersE3sg")
## colnames(E3postcrit_sg)<-c("subj","item","rt","int","x","expt")
## wagersE3sg<-E3postcrit_sg
## 
## ## E4
## E4postcrit<-subset(Experiment4,Region==8) ##
## nsubj_wagerse4<-length(unique(E4postcrit$Subj))
## #head(subset(Experiment4,Condition=="c"),n=10)
## #postcritical region
## #E4postcrit$intr.au<-ifelse(E4postcrit$gramm=="ungram" & E4postcrit$match=="match",-1/2,
## #                           ifelse(E4postcrit$gramm=="ungram" & E4postcrit$match=="mismatch",1/2,0))
## E4postcrit<-subset(E4postcrit,gramm=="ungram")
## E4postcrit$Condition<-factor(E4postcrit$Condition)
## E4postcrit$x<-ifelse(E4postcrit$Condition=="c",-1,1)
## E4postcrit$int<-ifelse(E4postcrit$Condition=="c","low","high")
## E4postcrit<-E4postcrit[,c(4,3,8,13,12)]
## E4postcrit$expt<-factor("wagersE4")
## colnames(E4postcrit)<-c("subj","item","rt","int","x","expt")
## wagersE4<-E4postcrit
## 
## # E5
## E5postcrit<-subset(Experiment5,Region==8) ##postcritical region
## nsubj_wagerse5<-length(unique(E5postcrit$Subj))
## E5postcrit<-subset(E5postcrit,gramm=="ungram")
## E5postcrit$Condition<-factor(E5postcrit$Condition)
## ## c,d
## E5postcrit$x<-ifelse(E5postcrit$Condition=="c",-1,1)
## E5postcrit$int<-ifelse(E5postcrit$Condition=="c","low","high")
## E5postcrit<-E5postcrit[,c(4,3,8,13,12)]
## colnames(E5postcrit)<-c("subj","item","rt","int","x")
## E5postcrit$expt<-factor("wagersE5")
## wagersE5<-E5postcrit
## 
## #head(wagersE5)
## 
## dat<-rbind(dillonE1,wagersE2,
##            lagoE1,lagoE2,
##            lagoE3a,lagoE3b,
##            wagersE2,
##            wagersE3pl,wagersE3sg,
##            wagersE4,wagersE5)
## dat$subj<-factor(paste(dat$expt,dat$subj,sep=""))
## dat$item<-factor(paste(dat$expt,dat$item,sep=""))
## #with(dat,tapply(subj,expt,function(x)length(unique(x))))
## ## combined data:
## logmagrmtattrn<-lmer(log(rt)~x + (1+x|subj)+(1+x|item),dat)
## #summary(logmagrmtattrn)


## ----agrmtattrn2,echo=FALSE,eval=FALSE,warning=FALSE,message=FALSE,results='asis',include=FALSE,cache=TRUE----
## ## Dillon E1:
## stanDat<-createStanDat(d=dillonE1,
##                        rt=dillonE1$rt,
##                        form=as.formula("~ 1 + x"))
## #str(stanDat)
## DillonE1 <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##              data = stanDat,
##              iter = 2000,
##              chains = 4)
## ## Int is Interference:
## pars<-c("Int","beta[2]","sigma_u[1]","sigma_u[2]","sigma_w[1]","sigma_w[2]","sigma_e")
## DillonE1_res<-stan_results(DillonE1,params=pars[1])
## 
## DillonE1postInt<-as.data.frame(DillonE1)$Int
## 
## stanDat<-createStanDat(d=lagoE1,
##                        rt=lagoE1$rt,
##                        form=as.formula("~ 1 + x"))
## 
## LagoE1 <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                  data = stanDat,
##                  iter = 2000,
##                  chains = 4)
## LagoE1_res<-stan_results(LagoE1,params=pars[1])
## 
## LagoE1postInt<-as.data.frame(LagoE1)$Int
## 
## 
## stanDat<-createStanDat(d=lagoE2,
##                        rt=lagoE2$rt,
##                        form=as.formula("~ 1 + x"))
## 
## LagoE2 <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                data = stanDat,
##                iter = 2000,
##                chains = 4)
## LagoE2_res<-stan_results(LagoE2,params=pars[1])
## 
## LagoE2postInt<-as.data.frame(LagoE2)$Int
## 
## stanDat<-createStanDat(d=lagoE3a,
##                           rt=lagoE3a$rt,
##                        form=as.formula("~ 1 + x"))
## 
## LagoE3a <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                data = stanDat,
##                iter = 2000,
##                chains = 4)
## 
## LagoE3a_res<-stan_results(LagoE3a,params=pars[1])
## 
## LagoE3apostInt<-as.data.frame(LagoE3a)$Int
## 
## 
## stanDat<-createStanDat(d=lagoE3b,
##                           rt=lagoE3b$rt,
##                        form=as.formula("~ 1 + x"))
## 
## LagoE3b <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                 data = stanDat,
##                 iter = 2000,
##                 chains = 4)
## 
## LagoE3b_res<-stan_results(LagoE3b,params=pars[1])
## 
## LagoE3bpostInt<-as.data.frame(LagoE3b)$Int
## 
## 
## stanDat<-createStanDat(d=wagersE2,
##                           rt=wagersE2$rt,
##                        form=as.formula("~ 1 + x"))
## 
## WagersE2 <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                 data = stanDat,
##                 iter = 2000,
##                 chains = 4)
## WagersE2_res<-stan_results(WagersE2,params=pars[1])
## 
## WagersE2postInt<-as.data.frame(WagersE2)$Int
## 
## stanDat<-createStanDat(d=wagersE3pl,
##                           rt=wagersE3pl$rt,
##                        form=as.formula("~ 1 + x"))
## 
## WagersE3pl <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                  data = stanDat,
##                  iter = 2000,
##                  chains = 4)
## WagersE3pl_res<-stan_results(WagersE3pl,params=pars[1])
## 
## WagersE3plpostInt<-as.data.frame(WagersE3pl)$Int
## 
## 
## stanDat<-createStanDat(d=wagersE3sg,
##                           rt=wagersE3sg$rt,
##                           form=as.formula("~ 1 + x"))
## 
## WagersE3sg <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                    data = stanDat,
##                    iter = 2000,
##                    chains = 4)
## WagersE3sg_res<-stan_results(WagersE3sg,params=pars[1])
## 
## WagersE3sgpostInt<-as.data.frame(WagersE3sg)$Int
## 
## 
## stanDat<-createStanDat(d=wagersE4,
##                           rt=wagersE4$rt,
##                           form=as.formula("~ 1 + x"))
## 
## WagersE4 <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                    data = stanDat,
##                    iter = 2000,
##                    chains = 4)
## WagersE4_res<-stan_results(WagersE4,params=pars[1])
## 
## WagersE4postInt<-as.data.frame(WagersE4)$Int
## 
## 
## stanDat<-createStanDat(d=wagersE5,
##                        rt=wagersE5$rt,
##                        form=as.formula("~ 1 + x"))
## 
## WagersE5 <- stan(file = "StanModels/maxModelTargetMismatch.stan",
##                  data = stanDat,
##                  iter = 2000,
##                  chains = 4)
## WagersE5_res<-stan_results(WagersE5,params=pars[1])
## 
## WagersE5postInt<-as.data.frame(WagersE5)$Int
## 
## 
## ## cunnings and sturt 2018
## CS18E1<-c(-22,-42,-4)
## 
## CS18E2<-c(-19,-40,1)
## 
## posteriors<-data.frame(expt=factor(1:12),rbind(DillonE1_res,LagoE1_res,LagoE2_res,LagoE3a_res,LagoE3b_res,WagersE2_res,WagersE3pl_res,WagersE3sg_res,WagersE4_res,WagersE5_res,CS18E1,CS18E2))
## 
## ## order expts to match posteriors:
## dat$expt<-factor(dat$expt,levels=c("dillonE1","lagoE1","lagoE2","lagoE3a","lagoE3b","wagersE2","wagersE3pl","wagersE3sg","wagersE4","wagersE5","CS18E1","CS18E2"))
## #levels(dat$expt)
## 
## n_subj<-as.vector(with(dat,tapply(subj,expt,function(x)length(unique(x)))))
## n_item<-as.vector(with(dat,tapply(item,expt,function(x)length(unique(x)))))
## n_subj[11:12]<-c(48,48)
## n_item[11:12]<-c(32,32)
## 
## posteriors$n_subj<-n_subj
## posteriors$n_item<-n_item
## ## width of credible interval
## posteriors$width<-posteriors$upper-posteriors$lower
## 
## ## reorder by mean magnitude:
## posteriors<-posteriors[with(posteriors,order(mean)),]
## 
## ## reorder expt by mean's magnitude:
## posteriors$expt <- factor(posteriors$expt,levels=posteriors$expt[order(posteriors$mean)])
## save(posteriors,file="data/posteriorsTargetMismatch.Rda")
## #xtable(posteriors)
## 
## ## meta-analysis:
## stan_dat<-list(y=posteriors$mean,
## s=posteriors$width/4,
## n=dim(posteriors)[1])
## 
## ma_model <- stan(file='StanModels/rema2.stan', data=stan_dat,
##             iter=2000, chains=4, seed=987654321,
##             control = list(adapt_delta = 0.99))
## 
## postsummary<-summary(ma_model,pars="mu")$summary[c(1,4,8)]
## 
## paramnames<-c("mu")
## 
## ma_mean<-round(postsummary[1])
## ma_lower<-round(postsummary[2])
## ma_upper<-round(postsummary[3])


## ----targetmismatchmetaanalysis,eval=FALSE,echo=FALSE--------------------
## ACTRpredictionmismatch[2]<-mean(mismatch_reduced$Effect)
## 
## load(file="data/posteriorsTargetMismatch.Rda")
## posteriors$expt<-as.numeric(as.character(posteriors$expt))
## posteriors<-rbind(posteriors,
##       c(13,ma_mean,ma_lower,ma_upper,NA,NA,NA),
##       c(14,ACTRpredictionmismatch[2],ACTRpredictionmismatch[1],ACTRpredictionmismatch[3],NA,NA,NA))
## 
## posteriors$data_model<-c(rep("data",12),"meta-analysis","model")
## 
## posteriors$expt<-factor(posteriors$expt)
## 
## exptlevels<-as.numeric(as.character(posteriors$expt[1:12]))
## 
## posteriors$expt<-factor(posteriors$expt,levels=c(14,13,exptlevels))
## 
## pd<-position_dodge(0.6)
## 
## ## need to fix this:
## plot_targetmismatch<-ggplot(posteriors, aes(x=expt,
##                                             y=mean,
##                              group=expt,shape=data_model)) +
##   geom_errorbar(aes(ymin=lower, ymax=upper),
##                 width=.25, size=.5, position=pd) +
##   scale_x_discrete(labels=c("LV05 model","meta-analysis",levels(posteriors$expt)[3:14]))+
##   labs(title="Facilitatory interference effects \n in reading studies") +
##   #scale_x_continuous(limits = c(0, 11))+
##   xlab("Experiment id")+
##   ylab("(total) reading time estimates in ms")+
##     theme_bw()+
##   scale_shape_discrete(labels=c("data",
##                                 "model"))+
##   #theme(legend.position=c(0.8,0.2)) +
##   #theme(legend.title=element_text("data / model"))+
##   #guide_legend(title="data / model")+
##   geom_hline(yintercept=0)+
##   geom_point(position=pd, size=2)+theme(legend.position = "none")+magnifytext(sze=18)+theme(plot.title = element_text(size=18, hjust = 0.5),
##        axis.text.y=element_text(size=18),
##        axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=18))+annotate("segment", x=2, xend=2, y=posteriors[13,]$lower, yend=posteriors[13,]$upper,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")),colour="red")
## #+coord_flip()


## ----loadaphasiaresults,echo=FALSE---------------------------------------
## source: MaetzigEtAlICCM2017.Rmd
load("data/results_SR_controls.Rda")
load("data/results_OR_controls.Rda")
load("data/results_SR_iwa.Rda")
load("data/results_OR_iwa.Rda")


## ----maetzigplots,echo=FALSE,eval=FALSE----------------------------------
## ## ggplots:
## ga_SR<-c(results_SR_controls$GA,results_SR_iwa$GA)
## ga_SR_subj<-factor(c(rep("control",length(results_SR_controls$GA)),rep("iwa",length(results_SR_iwa$GA))))
## ga_SR_df<-data.frame(participant=ga_SR_subj,ga_SR)
## SR_ga_plot<-ggplot(ga_SR_df, aes(x=ga_SR, fill=participant)) + geom_density(alpha=.3)+geom_line(stat="density")+theme_bw()+
##   xlab("")+
##   ggtitle("Subject relative")+theme(legend.title=element_blank(), legend.position = 'top')+theme(axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=14))+theme(
##     legend.position = c(.6, .95),
##     legend.justification = c("right", "top"),
##     legend.box.just = "right",
##     legend.margin = margin(6, 6, 6, 6)
##     )+magnifytext(sze=14)
## 
## ans_SR<-c(results_SR_controls$ANS,results_SR_iwa$ANS)
## ans_SR_subj<-factor(c(rep("control",length(results_SR_controls$GA)),rep("iwa",length(results_SR_iwa$GA))))
## ans_SR_df<-data.frame(participant=ans_SR_subj,ans_SR)
## SR_ans_plot<-ggplot(ans_SR_df, aes(x=ans_SR, fill=participant)) + geom_density(alpha=.3)+geom_line(stat="density")+theme_bw()+
##   xlab("")+ylab("")+
##   ggtitle("Subject relative") + theme(legend.title=element_blank(), legend.position = 'none') + theme(axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=14))+magnifytext(sze=14)
## 
## dat_SR<-c(results_SR_controls$DAT,results_SR_iwa$DAT)
## dat_SR_subj<-factor(c(rep("control",length(results_SR_controls$GA)),rep("iwa",length(results_SR_iwa$GA))))
## dat_SR_df<-data.frame(participant=dat_SR_subj,dat_SR)
## SR_dat_plot<-ggplot(dat_SR_df, aes(x=dat_SR, fill=participant)) + geom_density(alpha=.3)+geom_line(stat="density")+theme_bw()+
##   xlab("")+ylab("")+
##   ggtitle("Subject relative")+theme(legend.title=element_blank(), legend.position = 'none')+theme(axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=14))+magnifytext(sze=14)
## 
## ga_OR<-c(results_OR_controls$GA,results_OR_iwa$GA)
## ga_OR_subj<-factor(c(rep("control",length(results_OR_controls$GA)),rep("iwa",length(results_OR_iwa$GA))))
## ga_OR_df<-data.frame(participant=ga_OR_subj,ga_OR)
## OR_ga_plot<-ggplot(ga_OR_df, aes(x=ga_OR, fill=participant)) + geom_density(alpha=.3)+geom_line(stat="density")+theme_bw()+xlab("goal activation \n (resource reduction)")+ggtitle("Object relative") +  theme(legend.title=element_blank(), legend.position = 'none') + theme(axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=14))+magnifytext(sze=14)
## 
## ans_OR<-c(results_OR_controls$ANS,results_OR_iwa$ANS)
## ans_OR_subj<-factor(c(rep("control",length(results_OR_controls$GA)),rep("iwa",length(results_OR_iwa$GA))))
## ans_OR_df<-data.frame(participant=ans_OR_subj,ans_OR)
## OR_ans_plot<-ggplot(ans_OR_df, aes(x=ans_OR, fill=participant)) + geom_density(alpha=.3)+geom_line(stat="density")+theme_bw()+xlab("activation noise \n (intermittent deficiency)")+ylab("")+ggtitle("Object relative") + theme(legend.title=element_blank(), legend.position = 'none') + theme(axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=14))+magnifytext(sze=14)
## 
## dat_OR<-c(results_OR_controls$DAT,results_OR_iwa$DAT)
## dat_OR_subj<-factor(c(rep("control",length(results_OR_controls$GA)),rep("iwa",length(results_OR_iwa$GA))))
## dat_OR_df<-data.frame(participant=dat_OR_subj,dat_OR)
## OR_dat_plot<-ggplot(dat_OR_df, aes(x=dat_OR, fill=participant)) + geom_density(alpha=.3)+geom_line(stat="density")+theme_bw()+xlab("default action time \n (slowed processing)")+ylab("")+ggtitle("Object relative")+theme(legend.title=element_blank(), legend.position = 'none')+theme(axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=14))+magnifytext(sze=14)
## 
## multiplot(SR_ga_plot,SR_ans_plot,SR_dat_plot,
##           OR_ga_plot,OR_ans_plot,OR_dat_plot,cols=3)


## ----dillonloaddata, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# Bayesian analysis of original Experiment 1 from Dillon et al 2013
orig <- read.table("data/data_experiment1_jml.txt",header=TRUE)
orig$subj <- factor(orig$subj)
orig$item <- factor(orig$item)
orig$cond <- factor(orig$cond)
# rename conditions to match our data
levels(orig$cond) <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
# cast data to wide format to have the same data structure as in the analysis of our data above
orig <- dcast(orig, subj+item+cond+region ~ fixationtype, value.var = "value")

orig <- orig[, c('subj', 'item', 'cond', 'region', 'fp', 'tt', 'pr')]
# use same col names as for our data
colnames(orig) <- c('subj', 'item', 'cond', 'roi', 'FPRT', 'TFT', 'FPR')


## ----dillonvars,echo=FALSE, warning=TRUE, error=TRUE, message=TRUE-------
Nsubj_orig <- length(unique(factor(orig$subj)))
Nitem_orig <- length(unique(factor(subset(orig, cond!='filler')$item)))


## ----dilloncontrasts, include=TRUE,  echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
# Condition Labels (using our labels; same contrasts as above in analysis of our data); for documentation of contrasts see above. 

orig$Dep <- ifelse(orig$cond %in% c('a', 'b', 'c', 'd'), .5, -.5) # main effect of dependency type: agr=0.5, refl=-0.5
orig$Gram <- ifelse(orig$cond %in% c('a', 'b', 'e', 'f'), -.5, .5) # main effect of grammaticality: gram=-.5, ungram=.5
orig$Int_gram <- ifelse(orig$cond %in% c('a','e'), .5, ifelse(orig$cond %in% c('b', 'f'), -.5, 0) ) # interference in grammatical sentences: distr-match=0.5, distr-mismatch=-0.5
orig$Int_ungram <- ifelse(orig$cond %in% c('d', 'h'), .5, ifelse(orig$cond %in% c('c', 'g'), -.5, 0)) # interference in ungrammatical sentences: distr-match=0.5, distr-mismatch=-0.5
orig$DepxInt_gram <-ifelse(orig$cond %in% c('a', 'f'), .5, ifelse(orig$cond %in% c('b', 'e'), -.5, 0))
orig$DepxInt_ungram <- ifelse(orig$cond %in% c('d', 'g'), .5, ifelse(orig$cond %in% c('c', 'h'), -.5, 0))
orig$DepxGram <- ifelse(orig$cond %in% c('c', 'd', 'e', 'f'), .5, -.5)

#orig$DepxInt <- ifelse(orig$cond %in% c('a', 'd', 'f', 'g'), 0.5, -0.5)
#orig$Int <- ifelse(orig$cond %in% c('a', 'd', 'e', 'h'), 0.5, -0.5) # main effect of interference: int=0.5, no int=-0.5
#orig$GramxInt <- ifelse(orig$cond %in% c('b', 'd', 'f', 'h'),0.5,-0.5)
#orig$DepxGramxInt <- ifelse(orig$cond %in% c('b', 'd', 'e', 'g'), 0.5, -0.5)

orig$Int_gram_refl <- ifelse(orig$cond %in% c('e'), .5, ifelse(orig$cond %in% c('f'), -.5, 0))
orig$Int_gram_agr <- ifelse(orig$cond %in% c('a'), .5, ifelse(orig$cond %in% c('b'), -.5, 0))
orig$Int_ungram_refl <- ifelse(orig$cond %in% c('h'), .5, ifelse(orig$cond %in% c('g'), -.5, 0))
orig$Int_ungram_agr <- ifelse(orig$cond %in% c('d'), .5, ifelse(orig$cond %in% c('c'), -.5, 0))


## ----dilloncrit, include=TRUE,echo=FALSE, warning=TRUE, error=TRUE, message=TRUE,eval=FALSE----
## # in original data, critical region is
## crit_orig <- subset(orig, roi==5 & cond!='filler')
## summary(crit_orig)


## ----dillonanalysis,echo=FALSE,eval=FALSE--------------------------------
## priors_uninf<-c(set_prior("normal(0,10)",
##                     class = "Intercept"),
##           set_prior("normal(0,1)",
##                     class = "b"),
##                       set_prior("normal(0,1)",
##                                 class = "sd"),
##           set_prior("lkj(2)", class = "cor"))
## 
## m1<-brm(TFT~1+Dep+Gram+DepxGram+ Int_gram_refl  + Int_gram_agr +  Int_ungram_refl + Int_ungram_agr + (1+Dep+Gram+DepxGram+ Int_gram_refl  + Int_gram_agr +  Int_ungram_refl + Int_ungram_agr|subj) + (1+Dep+Gram+DepxGram+ Int_gram_refl  + Int_gram_agr +  Int_ungram_refl + Int_ungram_agr|item),
##         prior=priors_uninf, data= crit_orig,
##         family=lognormal(),
##             control = list(adapt_delta = 0.99,
##                            max_treedepth=15))
## save(m1,file="data/dillonE1m1.Rda")


## ----dillonplot,echo=FALSE,eval=FALSE,fig.height=8-----------------------
## ## load pre-fitted model
## load("data/dillonE1m1.Rda")
## #summary(m1)
## 
## ## figure out which columns need to be extracted in ranefs:
## predictors<-dimnames(fixef(m1))[[1]]
## ru_id<-which(predictors=="Int_ungram_refl")-1
## au_id<-which(predictors=="Int_ungram_agr")-1
## 
## post<-posterior_samples(m1)
## ## extract estimates:
## alpha<-post$b_Intercept
## beta_ru<-post$b_Int_ungram_refl
## beta_au<-post$b_Int_ungram_agr
## 
## ## mean int effect in ungram refl:
## meanru<-exp(alpha+beta_ru)-exp(alpha-beta_ru)
## meanru_quantile<-quantile(meanru,prob=c(0.025,0.975))
## 
## ## mean int effect in ungram agr:
## meanau<-exp(alpha+beta_au)-exp(alpha-beta_au)
## meanau_quantile<-quantile(meanau,prob=c(0.025,0.975))
## 
## subj_re<-posterior_samples(m1,"^r_subj")
## 
## subjlist<- 1:40
## nsubj<-length(subjlist)
## 
## subjdiff_ru<-subjdiff_au<-matrix(rep(NA,nsubj*4000),nrow=nsubj)
## 
## for(i in 1:nsubj){
## subjdiff_ru[i,] <- exp(alpha + subj_re[,i]  + (beta_ru+subj_re[,i+ru_id*nsubj])) - exp(alpha + subj_re[,i] - (beta_ru+subj_re[,i+ru_id*nsubj]))
## subjdiff_au[i,] <- exp(alpha + subj_re[,i]  + (beta_au+subj_re[,i+au_id*nsubj])) - exp(alpha + subj_re[,i] - (beta_au+subj_re[,i+au_id*nsubj]))
## }
## 
## ## ru target plot:
## subjdiff_ru<-t(subjdiff_ru)
## subjdiff_ru<-as.data.frame(subjdiff_ru)
## colnames(subjdiff_ru)<-factor(subjlist)
## mns_ru <- colMeans(subjdiff_ru)
## subjdiff_ru<-subjdiff_ru[,order(mns_ru)]
## colnames(subjdiff_ru)<-factor(subjlist)
## 
## ## model predictions See Vasishth and Engelmann book
## ## source of data: ABC_MethodsX_JML_Vasishth2019 on github.
## #load("data/match_reduced.Rda")
## #load("data/mismatch_reduced.Rda")
## load("data/au_predicted_meansD13.Rda")
## str(au_predicted_means)
## #ACTRpredictionmatch<-quantile(match_reduced$Effect,prob=c(0.025,0.5,0.975))
## #ACTRpredictionmatch[2]<-mean(match_reduced$Effect)
## ACTRpredictionmismatch<-quantile(au_predicted_means,prob=c(0.025,0.5,0.975))
## ACTRpredictionmismatch[2]<-mean(au_predicted_means)
## 
## plot_ruDillonE1<-mcmc_intervals(subjdiff_ru,prob_outer = 0.95, point_est="mean")+
##   geom_vline(xintercept=0)+#xlim(-300,200)+
##   xlab("Interference effect (ms)")+
##   magnifytext(sze=16)+
##   theme(axis.text.y = element_blank())+
##   ylab("participant")+
##   theme(axis.title.y = element_text(angle = 90))+
##   ggtitle("Dillon et al 2013 Expt 1: \n Ungrammatical reflexives")+
##   annotate("segment", x=ACTRpredictionmismatch[1], xend=ACTRpredictionmismatch[3], y=43, yend=43,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-30, y=44, label="Model prediction",family="serif",
##                  fontface="bold", colour="darkred", size=5)+
##   annotate("segment", x=meanru_quantile[1], xend=meanru_quantile[2], y=41, yend=41,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-20, y=41.7, label="Observed effect",family="serif",fontface="bold", colour="darkred", size=5)
## 
## ## au target plot:
## subjdiff_au<-t(subjdiff_au)
## subjdiff_au<-as.data.frame(subjdiff_au)
## colnames(subjdiff_au)<-factor(subjlist)
## mns_au <- colMeans(subjdiff_au)
## subjdiff_au<-subjdiff_au[,order(mns_au)]
## colnames(subjdiff_au)<-factor(subjlist)
## 
## plot_auDillonE1<-mcmc_intervals(subjdiff_au,prob_outer = 0.95, point_est="mean")+
##   geom_vline(xintercept=0)+#xlim(-300,200)+
##   xlab("Interference effect (ms)")+
##   magnifytext(sze=16)+
##   theme(axis.text.y = element_blank())+
##   ylab("participant")+
##   theme(axis.title.y = element_text(angle = 90))+
##   #ggtitle("Dillon et al 2013 Expt 1: \n Ungrammatical agreement")+
##   annotate("segment", x=ACTRpredictionmismatch[1], xend=ACTRpredictionmismatch[3], y=43, yend=43,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-30, y=44, label="Model prediction",family="serif",
##                  fontface="bold", colour="darkred", size=5)+
##   annotate("segment", x=meanau_quantile[1], xend=meanau_quantile[2], y=41, yend=41,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-120, y=41.7, label="Observed effect",family="serif",fontface="bold", colour="darkred", size=5)
## 
## plot_auDillonE1


## ----CS18E1inddiffplot,echo=FALSE,eval=FALSE-----------------------------
## load("data/M1_tft_CS18.Rda")
## post<-posterior_samples(M1_tft_CS18)
## ## extract estimates:
## alpha<-post$b_Intercept
## beta_pl<-post$b_plaus_int
## beta_impl<-post$b_implaus_int
## 
## ## mean int effect in plausible target:
## meanplaus<-exp(alpha+beta_pl)-exp(alpha-beta_pl)
## meanplaus_quantile<-quantile(meanplaus,prob=c(0.025,0.975))
## 
## ## mean int effect in implausible target:
## meanimplaus<-exp(alpha+beta_impl)-exp(alpha-beta_impl)
## meanimplaus_quantile<-quantile(meanimplaus,prob=c(0.025,0.975))
## 
## subj_re<-posterior_samples(M1_tft_CS18,"^r_subj")
## 
## subjlist<- 1:48
## nsubj<-length(subjlist)
## 
## subjdiff_pl<-subjdiff_impl<-matrix(rep(NA,nsubj*4000),nrow=nsubj)
## 
## for(i in 1:nsubj){
## subjdiff_pl[i,] <- exp(alpha + subj_re[,i]  + (beta_pl+subj_re[,i+nsubj])) - exp(alpha + subj_re[,i] - (beta_pl+subj_re[,i+nsubj]))
## subjdiff_impl[i,] <- exp(alpha + subj_re[,i]  + (beta_impl+subj_re[,i+2*nsubj])) - exp(alpha + subj_re[,i] - (beta_impl+subj_re[,i+2*nsubj]))
## }
## 
## ## plausible target plot:
## subjdiff_pl<-t(subjdiff_pl)
## subjdiff_pl<-as.data.frame(subjdiff_pl)
## colnames(subjdiff_pl)<-factor(subjlist)
## mns_pl <- colMeans(subjdiff_pl)
## subjdiff_pl<-subjdiff_pl[,order(mns_pl)]
## 
## plot_plE1<-mcmc_areas(subjdiff_pl,prob_outer = 0.95, point_est="mean")+
##   geom_vline(xintercept=0)+xlim(-300,200)+
##   xlab("Interference effect (ms)")+
##   magnifytext(sze=16)+
##   #theme(axis.text.y = element_blank())+
##   ylab("subject")+
##   theme(axis.title.y = element_text(angle = 90))+
##   ggtitle("Plausible target")+
##   annotate("segment", x=ACTRpredictionmatch[1], xend=ACTRpredictionmatch[3], y=49, yend=49,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=35, y=50, label="Predicted range",family="serif",
##                  fontface="bold", colour="darkred", size=5)+
##   annotate("segment", x=meanplaus_quantile[1], xend=meanplaus_quantile[2], y=0, yend=0,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-7, y=-1, label="Observed effect",family="serif",fontface="bold", colour="darkred", size=5)
## 
## ## implausible target plot:
## subjdiff_impl<-t(subjdiff_impl)
## subjdiff_impl<-as.data.frame(subjdiff_impl)
## colnames(subjdiff_impl)<-subjlist
## mns_impl <- colMeans(subjdiff_impl)
## subjdiff_impl<-subjdiff_impl[,order(mns_impl)]
## 
## plot_implE1<-mcmc_areas(subjdiff_impl)+
##   geom_vline(xintercept=0)+xlim(-300,200)+
##   xlab("Interference effect (ms)")+
##   magnifytext(sze=16)+
##   #theme(axis.text.y = element_blank())+
##   ylab("subject")+
##   theme(axis.title.y = element_text(angle = 90))+
##   ggtitle("Implausible target")+
##   annotate("segment", x=ACTRprediction[1], xend=ACTRprediction[3], y=49, yend=49,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-20, y=50, label="Predicted range",family="serif",
##                  fontface="bold", colour="darkred", size=5)+
##   annotate("segment", x=meanimplaus_quantile[1], xend=meanimplaus_quantile[2], y=0, yend=0,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-10, y=-1, label="Observed effect",family="serif",fontface="bold", colour="darkred", size=5)


## ----mixtureprocessillustration,echo=FALSE,eval=FALSE--------------------
## nsim<-100000
## A<-rlnorm(nsim,mean=6,0.2)
## B<-rlnorm(nsim,mean=6.3,0.2)
## p<-0.2
## AB<-rep(NA,nsim)
## for(i in 1:nsim){
##   ## toss coin:
##   toss<-rbinom(1,size=1,prob=p)
##   if(toss==1){
## ## sample from B
## AB[i]<-rlnorm(1,mean=6.3,sd=.2)} else {
##   ## sample from A
## AB[i]<-rlnorm(1,mean=6,sd=.2)}
## }
## 
## mixturedat<-data.frame(process=rep(c("A~LogNormal(6,0.2)",
##                                      "B~LogNormal(6.3,0.2)",
##                                      "Mixture"),
##                                    each=nsim),
##                                    y=c(A,B,AB))
## 
## ggplot(mixturedat, aes(x=y, fill=process)) + geom_density(alpha=.3)+geom_line(stat="density")+theme_bw()+
##   xlab("observed reading times (ms)")+
##   ggtitle("A two-component mixture")+theme(legend.title=element_blank(), legend.position = 'top')+
##   theme(axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=14))+theme(
##     legend.position = c(.75, .95),
##     legend.justification = c("right", "top"),
##     legend.box.just = "right",
##     legend.margin = margin(6, 6, 6, 6)
##     ) +
##   magnifytext(sze=14)


## ----predictions,eval=FALSE,echo=FALSE,fig.height=7----------------------
## ## cue weight 1:
## load("data/lv05pspaceEVcuewt1.Rd")
## means1<-lv05pspaceEVcuewt1
## ## cue weight 2:
## load("data/lv05pspaceEVcuewt2.Rd")
## means2<-lv05pspaceEVcuewt2
## ## cue weight 4:
## load("data/lv05pspaceEVcuewt4.Rd")
## means4<-lv05pspaceEVcuewt4
## 
## means<-rbind(means1,means2,means4)
## 
## means$Target <- factor(means$Target, labels=c("Target-Match",
##                                               "Target-Mismatch"))
## 
## match<-subset(means,Target=='Target-Match')
## mismatch<-subset(means,Target=='Target-Mismatch')
## 
## match_reduced<-match[,c(3,5,7,9,10,11,19)]
## #dim(match_reduced)
## #head(match_reduced)
## #summary(match_reduced)
## ## plot only the predictions for the LF parameter range actually used in EJV2019 paper.
## 
## match_reduced$ans<-factor(match_reduced$ans)
## #levels(match_reduced$ans)[levels(match_reduced$ans)==0.1]  <- "Noise: 0.1"
## #levels(match_reduced$ans)[levels(match_reduced$ans)==0.2]  <- "Noise: 0.2"
## #levels(match_reduced$ans)[levels(match_reduced$ans)==0.3]  <- "Noise: 0.3"
## 
## match_reduced$mp<-factor(match_reduced$mp)
## #levels(match_reduced$mp)[levels(match_reduced$mp)=="0.15"]  <- "Mismatch penalty: 0.15"
## #levels(match_reduced$mp)[levels(match_reduced$mp)=="0.25"]  <- "Mismatch penalty: 0.25"
## #levels(match_reduced$mp)[levels(match_reduced$mp)=="0.35"]  <- "Mismatch penalty: 0.35"
## 
## match_reduced$threshold<-factor(match_reduced$rth)
## #match_reduced$mas<-factor(match_reduced$mas)
## 
## match_reduced$cueweighting<-factor(match_reduced$cueweighting)
## levels(match_reduced$cueweighting)[levels(match_reduced$cueweighting)=="1"]  <- "Cue-weights: 1:1"
## levels(match_reduced$cueweighting)[levels(match_reduced$cueweighting)=="2"]  <- "Cue-weights: 2:1"
## levels(match_reduced$cueweighting)[levels(match_reduced$cueweighting)=="4"]  <- "Cue-weights: 4:1"
## 
## 
## plot_match<-ggplot(match_reduced, aes(x=lf,
##                                       y=Effect,
##                          color = mas)) +
##   #scale_shape_manual(values=c(0,16,2))+
##   #scale_colour_manual(values=c("gray10","gray30","gray60"))+
##   #scale_colour_gradient(low="gray10", high="gray60",trans="reverse")+
##   geom_point(size=3,position=position_jitter(width=0.01, height=0.01))+
##   #facet_wrap( ~ factor(ans)+factor(mp), nrow=3)+
##     theme_bw()+xlab("")+ylab("Interference  effect  (ms)")+theme(axis.title.y = element_text(angle = 90))+ggtitle("Grammatical")+theme(
##     legend.position = "none",
##     legend.justification = c("left", "top"),
##     legend.box.just = "left",
##     legend.margin = margin(6, 6, 6, 6)
##     ) + theme(plot.title = element_text(size=18, hjust = 0.5),
##        axis.text.y=element_text(size=18),
##        axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=18))+magnifytext(sze=14)
## 
## plot_match <- plot_match+facet_wrap( ~ cueweighting, ncol=3)+
##   theme(strip.text = element_text(face="bold", size=rel(1.5)),
##               strip.background = element_rect(fill="lightblue", colour="black",size=1))
## 
## ## mismatch data:
## mismatch_reduced<-mismatch[,c(3,5,7,9,10,11,19)]
## 
## mismatch_reduced$ans<-factor(mismatch_reduced$ans)
## #levels(mismatch_reduced$ans)[levels(mismatch_reduced$ans)==0.1]  <- "Noise: 0.1"
## #levels(mismatch_reduced$ans)[levels(mismatch_reduced$ans)==0.2]  <- "Noise: 0.2"
## #levels(mismatch_reduced$ans)[levels(mismatch_reduced$ans)==0.3]  <- "Noise: 0.3"
## 
## mismatch_reduced$mp<-factor(mismatch_reduced$mp)
## #levels(mismatch_reduced$mp)[levels(mismatch_reduced$mp)=="0.15"]  <- "Mismatch penalty: 0.15"
## #levels(mismatch_reduced$mp)[levels(mismatch_reduced$mp)=="0.25"]  <- "Mismatch penalty: 0.25"
## #levels(mismatch_reduced$mp)[levels(mismatch_reduced$mp)=="0.35"]  <- "Mismatch penalty: 0.35"
## 
## mismatch_reduced$threshold<-factor(mismatch_reduced$rth)
## #mismatch_reduced$mas<-factor(mismatch_reduced$mas)
## 
## mismatch_reduced$cueweighting<-factor(mismatch_reduced$cueweighting)
## levels(mismatch_reduced$cueweighting)[levels(mismatch_reduced$cueweighting)=="1"]  <- "Cue-weights: 1:1"
## levels(mismatch_reduced$cueweighting)[levels(mismatch_reduced$cueweighting)=="2"]  <- "Cue-weights: 2:1"
## levels(mismatch_reduced$cueweighting)[levels(mismatch_reduced$cueweighting)=="4"]  <- "Cue-weights: 4:1"
## 
## plot_mismatch<-ggplot(mismatch_reduced, aes(x=lf,
##                                       y=Effect,
##                          #shape = mas,
##                          color = mas)) +
##   #scale_shape_manual(values=c(0,16,2))+
##   #scale_colour_manual(values=c("gray10","gray30","gray60"))+
##   #scale_colour_gradient(low="gray10", high="gray60",trans="reverse")+
##   geom_point(size=3,position=position_jitter(width=0.01, height=0.01))+
##   #facet_wrap( ~ factor(ans)+factor(mp), nrow=3)+
##     theme_bw()+xlab("latency factor")+
##   ylab("Interference effect (ms)")+
##   theme(axis.title.y = element_text(angle = 90))+ggtitle("Ungrammatical")+
##   theme(
##     legend.position = c(.95, .7),
##     legend.justification = c("right", "top"),
##     legend.box.just = "left",
##     legend.margin = margin(6, 6, 6, 6)
##     ) + theme(plot.title = element_text(size=18, hjust = 0.5),
##        axis.text.y=element_text(size=18),
##        axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=18)) + magnifytext(sze=14)
## 
## plot_mismatch <- plot_mismatch+facet_wrap( ~ cueweighting, ncol=3)+
##   theme(strip.text = element_text(face="bold", size=rel(1.5)),
##               strip.background = element_rect(fill="lightblue", colour="black",size=1))
## 
## multiplot(plot_match,plot_mismatch,cols=1)

