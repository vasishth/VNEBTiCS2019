## R code to accompany the paper Computational models of retrieval processes in sentence processing by Vasishth, Engelmann, Nicenboim, Burchert 2019, submitted to Trends in Cognitive Sciences as an invited review.
## Date: 12 June 2019
## Author: Shravan Vasishth, vasishth@uni-potsdam.de
## Description: This code will reproduce all the analyses and figures in the paper. The commented out code can be uncommented to run it (some of it takes quite a lot of time to run, e.g., the Bayesian linear mixed models.)

## ----setup,include=FALSE,cache=FALSE,echo=FALSE--------------------------
library(MASS)
library(knitr)
library(xtable)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
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



## ----predictions,echo=FALSE,fig.height=8---------------------------------
## cue weight 1:
load("data/lv05pspaceEVcuewt1.Rd")
means1<-lv05pspaceEVcuewt1
## cue weight 2:
load("data/lv05pspaceEVcuewt2.Rd")
means2<-lv05pspaceEVcuewt2
## cue weight 4:
load("data/lv05pspaceEVcuewt4.Rd")
means4<-lv05pspaceEVcuewt4

means<-rbind(means1,means2,means4)

means$Target <- factor(means$Target, labels=c("Target-Match", 
                                              "Target-Mismatch"))

match<-subset(means,Target=='Target-Match')
mismatch<-subset(means,Target=='Target-Mismatch')

match_reduced<-match[,c(3,5,7,9,10,11,19)]
#dim(match_reduced)
#head(match_reduced)
#summary(match_reduced)
## plot only the predictions for the LF parameter range actually used in EJV2019 paper.

match_reduced$ans<-factor(match_reduced$ans)
#levels(match_reduced$ans)[levels(match_reduced$ans)==0.1]  <- "Noise: 0.1"
#levels(match_reduced$ans)[levels(match_reduced$ans)==0.2]  <- "Noise: 0.2"
#levels(match_reduced$ans)[levels(match_reduced$ans)==0.3]  <- "Noise: 0.3"

match_reduced$mp<-factor(match_reduced$mp)
#levels(match_reduced$mp)[levels(match_reduced$mp)=="0.15"]  <- "Mismatch penalty: 0.15"
#levels(match_reduced$mp)[levels(match_reduced$mp)=="0.25"]  <- "Mismatch penalty: 0.25"
#levels(match_reduced$mp)[levels(match_reduced$mp)=="0.35"]  <- "Mismatch penalty: 0.35"

match_reduced$threshold<-factor(match_reduced$rth)
#match_reduced$mas<-factor(match_reduced$mas)

match_reduced$cueweighting<-factor(match_reduced$cueweighting)
levels(match_reduced$cueweighting)[levels(match_reduced$cueweighting)=="1"]  <- "Cue-weights: 1:1"
levels(match_reduced$cueweighting)[levels(match_reduced$cueweighting)=="2"]  <- "Cue-weights: 2:1"
levels(match_reduced$cueweighting)[levels(match_reduced$cueweighting)=="4"]  <- "Cue-weights: 4:1"


plot_match<-ggplot(match_reduced, aes(x=lf, 
                                      y=Effect, 
                         color = mas)) +
  #scale_shape_manual(values=c(0,16,2))+
  #scale_colour_manual(values=c("gray10","gray30","gray60"))+
  #scale_colour_gradient(low="gray10", high="gray60",trans="reverse")+
  geom_point(size=3,position=position_jitter(width=0.01, height=0.01))+
  #facet_wrap( ~ factor(ans)+factor(mp), nrow=3)+
    theme_bw()+xlab("")+ylab("Interference  effect  (ms)")+theme(axis.title.y = element_text(angle = 90))+ggtitle("Grammatical")+theme(
    legend.position = "none",
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6)
    ) + theme(plot.title = element_text(size=18, hjust = 0.5),
       axis.text.y=element_text(size=18), 
       axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=18))+magnifytext(sze=14)

plot_match <- plot_match+facet_wrap( ~ cueweighting, ncol=3)+
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
              strip.background = element_rect(fill="lightblue", colour="black",size=1))

## mismatch data:
mismatch_reduced<-mismatch[,c(3,5,7,9,10,11,19)]

mismatch_reduced$ans<-factor(mismatch_reduced$ans)
#levels(mismatch_reduced$ans)[levels(mismatch_reduced$ans)==0.1]  <- "Noise: 0.1"
#levels(mismatch_reduced$ans)[levels(mismatch_reduced$ans)==0.2]  <- "Noise: 0.2"
#levels(mismatch_reduced$ans)[levels(mismatch_reduced$ans)==0.3]  <- "Noise: 0.3"

mismatch_reduced$mp<-factor(mismatch_reduced$mp)
#levels(mismatch_reduced$mp)[levels(mismatch_reduced$mp)=="0.15"]  <- "Mismatch penalty: 0.15"
#levels(mismatch_reduced$mp)[levels(mismatch_reduced$mp)=="0.25"]  <- "Mismatch penalty: 0.25"
#levels(mismatch_reduced$mp)[levels(mismatch_reduced$mp)=="0.35"]  <- "Mismatch penalty: 0.35"

mismatch_reduced$threshold<-factor(mismatch_reduced$rth)
#mismatch_reduced$mas<-factor(mismatch_reduced$mas)

mismatch_reduced$cueweighting<-factor(mismatch_reduced$cueweighting)
levels(mismatch_reduced$cueweighting)[levels(mismatch_reduced$cueweighting)=="1"]  <- "Cue-weights: 1:1"
levels(mismatch_reduced$cueweighting)[levels(mismatch_reduced$cueweighting)=="2"]  <- "Cue-weights: 2:1"
levels(mismatch_reduced$cueweighting)[levels(mismatch_reduced$cueweighting)=="4"]  <- "Cue-weights: 4:1"

plot_mismatch<-ggplot(mismatch_reduced, aes(x=lf, 
                                      y=Effect, 
                         #shape = mas, 
                         color = mas)) +
  #scale_shape_manual(values=c(0,16,2))+
  #scale_colour_manual(values=c("gray10","gray30","gray60"))+
  #scale_colour_gradient(low="gray10", high="gray60",trans="reverse")+
  geom_point(size=3,position=position_jitter(width=0.01, height=0.01))+
  #facet_wrap( ~ factor(ans)+factor(mp), nrow=3)+
    theme_bw()+xlab("latency factor")+
  ylab("Interference effect (ms)")+
  theme(axis.title.y = element_text(angle = 90))+ggtitle("Ungrammatical")+
  theme(
    legend.position = c(.95, .7),
    legend.justification = c("right", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6)
    ) + theme(plot.title = element_text(size=18, hjust = 0.5),
       axis.text.y=element_text(size=18), 
       axis.text.x  = element_text(angle=55,vjust=1,hjust=1, size=18)) + magnifytext(sze=14)

plot_mismatch <- plot_mismatch+facet_wrap( ~ cueweighting, ncol=3)+
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
              strip.background = element_rect(fill="lightblue", colour="black",size=1))

multiplot(plot_match,plot_mismatch,cols=1)


## ----modelpredictions,echo=FALSE-----------------------------------------
## model predictions See Vasishth and Engelmann book
load("data/match_reduced.Rda")
load("data/mismatch_reduced.Rda")
ACTRpredictionmatch<-quantile(match_reduced$Effect,prob=c(0.025,0.5,0.975))
ACTRpredictionmatch[2]<-mean(match_reduced$Effect)
ACTRpredictionmismatch<-quantile(mismatch_reduced$Effect,prob=c(0.025,0.5,0.975))
ACTRpredictionmismatch[2]<-mean(mismatch_reduced$Effect)




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
  magnifytext(sze=20)

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
## #multiplot(SR_ga_plot,SR_ans_plot,SR_dat_plot,
## #          OR_ga_plot,OR_ans_plot,OR_dat_plot,cols=3)


## ----powerplots,echo=FALSE,eval=FALSE------------------------------------
## nsim<-10000
## pow30<-pow40<-pow50<-pow60<-rep(NA,nsim)
## stddev <- 150
## d_mean <-30
## d_sd <- 10
## 
## for(i in 1:nsim){
##   d<-rnorm(1,mean=d_mean,sd=d_sd)
##   pow30[i]<-power.t.test(delta=d,n=30,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow40[i]<-power.t.test(delta=d,n=40,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow50[i]<-power.t.test(delta=d,n=50,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow60[i]<-power.t.test(delta=d,n=60,sd=stddev,type="one.sample",strict=TRUE)$power
## }
## 
## powerdistrn<-data.frame(nsubj=factor(rep(c(30,40,50,60),each=nsim)),power=c(pow30,pow40,pow50,pow60))
## 
## sd150<-ggplot(powerdistrn,
##        aes(x = power, y = nsubj, fill = nsubj, height = ..density..)) +
##   geom_density_ridges(scale = 4, stat = "density") +
##   scale_y_discrete(expand = c(0.01, 0)) +
##   #scale_x_continuous(expand = c(0.05, 0)) +
##   xlim(0.05,1)+
##   scale_fill_brewer(palette = "PuBuGn") +
##   theme_ridges() + theme(legend.position = "none")+
##   xlab("prospective statistical power")+
##   ggtitle("Power estimates \n (sd=150)")+
##   ylab("")+
##   magnifytext(sze=16)
## 
## ## sd 200
## pow30<-pow40<-pow50<-pow60<-rep(NA,nsim)
## stddev <- 200
## 
## for(i in 1:nsim){
##   d<-rnorm(1,mean=d_mean,sd=d_sd)
##   pow30[i]<-power.t.test(delta=d,n=30,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow40[i]<-power.t.test(delta=d,n=40,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow50[i]<-power.t.test(delta=d,n=50,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow60[i]<-power.t.test(delta=d,n=60,sd=stddev,type="one.sample",strict=TRUE)$power
## }
## 
## powerdistrn<-data.frame(nsubj=factor(rep(c(30,40,50,60),each=nsim)),power=c(pow30,pow40,pow50,pow60))
## 
## sd200<-ggplot(powerdistrn,
##        aes(x = power, y = nsubj, fill = nsubj, height = ..density..)) +
##   geom_density_ridges(scale = 4, stat = "density") +
##   scale_y_discrete(expand = c(0.01, 0)) +
##   #scale_x_continuous(expand = c(0.05, 0)) +
##   xlim(0.05,1)+
##   scale_fill_brewer(palette = "PuBuGn") +
##   theme_ridges() + theme(legend.position = "none")+
##   xlab("prospective statistical power")+
##   ggtitle("Power estimates \n (sd=200)")+
##   ylab("number of subjects")+
##   magnifytext(sze=16)
## 
## ## sd 250
## pow30<-pow40<-pow50<-pow60<-rep(NA,nsim)
## stddev <- 250
## 
## for(i in 1:nsim){
##   d<-rnorm(1,mean=d_mean,sd=d_sd)
##   pow30[i]<-power.t.test(delta=d,n=30,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow40[i]<-power.t.test(delta=d,n=40,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow50[i]<-power.t.test(delta=d,n=50,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow60[i]<-power.t.test(delta=d,n=60,sd=stddev,type="one.sample",strict=TRUE)$power
## }
## 
## powerdistrn<-data.frame(nsubj=factor(rep(c(30,40,50,60),each=nsim)),power=c(pow30,pow40,pow50,pow60))
## 
## sd250<-ggplot(powerdistrn,
##        aes(x = power, y = nsubj, fill = nsubj, height = ..density..)) +
##   geom_density_ridges(scale = 4, stat = "density") +
##   scale_y_discrete(expand = c(0.01, 0)) +
##   #scale_x_continuous(expand = c(0.05, 0)) +
##   xlim(0.05,1)+
##   scale_fill_brewer(palette = "PuBuGn") +
##   theme_ridges() + theme(legend.position = "none")+
##   xlab("")+
##   ggtitle("Power estimates \n (sd=250)")+
##   ylab("")+
##   magnifytext(sze=16)
## 
## # sd 300
## pow30<-pow40<-pow50<-pow60<-rep(NA,nsim)
## stddev <- 300
## 
## for(i in 1:nsim){
##   d<-rnorm(1,mean=d_mean,sd=d_sd)
##   pow30[i]<-power.t.test(delta=d,n=30,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow40[i]<-power.t.test(delta=d,n=40,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow50[i]<-power.t.test(delta=d,n=50,sd=stddev,type="one.sample",strict=TRUE)$power
##   pow60[i]<-power.t.test(delta=d,n=60,sd=stddev,type="one.sample",strict=TRUE)$power
## }
## 
## powerdistrn<-data.frame(nsubj=factor(rep(c(30,40,50,60),each=nsim)),power=c(pow30,pow40,pow50,pow60))
## 
## sd300<-ggplot(powerdistrn,
##        aes(x = power, y = nsubj, fill = nsubj, height = ..density..)) +
##   geom_density_ridges(scale = 4, stat = "density") +
##   scale_y_discrete(expand = c(0.01, 0)) +
##   #scale_x_continuous(expand = c(0.05, 0)) +
##   xlim(0.05,1)+
##   scale_fill_brewer(palette = "PuBuGn") +
##   theme_ridges() + theme(legend.position = "none")+
##   xlab("")+
##   ggtitle("Power estimates \n (sd=300, effect ~ Normal(30,10))")+
##   ylab("number of subjects")+
##   magnifytext(sze=16)
## 
## multiplot(sd300,sd250,
##           sd200,sd150,cols=2)


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


## ----dillonvars, include=TRUE, echo=FALSE, warning=TRUE, error=TRUE, message=TRUE----
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


## ----dillonplot,echo=FALSE,eval=FALSE------------------------------------
## ## load pre-fitted model
## load("data/dillonE1m1.Rda")
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
## load("data/match_reduced.Rda")
## load("data/mismatch_reduced.Rda")
## ACTRpredictionmatch<-quantile(match_reduced$Effect,prob=c(0.025,0.5,0.975))
## ACTRpredictionmatch[2]<-mean(match_reduced$Effect)
## ACTRpredictionmismatch<-quantile(mismatch_reduced$Effect,prob=c(0.025,0.5,0.975))
## ACTRpredictionmismatch[2]<-mean(mismatch_reduced$Effect)
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
##   annotate("text", x=-20, y=41.7, label="Mean effect",family="serif",fontface="bold", colour="darkred", size=5)
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
##   ggtitle("Dillon et al 2013 Expt 1: \n Ungrammatical agreement")+
##   annotate("segment", x=ACTRpredictionmismatch[1], xend=ACTRpredictionmismatch[3], y=43, yend=43,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-30, y=44, label="Model prediction",family="serif",
##                  fontface="bold", colour="darkred", size=5)+
##   annotate("segment", x=meanau_quantile[1], xend=meanau_quantile[2], y=41, yend=41,arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))+
##   annotate("text", x=-120, y=41.7, label="Mean effect",family="serif",fontface="bold", colour="darkred", size=5)
## 
## multiplot(plot_ruDillonE1,plot_auDillonE1,cols=2)


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
##   annotate("text", x=-7, y=-1, label="Mean effect",family="serif",fontface="bold", colour="darkred", size=5)
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
##   annotate("text", x=-10, y=-1, label="Mean effect",family="serif",fontface="bold", colour="darkred", size=5)


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

