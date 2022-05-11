# Analaysis of Curiosity experiment 1
# Tim Hollins
# There are three sets of analyses here, corresponding to the analyses reported Experiment 1 of the Curiosity paper


library(plyr)        # provides 'ddply'
library(Rmisc)       # provides 'summarySE'
library(ez)          # provides 'ezANOVA'
library(pwr)         # provides 'pwr.t.test'
library(ggplot2)     # provides 'ggplot'
library(BayesFactor) # provides 'ttestBF'
library(cowplot)     # provides 'plot_grid'
library(tidyverse)  # provides functionality
library(afex)       # provides ANOVA


recog <- read.csv("jack data long format.csv")

recog$Pos <-factor(recog$Pos)
recog$Guess <-factor(recog$Guess)
recog$subj <- factor(recog$subj)
recog$Trial <- factor(recog$Trial)

recog$PosGuess<-factor(recog$PosGuess)

recogmeans<-recog %>% group_by (Trial, Pos) %>%
  summarise(Acc = mean(Acc))
  recogmeans
recogSE<-recog %>% group_by (Guess) %>%
    summarise(Acc = sd(Acc)/ (length(Acc)^.5))
recogSE

# Run traditional and BF Anovas on the effect of Trial and Position on accuracy
recoganova<- aov_car(
  formula = Acc ~Error(subj/ Pos*Trial),
  data = recog
)
recoganova


recoganovaBF <- anovaBF(
  formula = Acc ~ Pos*Trial + subj,
  data = data.frame(recog),
  whichRandom = 'subj'
)
recoganovaBF
recoganovaBF[4]/recoganovaBF[3]

# Run analysis of just Guess items in Guess trials vs equivalent position in Study-only. 

# First select trials where guess position is the same as trial position
recog$matchtrial<- (as.numeric(recog$Pos) - as.numeric(recog$PosGuess))
recogmatch <- recog %>% subset(recog$matchtrial ==0)
# Just select the study trials
recogstudy <-recog %>% subset(recog$Guess =="S")
## Combine the guess and study trials
guessvsstudytrials<- rbind(recogmatch, recogstudy)

# Run traditional and BF Anovas on the effect of Trial and Position on accuracy only for guesses on guess trials

recogmeans<-guessvsstudytrials %>% group_by (Guess) %>%
  summarise(Acc = mean(Acc))
recogmeans
recogSE<-guessvsstudytrials %>% group_by (Guess) %>%
  summarise(Acc = sd(Acc)/ (length(Acc)^.5))
recogSE

recoganova<- aov_car(
  formula = Acc ~Error(subj/ Guess*Pos),
  data = guessvsstudytrials
)
recoganova


recoganovaBF <- anovaBF(
  formula = Acc ~ Guess*Pos + subj,
  data = data.frame(guessvsstudytrials),
  whichRandom = 'subj'
)
recoganovaBF
recoganovaBF[4]/recoganovaBF[3]

## Now select the study items on guess trials

recogmismatch <- recog %>% subset(recog$matchtrial !=0) 
recogmismatch <- recogmismatch %>% subset(recogmismatch$PosGuess ==2)
# Just select the study trials
recogstudy <-recogstudy%>% subset(recogstudy$Pos !=2)

Firstvsthird <- rbind(recogmismatch, recogstudy)

# Run traditional and BF Anovas on the effect of Trial and Position on accuracy only for guesses on guess trials

recogmeans<-Firstvsthird %>% group_by (Guess) %>%
  summarise(Acc = mean(Acc))
 recogmeans
 recogSE<-Firstvsthird %>% group_by (Guess) %>%
   summarise(Acc = sd(Acc)/ (length(Acc)^.5))
 recogSE
 
 
recoganova<- aov_car(
  formula = Acc ~Error(subj/ Guess*Pos),
  data = Firstvsthird
)
recoganova


recoganovaBF <- anovaBF(
  formula = Acc ~ Guess*Pos + subj,
  data = data.frame(Firstvsthird),
  whichRandom = 'subj'
)
recoganovaBF
recoganovaBF[4]/recoganovaBF[3]

