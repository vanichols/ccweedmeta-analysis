#############################
##
# Author: Gina
#
# Date Created: Sept 11 2019
#
# Date last modified: Oct 3 (added modifier analyses)
#                     Dec 6 updated and renamed output files
#
# Purpose: Fit linear models to get estimates of effect sizes
#
# Inputs: td_cc-database-clean-long
#
# Outputs: td_stats-overall, sd_lm-res, sd_lm-cont-res
#
# NOtes: need to write functino to test continuous variables
#
##############################


rm(list=ls())
library(lme4)
library(ggplot2)
library(tidyverse)
library(lmerTest)

source("_code/code_00_functions.R")

dat <- read_csv("_tidydata/td_cc-database-clean-long.csv")

den <- dat %>% filter(resp == "den")
bio <- dat %>% filter(resp == "bio")



# reverse fit density -----------------------------------------------------

densm <- 
  den %>% 
  filter(!is.na(cc_bio_kgha)) %>% 
  select(obs_no, study, wgt, LRR, cc_bio_kgha) %>% 
  filter(LRR > -5) %>% 
  rename("ccbio_kgha" = cc_bio_kgha,
         "lrr"= LRR) %>% 
  mutate(ccbio_Mgha = ccbio_kgha/1000)


densm %>% 
  ggplot(aes(ccbio_kgha)) + 
  geom_histogram()

densm %>% 
  ggplot(aes(ccbio_kgha, lrr)) + 
  geom_point() + 
  geom_smooth(method = "lm")


mymod <- lmer(lrr ~ ccbio_Mgha + (1|study), data = densm)
summary(mymod)
predict(mymod, re.form = NA)

# Separate the betas
betas <- as.vector(summary(mymod)$coefficients[,1])
b0 <- betas[1] 
b1 <- betas[2] 

# find lrr of weed density at 3 Mg/ha
predlrr <- ((b0 + 3*b1))

# what does this look like?
densm %>% 
  select(ccbio_Mgha) %>% 
  mutate(pred = predict(mymod)) %>% 
  arrange(ccbio_Mgha) %>% 
    ggplot(aes(ccbio_Mgha, pred)) + 
    geom_point() + 
    geom_vline(xintercept = 3)

densm %>% select(ccbio_Mgha) %>% 
    mutate(pred = predict(mymod)) %>% 
  filter(ccbio_Mgha >2.9)
    
#predict(mymod, newdata = df.predicted, re.form = NA)


# reverse fit to find biomass needed for 50% control ----------------------

biosm <- 
  bio %>% 
  filter(!is.na(cc_bio_kgha)) %>% 
  select(obs_no, study, wgt, LRR, cc_bio_kgha) %>% 
  filter(LRR > -5) %>% 
  rename("ccbio_kgha" = cc_bio_kgha,
         "lrr"= LRR) %>% 
  mutate(ccbio_Mgha = ccbio_kgha/1000)


#write_csv(biosm, "_tidydata/td_weedbio-for-KG.csv")


biosm %>% 
  ggplot(aes(ccbio_kgha)) + 
  geom_histogram()

biosm %>% 
  ggplot(aes(ccbio_kgha, lrr)) + 
  geom_point() + 
  geom_smooth(method = "lm")


mymod <- lmer(lrr ~ ccbio_Mgha + (1|study), data = biosm)
summary(mymod)
predict(mymod, re.form = NA)

biosm.new <- biosm %>% select(ccbio_Mgha)
#predict(mymod, newdata = df.predicted, re.form = NA)

forfig <- 
biosm %>% 
  mutate(preds = predict(mymod, newdata = biosm.new, re.form = NA))

forfig %>% write_csv("_tidydata/sd_bio-preds.csv")

ComputeSE(lrr = -0.69, mymod = mymod)
resid_panel(mymod, plots = "all")
resid_panel(mymod, plots = "yvp")
