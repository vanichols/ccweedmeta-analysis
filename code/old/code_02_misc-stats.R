#############################
##
# Purpose: Fit linear models to get estimates of effect sizes
# Author: Gina
# Date Created: Dec 26 2019
# Date last modified: 
#
# Inputs: td_cc-database-clean-long
#
# Outputs:
#
#
# NOtes:
#
##############################


rm(list=ls())
library(lme4)
library(tidyverse)
library(lmerTest)
library(janitor)
library(emmeans)

dat <- read_csv("_tidydata/td_cc-database-clean-long.csv")

den <- dat %>% filter(resp == "den") %>% filter(!is.na(LRR)) %>% mutate(cc_bio_Mgha = cc_bio_kgha/1000)
bio <- dat %>% filter(resp == "bio") %>% filter(!is.na(LRR)) %>% mutate(cc_bio_Mgha = cc_bio_kgha/1000)


# mean biomass grass and non ----------------------------------------------

# need to eliminate repeats? 
dat_ccbio <- read_csv("_tidydata/td_cc-database-clean.csv") %>% 
  select(study, lat, long, reps, wgt, cc_type2, cc_bio_kgha) %>% 
  mutate(cc_bio_Mgha = cc_bio_kgha/1000) %>% 
  distinct()

m1 <- lmer(cc_bio_Mgha ~ cc_type2 + (1|study), weights = wgt, data = dat_ccbio)
summary(m1)

#No. Just report regular means. By dataset? Hmmm. yes?

den %>% 
  group_by(cc_type2) %>% 
  summarise(cc_bio_Mgha = mean(cc_bio_Mgha, na.rm = T))

bio %>% 
  group_by(cc_type2) %>% 
  summarise(cc_bio_Mgha = mean(cc_bio_Mgha, na.rm = T))

m1 <- (lmer(cc_bio_Mgha ~ cc_type2 + (1|study), data = bio))
summary(m1)

#--do any other modifiers have sig different CC biomasses?
# msmt_season, no
summary(lmer(cc_bio_Mgha ~ msmt_season + (1|study), data = bio))

# weed_group, no
summary(lmer(cc_bio_Mgha ~ weed_group + (1|study), data = bio))

# sys_tillage, no
summary(lmer(cc_bio_Mgha ~ cropsys_tillage + (1|study), data = bio))

# aridity, no
summary(lmer(cc_bio_Mgha ~ aridity_index + (1|study), data = bio))

# OM, yes, but it goes down as OM pct goes up. I guess include it?
summary(lmer(cc_bio_Mgha ~ OM_pct + (1|study), data = bio))
summary(bio$OM_pct)
