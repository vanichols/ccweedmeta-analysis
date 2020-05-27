#############################
##
# Author: Gina
#
# Date Created: Sept 11 2019
#
# Date last modified: March 6 2020
#
# Purpose: Fit linear models to get estimates of effect sizes for yield response
#
# Inputs: 
##############################


rm(list=ls())
library(lme4)
library(ggplot2)
library(tidyverse)
library(lmerTest)
library(here)
library(ccweedmetapkg)

setwd(here::here())
setwd("../Box/1_Gina_Projects/proj_WeedMetaCC/")
source("code/00_functions.R")


# # what did I test for weeds?
# themods <- c("cropsys_tillage", 
#              "msmt_season", "msmt_planting", 
#              "weed_group", "ccterm_meth", 
#              "crop_follow")

raw <- read_csv("working_data/wd_long.csv") 

bio <- raw %>% 
  select(-LRR) %>% 
  filter(!is.na(yieldLRR)) %>% 
  rename("LRR" = yieldLRR) %>% 
  filter(resp == "bio")

den <- raw %>% 
  select(-LRR) %>% 
  filter(!is.na(yieldLRR)) %>% 
  rename("LRR" = yieldLRR) %>% 
  filter(resp == "den")

all <- raw %>% 
  filter(!is.na(yieldLRR)) %>% 
  mutate(LRR = yieldLRR,
         resp = "all") 


# fit full models on yield ------------------------------------------------

ybio <- RunModelNoModsFun(mydata = den, resp = "bio")
yden <- RunModelNoModsFun(mydata = bio, resp = "den")
yall <- RunModelNoModsFun(mydata = all, resp = 'all')

yall2 <- summary(lmer(LRR ~ cc_type2 + (1|study), weights = wgt, data = all))
yall2 <- summary(lmer(LRR ~ 1 + (1|study), weights = wgt, data = all))


yres <- bind_rows(ybio, yden) %>%
  mutate_if(is.numeric, round, 3) 

yres %>%  write_csv("stats_summaries/ss_yields.csv")


# do select contrasts ------------------------------------------------------------

themods <- c("cropsys_tillage",
             "msmt_season", "msmt_planting",
             "weed_group", "cc_term_meth",
             "crop_follow", "cc_type2")

#--does cctype affect yield? No. bio: p=0.2, den:p=0.3
denmc <- RunModelModsContrastFun(mydata = den, mymod = themods[7], myresp = "den")
biomc <- RunModelModsContrastFun(mydata = bio, mymod = themods[7], myresp = "bio")
allmc <- RunModelModsContrastFun(mydata = all, mymod = themods[7], myresp = "all")

#--does ccbio differ for yields? No. 
denbio <- summary(lmer(LRR ~ cc_bm_Mgha + (1|study), weights = wgt, data = den))
biobio <- summary(lmer(LRR ~ cc_bm_Mgha + (1|study), weights = wgt, data = bio))
allbio <- summary(lmer(LRR ~ cc_bm_Mgha + (1|study), weights = wgt, data = filter(all, cc_bm_Mgha < 8)))

all %>%
  filter(cc_bm_Mgha < 8) %>% 
  ggplot(aes(cc_bm_Mgha, LRR)) +
  geom_point()


# fit model w/modifier ----------------------------------------------------

themods2 <- c("cropsys_tillage","msmt_season", "msmt_planting", "cc_type2")

#--density
denmr <- RunModelModsFun(mydata = den, mymod = themods2[1], myresp = "den")
for (i in 2:length(themods2)) {
  tmp <- RunModelModsFun(mydata = den, mymod = themods2[i], myresp = "den")
  denmr <- bind_rows(denmr, tmp)
}

#--biomass
biomr <- RunModelModsFun(mydata = bio, mymod = themods2[1], myresp = "bio")
for (i in 2:length(themods2)){
  tmp <- RunModelModsFun(mydata = bio, mymod = themods2[i], myresp = "bio")
  biomr <- bind_rows(biomr, tmp)
}

#--combine and write
yldmods <- denmr %>%
  bind_rows(biomr) %>%
  rename(modlvl = desc) %>% 
  select(resp, mod_code, modlvl, everything()) %>% 
  arrange(resp, p_val)


yldmods %>%  write_csv("stats_summaries/ss_yields-mods.csv")
