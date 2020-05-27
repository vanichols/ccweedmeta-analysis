#############################
#
# Author: Gina
#
# Date Created: Sept 11 2019
#
# Date last modified: Oct 3 (added modifier analyses)
#                     Dec 6 updated and renamed output files
#                     Dec 26 2019 (fixed cc_type2 to include cc_biomass)
#                     Jan 2 2020 (cleaning up)
#                     March 18 2020 (putting it on github)
#                     April 2 2020 (rounding stat summarise to tenths)
#
# Purpose: Fit linear models to get estimates of effect sizes
#
# Inputs: wd_long
#
# Outputs: files to stats_summaries
#
# NOTES:
#
##############################


rm(list=ls())
library(janitor)
library(tidyverse)

source("code/00_functions.R")



# data --------------------------------------------------------------------

dat <- read_csv("working_data/wd_long.csv")

den <- dat %>% filter(resp == "den") %>% filter(!is.na(LRR)) 
bio <- dat %>% filter(resp == "bio") %>% filter(!is.na(LRR)) 


pubs <- dat %>% select(study, pub_reference) %>% distinct()

# fit full models --------------------------------------------------------------

dres <- RunModelNoModsFun(mydata = den, resp = "den")
bres <- RunModelNoModsFun(mydata = bio, resp = "bio")



# fit full model on each study individually -------------------------------

#--density

den_studies <- unique(den$study)

tmp.den <- 
  den %>%
  filter(study == den_studies[1])

dres_study <- RunModelOneStudyFun(mydata = tmp.den, resp = "den") %>% 
  mutate(study = den_studies[1])

for (i in (den_studies[2]:den_studies[length(den_studies)])) {
  
  tmp.study <- den_studies[i]
  
  tmp.den <- 
    den %>%
    filter(study == tmp.study)

  tmp.dat <- RunModelOneStudyFun(mydata = tmp.den, resp = "den") %>% 
    mutate(study = tmp.study)

  dres_study <- 
    dres_study %>% 
    bind_rows(tmp.dat)
  
  i <- i + 1

  print(i)
  
}


#--biomass

bio_studies <- unique(bio$study)

tmp.bio <- 
  bio %>%
  filter(study == bio_studies[1])

bres_study <- RunModelOneStudyFun(mydata = tmp.bio, resp = "bio") %>% 
  mutate(study = bio_studies[1])

for (i in (bio_studies[2]:bio_studies[length(bio_studies)])) {
  
  tmp.study <- bio_studies[i]
  
  tmp.bio <- 
    bio %>%
    filter(study == tmp.study)
  
  tmp.dat <- RunModelOneStudyFun(mydata = tmp.bio, resp = "bio") %>% 
    mutate(study = tmp.study)
  
  bres_study <- 
    bres_study %>% 
    bind_rows(tmp.dat)
  
  i <- i + 1
  
  print(i)
  
}

bres_study %>% 
  bind_rows(dres_study) %>% 
  left_join(pubs) %>% 
  select(pub_reference, resp, estimate, std.error, p.value, cilo, cihi) %>% 
  write_csv("stats_summaries/ss_effect-size-by-study.csv")


# leave-one-out analysis on den by study -------------------------------------------

dreslo <- dres %>% mutate(studylo = NA)
den_stud <- unique(den$study)

for (i in 1:length(den_stud)) {
  
  # trouble
  #i <- 1
  lo <- den_stud[i]
  d.tmp <- den %>% filter(study != lo)
  
  dres.tmp <- RunModelNoModsFun(mydata = d.tmp, resp = "den") %>% 
    mutate(studylo = lo)

  dreslo <- bind_rows(dreslo, dres.tmp)

}

# leave-one-out analysis on bio by study-------------------------------------------

breslo <- bres %>% mutate(studylo = NA)
bio_stud <- unique(bio$study)

for (i in 1:length(bio_stud)) {
  
  # trouble
  #i <- 1
  lo <- bio_stud[i]
  b.tmp <- bio %>% filter(study != lo)
  
  bres.tmp <- RunModelNoModsFun(mydata = b.tmp, resp = "bio") %>% 
    mutate(studylo = lo)
  
  breslo <- bind_rows(breslo, bres.tmp)
  
}


loores <- bind_rows(dreslo, breslo) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(-pavg, -plo, -phi)

loores %>% write_csv("stats_summaries/ss_loo-overall.csv")


# ccbio as covariate ------------------------------------------------------
# which modifiers need ccbio as a covariate?

# lat no
summary(lmer(cc_bm_Mgha ~ lat + (1|study), data = bio))

# cc_type2 yes
summary(lmer(cc_bm_Mgha ~ cc_type2 + (1|study), data = bio))

# msmt_season, no
summary(lmer(cc_bm_Mgha ~ msmt_season + (1|study), data = bio))

# weed_group, no
summary(lmer(cc_bm_Mgha ~ weed_group + (1|study), data = bio))

# ccterm_meth/2, no
anova(lmer(cc_bm_Mgha ~ cc_term_meth2 + (1|study), data = bio))

# sys_tillage, no
summary(lmer(cc_bm_Mgha ~ cropsys_tillage + (1|study), data = bio))

# aridity, no
summary(lmer(cc_bm_Mgha ~ aridity_index + (1|study), data = bio))

# OM, yes, but it goes down as OM pct goes up, not big range in values, and not many values
summary(lmer(cc_bm_Mgha ~ om_pct + (1|study), data = bio))


# is cc_type2 interaction with biomass sig? -------------------------------------------------

mod1 <- lmer(LRR ~ cc_type2*cc_bm_Mgha + (1|study), weight = wgt, data = bio) #--interaction
mod2 <- lmer(LRR ~ cc_type2 + cc_bm_Mgha + (1|study), data = bio, weight = wgt) #--no interaction
mod3 <- lmer(LRR ~ cc_bm_Mgha + (1|study), data = bio, weight = wgt) #--no cc_type

anova(mod1, mod2) #--the interaction is not sig
anova(mod2, mod3) #--including the type sig improves model

# so no need to include interaction

# fit model w/categoricl modifier ----------------------------------------------------

#--do cc_type2 separately due to covariate analysis above

themods <- c("cropsys_tillage", 
             "msmt_season", "msmt_planting", 
             "weed_group", "cc_term_meth2",  
             "crop_follow")



#--density
denmr <- RunModelModsFun(mydata = den, mymod = themods[1], myresp = "den")
for (i in 2:length(themods)){
  tmp <- RunModelModsFun(mydata = den, mymod = themods[i], myresp = "den")
  suppressWarnings(
  denmr <- bind_rows(denmr, tmp)
  )
}
  
#--biomass
biomr <- RunModelModsFun(mydata = bio, mymod = themods[1], myresp = "bio")
for (i in 2:length(themods)){
  tmp <- RunModelModsFun(mydata = bio, mymod = themods[i], myresp = "bio")
 suppressWarnings(
   biomr <- bind_rows(biomr, tmp)
 )
}

# need to do the cc_type2 separately
#--bio cc_type2
m1 <- (lmer(LRR ~ cc_bm_Mgha + cc_type2 + (1|study), weights = wgt, data = bio))
em1 <- (emmeans(m1, spec = "cc_type2"))
p1 <- test(em1, level = 0.95)$p.value

biocctype <- as_tibble(em1) %>% 
  mutate(p_val = p1) %>% 
  mutate(resp = "bio",
         mod_code = "cc_type2") %>%
  clean_names() %>% 
  rename("modlvl" = "cc_type2",
         "est" = "emmean",
         "cilo" = "lower_cl",
         "cihi" = "upper_cl")


#--den cc_type2
m2 <- (lmer(LRR ~ cc_bm_Mgha + cc_type2 + (1|study), weights = wgt, data = den))
em2 <- (emmeans(m2, spec = "cc_type2"))
p2 <- test(em1, level = 0.95)$p.value

dencctype <- as_tibble(em2) %>% 
  mutate(p_val = p2) %>% 
  mutate(resp = "den",
         mod_code = "cc_type2") %>%
  clean_names() %>% 
  rename("modlvl" = "cc_type2",
         "est" = "emmean",
         "cilo" = "lower_cl",
         "cihi" = "upper_cl")


#--combine and write

modres <- denmr %>%
  bind_rows(biomr) %>%
  rename(modlvl = desc)  %>% 
  bind_rows(biocctype, dencctype) %>%
  select(resp, mod_code, modlvl, everything()) %>% 
  arrange(resp, mod_code, modlvl) %>% 
  mutate_if(is.numeric, round, 2)

modres %>% write_csv("stats_summaries/ss_catmods.csv")

# do contrasts ------------------------------------------------------------

#--density
denmc <- RunModelModsContrastFun(mydata = den, mymod = themods[1], myresp = "den")
for (i in 2:length(themods)){
  tmp <- RunModelModsContrastFun(mydata = den, mymod = themods[i], myresp = "den")
  suppressWarnings(
  denmc <- bind_rows(denmc, tmp)
  )
}

#NOTE: m2 is weedden w/cc_type2 and ccbio
dencctype <- as_tibble(contrast(em2)) %>%
  select(-contrast) %>% 
  slice(2) %>% 
  mutate(level1 = "grass",
         level2 = "non-grass",
         resp = "bio",
         mod_code = "cc_type2") %>% 
  rename(est = estimate,
         se = SE,
         statistic = t.ratio)

dencons <- 
  denmc %>% 
  bind_rows(dencctype)

#--biomass
biomc <- RunModelModsContrastFun(mydata = bio, mymod = themods[1], myresp = "bio")
for (i in 2:length(themods)){
  tmp <- RunModelModsContrastFun(mydata = bio, mymod = themods[i], myresp = "bio")
  biomc <- bind_rows(biomc, tmp)
}

#NOTE: m1 is weedbio w/cc_type2 and ccbio
biocctype <- as_tibble(contrast(em1)) %>%
  select(-contrast) %>% 
  slice(2) %>% 
  mutate(level1 = "grass",
         level2 = "non-grass",
         resp = "bio",
         mod_code = "cc_type2bio") %>% 
  rename(est = estimate,
         se = SE,
         statistic = t.ratio)


biocons <- 
  biomc %>% 
  bind_rows(biocctype)

#--combine and write
conres <- dencons %>%
  bind_rows(biocons) %>% 
  arrange(resp, p.value) 

conres %>% write_csv("stats_summaries/ss_catmods-contrasts.csv")


# continuous modifers -----------------------------------------------------

# jsut for reference
thecontmods <- c("aridity_index", "om_pct", "cc_bm_Mgha")


#--density
denmcont <- RunModelContModsFun(mydata = den, mymod = thecontmods[1], myresp = "den")
for (i in 2:length(thecontmods)){
  tmp <- RunModelContModsFun(mydata = den, mymod = thecontmods[i], myresp = "den")
  denmcont <- bind_rows(denmcont, tmp)
}

#--biomass
biomcont <- RunModelContModsFun(mydata = bio, mymod = thecontmods[1], myresp = "bio")
for (i in 2:length(thecontmods)){
  tmp <- RunModelContModsFun(mydata = bio, mymod = thecontmods[i], myresp = "bio")
  biomcont <- bind_rows(biomcont, tmp)
}

contmodres <- denmcont %>% bind_rows(biomcont)
contmodres %>% write_csv("stats_summaries/ss_contmods.csv")



# make preds for figures --------------------------------------------------


# make preds data for graphing LRR weed biomass vs cc bio
biopreds <- bio %>% 
  select(obs_no, wgt, LRR, cc_type2, cc_bm_Mgha) %>% 
  filter(!is.na(cc_bm_Mgha)) %>% 
  mutate(predLRR = predict(m1)) #--note: m1 is the linear model w/ cctype and cc_bm in it

biopreds %>% 
  ggplot(aes(cc_bm_Mgha, LRR)) +
  geom_point() +
  geom_line(aes(cc_bm_Mgha, predLRR, color = cc_type2))

# this isn't working. I guess just extract the beta and do my own predictions?
beta0 <- as.vector(summary(m1)$coefficients[,1])[1]
beta1 <- as.vector(summary(m1)$coefficients[,1])[2]
beta2 <- as.vector(summary(m1)$coefficients[,1])[3]



bio %>% 
  select(obs_no, wgt, LRR, cc_type2, cc_bm_Mgha) %>% 
  filter(!is.na(cc_bm_Mgha)) %>% 
  mutate(grass = ifelse(cc_type2 == "grass", 0, 1),
         predLRR = beta0 + beta1*cc_bm_Mgha + beta2*grass) %>% 
  ggplot(aes(cc_bm_Mgha, LRR)) +
  geom_point() +
  geom_line(aes(cc_bm_Mgha, predLRR, color = cc_type2))

# ok write it.
biopreds <- 
  bio %>% 
  select(obs_no, wgt, LRR, cc_type2, cc_bm_Mgha) %>% 
  filter(!is.na(cc_bm_Mgha)) %>% 
  mutate(grass = ifelse(cc_type2 == "grass", 0, 1),
         predLRR = beta0 + beta1*cc_bm_Mgha + beta2*grass) 

biopreds %>%  write_csv("working_data/wd_bio-preds.csv")

# 75% control requires....
(log(0.25) - beta0)/beta1 #--grass
(log(0.25) - beta0 - beta2)/beta1 #--other

# let's force it thru 0? No. https://dynamicecology.wordpress.com/2017/04/13/dont-force-your-regression-through-zero-just-because-you-know-the-true-intercept-has-to-be-zero/


# random forest -----------------------------------------------------------

library(rpart) # Decision tree package
library(partykit)
library(tree)
library(randomForest)
#library(gbm)
#library(caret)


# need to think about how to evaluate this. What makes it more likely to fall into a WW category?
ydat <-  
  dat %>% 
  mutate(yinc = ifelse(yieldLRR > 0, "Y", "N"),
         wdec = ifelse(LRR < 0 , "Y", "N"),
         ww = ifelse(yinc == "Y" & wdec == "Y", "Y", "N")) %>%
  select(ww, 
         aridity_index, 
         cc_type2, 
         cropsys_tillage,  
         msmt_planting, 
         weed_group,
         #ccpl_den_kgha, #--this is going to mean different things for different crops, eliminate it
         cc_term_meth2,
         crop_follow, 
         cc_bm_kgha,
         #cc_growdays, #--most don't report dates, just months
         #termgap_days #--100 of the points didn't even report a date. 
  ) %>%
  mutate_if(is.character, as.factor) #%>%
# filter(termgap_days > -6)   %>%   #--Hoffman let the vetch grow for a month after planting. got terrible weed control. 
#filter(crop_follow != "corn/soy",  #--remove points that were averaged over crops
#      ccterm_meth != "D")        #--only 3 of these, get rid of them



ydat <- na.omit(ydat)

#--win/win as function of everything
tyld <- partykit::ctree(ww ~ ., 
              data = ydat)
plot(tyld)

f_tree <- tree::tree(ww~., ydat)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b') #--ok prune at 3

prune_tree <- prune.tree(f_tree, best = 3)
plot(prune_tree)
text(prune_tree, pretty = 0)

#--so everything winds up with a 'N'


# try the LRR response ----------------------------------------------------


bio <- dat %>%
  filter(resp == "bio") %>%
  filter(!is.na(LRR)) %>%
  select(-resp)



bio2 <- na.omit(bio)


den <- dat %>%
  filter(resp == "den") %>%
  filter(!is.na(LRR)) %>%
  select(-resp)

den2 <- na.omit(den)


# Try party package -------------------------------------------------------

# Regression tree

#--density
tden <- ctree(LRR ~ ., 
              data = den2)
plot(tden)

#--biomass
tbio <- ctree(LRR ~ ., 
              data = bio2)
plot(tbio) #--says msmt with regard to planting is most important

# try tree package --------------------------------------------------------

#--bio
f_tree <- tree(LRR~., bio2)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

#--this is nonsense
cv_tree <- cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b')

