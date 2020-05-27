#############################
##
# Author: Gina
#
# Date Created: Sept 11 2019
#
# Date last modified: Oct 3 (added modifier analyses)
#                     Dec 6 updated and renamed output files
#                     Dec 26 2019 (fixed cc_type2 to include cc_biomass)
#                     Jan 2 2020 (cleaning up)
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
library(janitor)
library(ggplot2)
library(tidyverse)
library(lmerTest)

source("_code/code_00_functions.R")

dat <- read_csv("_tidydata/td_cc-database-clean-long.csv")

den <- dat %>% filter(resp == "den") %>% filter(!is.na(LRR)) 
bio <- dat %>% filter(resp == "bio") %>% filter(!is.na(LRR)) 

bio %>% 
  group_by(cc_type) %>% 
  summarise(n = n())


# fit full models --------------------------------------------------------------

dres <- RunModelNoModsFun(mydata = den, resp = "den")
bres <- RunModelNoModsFun(mydata = bio, resp = "bio")

sdover <- bind_rows(dres, bres) %>%
  mutate_if(is.numeric, round, 3) 

sdover %>%  write_csv("_tidydata/sd_overall-weeds.csv")

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
  mutate_if(is.numeric, round, 2)

loores %>% write_csv("_tidydata/sd_loo-overall.csv")


# ccbio as covariate ------------------------------------------------------

# which modifiers need ccbio as a covariate?

# see if latitude is sig
mcbio2 <- lmer(cc_bio_Mgha ~ lat + (1|study), data = bio)
summary(mcbio2)
emmeans(mcbio1, specs = "cc_type2")

# cc_type2 yes
mcbio1 <- lmer(cc_bio_Mgha ~ cc_type2 + (1|study), data = bio)
emmeans(mcbio1, specs = "cc_type2")

# msmt_season, no
summary(lmer(cc_bio_Mgha ~ msmt_season + (1|study), data = bio))

# weed_group, no
summary(lmer(cc_bio_Mgha ~ weed_group + (1|study), data = bio))

# ccterm_meth/2, no
anova(lmer(cc_bio_Mgha ~ ccterm_meth + (1|study), data = bio))

# sys_tillage, no
summary(lmer(cc_bio_Mgha ~ cropsys_tillage + (1|study), data = bio))

# aridity, no
summary(lmer(cc_bio_Mgha ~ aridity_index + (1|study), data = bio))

# OM, yes, but it goes down as OM pct goes up. This seems weird
summary(lmer(cc_bio_Mgha ~ OM_pct + (1|study), data = bio))
summary(bio$OM_pct) #--lots of NAs


# is cc_type2 interaction with biomass sig? -------------------------------------------------

mod1 <- lmer(LRR ~ cc_type2*cc_bio_Mgha + (1|study), weight = wgt, data = bio) #--interaction
mod2 <- lmer(LRR ~ cc_type2 + cc_bio_Mgha + (1|study), data = bio, weight = wgt) #--no interaction
mod3 <- lmer(LRR ~ cc_bio_Mgha + (1|study), data = bio, weight = wgt) #--no cc_type

anova(mod1, mod2) #--the interaction is not sig
anova(mod2, mod3) #--including the type sig improves model

# so no need to include interaction

# fit model w/categoricl modifier ----------------------------------------------------

#--do cc_type2 separately due to covariate analysis above

themods <- c("cropsys_tillage", 
             "msmt_season", "msmt_planting", 
             "weed_group", "ccterm_meth",  
             "crop_follow")



#--density
denmr <- RunModelModsFun(mydata = den, mymod = themods[1], myresp = "den")
for (i in 2:length(themods)){
  tmp <- RunModelModsFun(mydata = den, mymod = themods[i], myresp = "den")
  denmr <- bind_rows(denmr, tmp)
}
  
#--biomass
biomr <- RunModelModsFun(mydata = bio, mymod = themods[1], myresp = "bio")
for (i in 2:length(themods)){
  tmp <- RunModelModsFun(mydata = bio, mymod = themods[i], myresp = "bio")
  biomr <- bind_rows(biomr, tmp)
}


# need to do the cc_type2 separately
#--bio cc_type2
m1 <- (lmer(LRR ~ cc_bio_Mgha + cc_type2 + (1|study), weights = wgt, data = bio))
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
m2 <- (lmer(LRR ~ cc_bio_Mgha + cc_type2 + (1|study), weights = wgt, data = den))
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
  arrange(resp, mod_code, modlvl)

modres %>% write_csv("_tidydata/sd_mods-weeds.csv")



# do contrasts ------------------------------------------------------------

#--density
denmc <- RunModelModsContrastFun(mydata = den, mymod = themods[1], myresp = "den")
for (i in 2:length(themods)){
  tmp <- RunModelModsContrastFun(mydata = den, mymod = themods[i], myresp = "den")
  denmc <- bind_rows(denmc, tmp)
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

conres %>% write_csv("_tidydata/sd_mods-weeds-contrasts.csv")


# continuous modifers -----------------------------------------------------

# jsut for reference
thecontmods <- c("aridity_index", "OM_pct", "cc_bio_Mgha")


#--density

# aridity index
summary(lmer(LRR ~ aridity_index  + (1|study), data = den, weights = wgt))

#om
summary(lmer(LRR ~ OM_pct  + (1|study), data = den, weights = wgt))

#ccbio
ccbio1 <- lmer(LRR ~ cc_bio_Mgha + (1|study), 
           data = den, 
           weights = wgt) 
summary(ccbio1)


#--biomass

# aridity index
summary(lmer(LRR ~ aridity_index  + (1|study), data = bio, weights = wgt))

#om
summary(lmer(LRR ~ OM_pct  + (1|study), data = bio, weights = wgt))

ccbio2 <- lmer(LRR ~ cc_bio_Mgha  + cc_type2 + (1|study), 
              data = bio, 
              weights = wgt) 
summary(ccbio2)

# make preds data for graphing LRR weed biomass vs cc bio

biopreds <- bio %>% 
  select(obs_no, wgt, LRR, cc_type2, cc_bio_Mgha) %>% 
  filter(!is.na(cc_bio_Mgha)) %>% 
  mutate(predLRR = predict(ccbio2))

biopreds %>% 
  ggplot(aes(cc_bio_Mgha, LRR)) +
  geom_point() +
  geom_line(aes(cc_bio_Mgha, predLRR, color = cc_type2))

# this isn't working. I guess just extract the beta and do my own predictions?
beta0 <- as.vector(summary(ccbio2)$coefficients[,1])[1]
beta1 <- as.vector(summary(ccbio2)$coefficients[,1])[2]
beta2 <- as.vector(summary(ccbio2)$coefficients[,1])[3]

bio %>% 
  select(obs_no, wgt, LRR, cc_type2, cc_bio_Mgha) %>% 
  filter(!is.na(cc_bio_Mgha)) %>% 
  mutate(grass = ifelse(cc_type2 == "grass", 0, 1),
         predLRR = beta0 + beta1*cc_bio_Mgha + beta2*grass) %>% 
  ggplot(aes(cc_bio_Mgha, LRR)) +
  geom_point() +
  geom_line(aes(cc_bio_Mgha, predLRR, color = cc_type2))

# ok write it.
biopreds <- 
  bio %>% 
  select(obs_no, wgt, LRR, cc_type2, cc_bio_Mgha) %>% 
  filter(!is.na(cc_bio_Mgha)) %>% 
  mutate(grass = ifelse(cc_type2 == "grass", 0, 1),
         predLRR = beta0 + beta1*cc_bio_Mgha + beta2*grass) 

biopreds %>%  write_csv("_tidydata/sd_bio-preds.csv")

# 50% control? 75% control?
(log(0.25) - beta0)/beta1
(log(0.25) - beta0 - beta2)/beta1

exp(beta0 + beta1*5 + beta2)


(log(0.5) - beta0 - beta2)/beta1


# let's force it thru 0? No. https://dynamicecology.wordpress.com/2017/04/13/dont-force-your-regression-through-zero-just-because-you-know-the-true-intercept-has-to-be-zero/
ccbio2 <- lmer(LRR ~ 0 + cc_bio_Mgha  + cc_type2 + (1|study), 
               data = bio, 
               weights = wgt) 
summary(ccbio2)



# 50% control SE -------------------------------------------------------------

library(ggResidpanel)

# Load KG's function ------------------------------------------------------

compute_se <- function(lrr, mymod){
  
  #NOTE: I added this part
  ####
  # Extract the variance-covariance matrix for beta0 and beta1
  betas <- as.vector(summary(mymod)$coefficients[,1])
  
  # Extract the variance-covariance matrix for beta0 and beta1
  vcov <- matrix(vcov(mymod), nrow = 2)
  #####
  
  
  # Separate the betas
  b0 <- betas[1] 
  b1 <- betas[2] 
  
  # Compute the inverse prediction of weed biomass
  pred_biomass <- (lrr - b0) / b1
  
  # Create an empty 1x2 matrix to store the elements of d in
  d <- matrix(NA, nrow = 1, ncol = 2)
  
  # Compute the elements of d (partial derivatives of g(beta))
  d[1] <- -1 / b1 
  d[2] <- (-lrr + b0) / (b1^2) 
  
  # Compute the standard error of annual survival (using the delta method)
  se <- sqrt(d %*% vcov %*% t(d))
  
  # Compute the lower and upper bounds of the 95% CI for annual survival
  lower <- pred_biomass - (1.96 * se) 
  upper <- pred_biomass + (1.96 * se)
  
  # Return the log response ratio, the predicted weed biomass given the 
  # resopnse ration, the delta method standard error, and the lower 
  # and upper bounds of the 95% CI for the predicted weed biomass
  return(data.frame(lrr = lrr,
                    pred_biomass = pred_biomass, 
                    se = se, 
                    ci_lower = lower, 
                    ci_upper = upper))
  
}




# fit models --------------------------------------------------------------

#--fit a model without a forced 0,0 intercept

#--biomass
biomod <- lmer(LRR ~ cc_bio_Mgha + (1|study), weights = wgt, data = bio)
summary(biomod)
#resid_panel(mymod)
##--use KG's function to see where 50% weed reduction happens
compute_se(lrr = log(0.5), mymod = biomod)
# well that's a big range. But fine.  

#--density
denmod <- lmer(LRR ~ cc_bio_Mgha + (1|study), weights = wgt, data = den)
summary(denmod)
#resid_panel(mymod)
##--use KG's function to see where 50% weed reduction happens
compute_se(lrr = log(0.5), mymod = denmod)

dat %>% 
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_point(aes(color = resp)) + 
  geom_smooth(method = "lm", aes(color = resp))


# # is type by biomass sig? -------------------------------------------------
# 
# biosm <- 
#   bio %>% 
#   filter(!is.na(cc_bio_kgha)) %>% 
#   mutate(cc_bio_Mgha = cc_bio_kgha/1000) %>% 
#   select(study, resp, wgt, LRR, cc_type2, cc_bio_Mgha)
# 
# 
# mod1 <- lmer(LRR ~ cc_type2*cc_bio_Mgha + (1|study), weight = wgt, data = biosm) #--interaction
# mod2 <- lmer(LRR ~ cc_type2 + cc_bio_Mgha + (1|study), data = biosm, weight = wgt) #--no interaction
# mod3 <- lmer(LRR ~ cc_bio_Mgha + (1|study), data = biosm, weight = wgt) #--no cc_type
# 
# anova(mod1, mod2) #--the interaction is not sig
# anova(mod2, mod3) #--including the type sig improves model
# 
# #--is the amount of biomass sig affected by cc_type2?
# # yes. but only if you include those extreme values
# 
# biosmsub <- biosm %>% filter(cc_bio_Mgha < 7)
# mod4 <- lmer(cc_bio_Mgha ~ cc_type2 + (1|study), weight = wgt, data = biosm)
# summary(mod4)
# 
# biosmsub %>% 
#   ggplot(aes(cc_bio_Mgha)) + 
#   geom_density(aes(fill = cc_type2), alpha = 0.5)
# 
# mod2a <- (lmer(LRR ~ cc_type2 + cc_bio_Mgha + (1|study), data = biosmsub, weight = wgt))
# summary(mod2a)
# 
# biosmsub %>% 
#   mutate(preds = predict(mod2a)) %>% 
#   ggplot(aes(cc_bio_Mgha, LRR)) + 
#   #geom_point(aes(color = cc_type2)) + 
#   geom_point(aes(cc_bio_Mgha, preds, color = cc_type2))
# 
# 
# # Separate the betas
# betas <- as.vector(summary(mod2)$coefficients[,1])
# b0 <- betas[1] 
# b1 <- betas[2] 
# b2 <- betas[3] 
# 
# #--what does 3 Mg/ha get you?
# LRRgrass3Mg <- exp(b0 + b1*0 + b2*3)*100 - 100
# LRRother3Mg <- exp(b0 + b1*1 + b2*3)*100 - 100
# 
# 
# Mg50grass <- (-1.11 - b0 - b1*0) / b2
# Mg50other <- (-1.11 - b0 - b1*1) / b2
# 
# 
# # messing with continuous variables ---------------------------------------
# 
# #RunModelContModsFun <- function(mydata, mymod, myresp) {
# 
# 
# ##### For trouble shooting, comment out when you actually run it######
# ##~~~~~~~~~~~~~~~~~
# mydata <- bio
# mymod = "cc_bio_kgha"
# myresp = "bio"
# 
# d.tmp <- 
#   mydata %>% 
#   select(resp, study, LRR, wgt, mymod) %>%
#   rename_at(5, ~"mod") %>%         # change column name to generalizable 'mymod'
#   #mutate(mod= as.numeric(mod)/1000) %>%
#   filter(!is.na(mod)) %>%
#   filter(mod > -6)
# 
# # Fit model to feed to emmeans
# F.res <- lmer(LRR ~ mod  + (1|study), 
#               data = d.tmp, 
#               weights = wgt) 
# 
# summary(F.res)
# fixef(F.res)
# d.tmp %>%
#   mutate(predict = predict(F.res)) %>%
#   
#   ggplot(aes(mod, LRR)) + 
#   geom_point() + 
#   geom_point(aes(mod, predict), color = 'red') + 
#   geom_vline(xintercept = 4000) + 
#   #geom_vline(xintercept = 5.7) + 
#   geom_hline(yintercept = -.69, color = "blue")
# #}
# 
