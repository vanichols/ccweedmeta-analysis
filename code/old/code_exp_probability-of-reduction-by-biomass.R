#############################
##
# Author: Gina
# Purpose: Find prob of weed red at various cc biomass values
# Date Created: Dec 6 2019
# Date last modified:
#
# Inputs: td_cc-database-clean-long
# NOtes: 
#
##############################


rm(list=ls())
library(tidyverse)

dat <- read_csv("_tidydata/td_cc-database-clean-long.csv") #%>% 
  #select(obs_no, cc_bio_kgha, resp, LRR) 
  

den <- dat %>% filter(resp == "den")
bio <- dat %>% filter(resp == "bio")  %>% 
  filter(LRR > -5) %>% 
  filter(!is.na(cc_bio_kgha))


# run across biomass groups -----------------------------------------------

bmgrp <- 
  bio %>% 
  mutate(
    bio_Mgha = cc_bio_kgha/1000,
    bio_cut = cut_number(bio_Mgha, 22)) %>% 
  arrange(bio_Mgha) %>% 
  mutate(group_ID = group_indices(., bio_cut)) %>% 
  select(study, wgt, group_ID, bio_Mgha, LRR)


biores <- tibble(group_ID = NA,
                 pval = NA,
                 avg = NA,
                 cilo = NA,
                 cihi = NA,
                 pavg = NA,
                 plo = NA,
                 phi = NA)

for (i in 1:max(bmgrp$group_ID)) {
  
  #i <- 1
  
  dome <- bmgrp %>%
    filter(group_ID %in% c(1:i))
  
  group.tmp <- max(dome$group_ID)
  
  F.res <- lmer(LRR ~ 1 + (1|study), 
                data = dome, 
                weights = wgt) 
  
  # Extract lmer results using contest from lmerTest package
  F.tidy <- contest(F.res, L = 1, joint = F, level = 0.95) 
  
  # Fix names
  names(F.tidy) <- c("avg", "se", "df", "t", "cilo", "cihi", "pval")
  
  F.res <- F.tidy %>%
    mutate(pavg = exp(avg) * 100 - 100,
           plo = exp(cilo) * 100 - 100,
           phi = exp(cihi) * 100 - 100
           
    ) %>%
    select(pval, avg, cilo, cihi, pavg, plo, phi) %>% 
    mutate(group_ID = group.tmp)
  
  biores <- bind_rows(biores, F.res)
  
}


grpbio <- bmgrp %>% 
  select(bio_Mgha, group_ID)

biores2 <- 
  biores %>%
  filter(!is.na(group_ID)) %>% 
  left_join(grpbio)

biores2 %>%
  ggplot(aes(bio_Mgha, avg)) + 
  geom_pointrange(aes(ymin = cilo, ymax = cihi)) + 
  geom_hline(yintercept = 0, color = "red")


bio %>% 
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_point(aes(color = cc_type2))

read_csv("_tidydata/sd_mods-weeds-contrasts.csv") %>% 
  arrange(resp, p.value)

# other shit --------------------------------------------------------------


den %>% 
  select(obs_no, cc_bio_kgha, resp, LRR) %>% 
  arrange(cc_bio_kgha) %>% 
  mutate(lessweeds = ifelse(LRR < 0, 1, 0),
         n = n(),
         cumless = cumsum(lessweeds)/n) %>% 
  ggplot(aes(cc_bio_kgha, cumless)) + 
  geom_point() + 
  geom_hline(yintercept = 0.5)



bio %>% 
  select(obs_no, cc_bio_kgha, resp, LRR) %>% 
  arrange(cc_bio_kgha) %>% 
  mutate(lessweeds = ifelse(LRR < 0, 1, 0),
         n = n(),
         cumless = cumsum(lessweeds)/n) %>% 
  ggplot(aes(cc_bio_kgha, cumless)) + 
  geom_point() +
  geom_hline(yintercept = 0.5) + 
  geom_vline(xintercept = 3766)


bio %>% 
  select(obs_no, cc_bio_kgha, resp, LRR) %>% 
  arrange(cc_bio_kgha) %>% 
  mutate(lessweeds = ifelse(LRR < 0, 1, 0),
         n = n(),
         cumless = cumsum(lessweeds)/n) %>% 
  filter(cumless>0.5)

bio %>% 
  select(obs_no, cc_bio_kgha, resp, LRR) %>% 
  arrange(-cc_bio_kgha) %>% 
  mutate(lessweeds = ifelse(LRR < 0, 1, 0),
         n = n(),
         rownum = 1,
         cum = cumsum(rownum),
         cumless = cumsum(lessweeds)/cum) %>% 
  ggplot(aes(cc_bio_kgha, cumless)) + 
  geom_point() + 
  scale_x_reverse()


#overall, 77% of points give you a positive effect
bio %>% 
  mutate(good = ifelse(LRR<0, 1, 0),
         tot = n()) %>%
  summarise(good = sum(good)/123)
bio %>% 
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method = "lm")

  
bio %>% 
  arrange(-cc_bio_kgha) %>% 
  mutate(lessweeds = ifelse(LRR < 0, 1, 0),
         n = n(),
         rownum = 1,
         cum = cumsum(rownum),
         cumless = cumsum(lessweeds)/cum) %>% 
  ggplot(aes(cc_bio_kgha, cumless)) + 
  geom_point()

library(earth)


biomars <- data.frame(bio %>% 
  filter(!is.na(LRR)) %>% 
  filter(!is.na(cc_bio_kgha)) %>% 
  select(cc_bio_kgha, LRR))

mars1 <- earth(LRR ~ cc_bio_kgha, data = biomars)

summary(mars1)
