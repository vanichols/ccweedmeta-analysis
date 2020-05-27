#########################
#
# Date of creation: Sept 18 2019
# Date last modified: 
#
# Author: Gina
# Purpose: Look at results of resampling modifier means
#          
# Inputs: 
#
# Outputs: 
# 
#########################

rm(list = ls())
library(tidyverse)
library(here)

setwd(here())

# get functions -----------------------------------------------------------

source("_code/code_00_functions.R")


# read data ---------------------------------------------------------------

raw0 <- read_csv("_tidydata/td_cc-database-clean-long.csv")

bio <- raw0 %>%
  filter(resp == "bio") %>%
  filter(!is.na(LRR))

den <- raw0 %>%
  filter(resp == "den") %>%
  filter(!is.na(LRR))

#--biomass boostrapping res

bsm <- read_csv("_tidydata/td_bs-mods-means.csv") %>%
  #--only 3 dual points, eliminate them
  filter(modlvl != "modD")

bsd <- read_csv("_tidydata/td_bs-mods-dist.csv") %>%
  #--only 3 dual points, eliminate them
  filter(modlvl != "modD")


#--density bootstrapping results

bsdm <- read_csv("_tidydata/td_bs-DEN-mods-means.csv") %>%
  #--only 3 dual points, eliminate them
  filter(modlvl != "modD")

bsdd <- read_csv("_tidydata/td_bs-DEN-mods-dist.csv") %>%
  #--only 3 dual points, eliminate them
  filter(modlvl != "modD")


# Get 95% cis from distributions ------------------------------------------

#-- 2-levels: cc_type2, crop_follow, msmt_season, msmt_planting
#-- 3-levels: weed_group, ccterm_meth
#Examples: myd is data, myci = 0.95, mymod = "sys_tillage", myresp = "bio"

## cc_type2
bm_cctyp2 <- get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "cc_type2", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm)

cctyp2 <- get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "cc_type2", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  bind_rows(bm_cctyp2)

## crop_follow
bm_cf <- get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "crop_follow", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm)

cf <- get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "crop_follow", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  bind_rows(bm_cf)

## msmt_season
bm_ms <- get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "msmt_season", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm)

mseas <- get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "msmt_season", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  bind_rows(bm_ms)

## msmt_planting
bm_mp <- get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "msmt_planting", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm)

mpl <- get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "msmt_planting", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  bind_rows(bm_mp)

## weed group
bm_wg <- get95CI_3lvl_Fun(myd = bsd, myci = 0.95, mymod = "weed_group", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm)

wg <- get95CI_3lvl_Fun(myd = bsdd, myci = 0.95, mymod = "weed_group", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  bind_rows(bm_wg)

## ccterm_meth
bm_tmeth <- get95CI_3lvl_Fun(myd = bsd, myci = 0.95, mymod = "ccterm_meth", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm)

tmeth <- get95CI_3lvl_Fun(myd = bsdd, myci = 0.95, mymod = "ccterm_meth", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  bind_rows(bm_tmeth)



# make results ------------------------------------------------------------

bs_res <- 
  cctyp2 %>%
  bind_rows(cf) %>%
  bind_rows(mseas) %>%
  bind_rows(mpl) %>%
  bind_rows(wg) %>%
  bind_rows(tmeth) %>%
  select(-meanval) %>%
  select(resp, mod, modlvl, bs_low, bs_up, est) 

bs_res %>% write_csv("_tidydata/sd_bs-res.csv")
  



# 2 level -----------------------------------------------------------------

#--cc_type2
get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "cc_type2", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

# hm, why doesn't this show up in the decision tree?
# is bio or term_days greater in grass?

# mean bio for each group
mbio <- bio %>%
  group_by(cc_type2) %>%
  summarise(cc_biomean = mean(cc_bio_kgha, na.rm = T))

#  biomasses, they have the same
bio %>%
  group_by(cc_type2) %>%
  mutate(cc_biomean = mean(cc_bio_kgha, na.rm = T)) %>%
  
  ggplot(aes(cc_bio_kgha)) + 
  geom_density(aes(fill = cc_type2), alpha = 0.2) +
  #geom_vline(aes(xintercept = cc_biomean, color = cc_type2)) + 
  geom_point(aes(x = cc_biomean, y = 0, color = cc_type2), size = 5)


bio %>%
  group_by(cc_type2) %>%
  mutate(term_mean = mean(termgap_days, na.rm = T)) %>%

  ggplot(aes(termgap_days)) + 
  geom_density(aes(fill = cc_type2), alpha = 0.2) + 
  geom_vline(aes(xintercept = term_mean, color = cc_type2))

library(ggridges)

# term gap
bio %>%
  mutate(cc_type2 = factor(cc_type2, levels = c("non-grass", "grass"))) %>%
  group_by(cc_type2) %>%
  mutate(mymean = mean(termgap_days, na.rm = T)) %>%
  
  ggplot(aes(termgap_days, cc_type2)) + 
  geom_density_ridges(aes(fill = cc_type2)) +
  geom_point(aes(x = term_mean, y = cc_type2)) 

# biomass
bio %>%
  mutate(cc_type2 = factor(cc_type2, levels = c("non-grass", "grass"))) %>%
  group_by(cc_type2) %>%
  mutate(mymean = mean(cc_bio_kgha, na.rm = T)) %>%
  
  ggplot(aes(cc_bio_kgha, cc_type2)) + 
  geom_density_ridges(aes(fill = cc_type2)) +
  geom_point(aes(x = mymean, y = cc_type2)) 
  

#--crop_follow
get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "crop_follow", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

#--msmt_season
get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "msmt_season", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

#--msmt_planting
get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "msmt_planting", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()



# 3 level -----------------------------------------------------------------


#--weed group
get95CI_3lvl_Fun(myd = bsd, myci = 0.95, mymod = "weed_group", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

#--ccterm_meth
get95CI_3lvl_Fun(myd = bsd, myci = 0.95, mymod = "ccterm_meth", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()


#--ccterm_meth
get95CI_2lvl_Fun(myd = bsd, myci = 0.95, mymod = "ccterm_meth", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()




##########################################################
# Density -----------------------------------------------------------------
##########################################################


#--density boostrapping res

bsdm <- read_csv("_tidydata/td_bs-DEN-mods-means.csv") %>%
  #--only 3 dual points, eliminate them
  filter(modlvl != "modD")

bsdd <- read_csv("_tidydata/td_bs-DEN-mods-dist.csv") %>%
  #--only 3 dual points, eliminate them
  filter(modlvl != "modD")



# Get 95% cis from distributions ------------------------------------------

#-- 2-levels: cc_type2, crop_follow, msmt_season, msmt_planting
#-- 3-levels: weed_group, ccterm_meth
#Examples: myd is data, myci = 0.95, mymod = "sys_tillage", myresp = "den"


# 2 level -----------------------------------------------------------------

#--cc_type2
get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "cc_type2", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

# mean planting den for each group
mden <- den %>%
  group_by(cc_type2) %>%
  summarise(cc_denmean = mean(ccpl_den_kgha, na.rm = T))

#  denm, they have the same
den %>%
  group_by(cc_type2) %>%
  mutate(cc_biomean = mean(cc_bio_kgha, na.rm = T)) %>%
  
  ggplot(aes(cc_bio_kgha)) + 
  geom_density(aes(fill = cc_type2), alpha = 0.2) +
  #geom_vline(aes(xintercept = cc_denmean, color = cc_type2)) + 
  geom_point(aes(x = cc_biomean, y = 0, color = cc_type2), size = 5)


den %>%
  group_by(cc_type2) %>%
  mutate(term_mean = mean(termgap_days, na.rm = T)) %>%
  
  ggplot(aes(termgap_days)) + 
  geom_density(aes(fill = cc_type2), alpha = 0.2) + 
  geom_vline(aes(xintercept = term_mean, color = cc_type2))


#--crop_follow
get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "crop_follow", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

#--msmt_season
get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "msmt_season", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

#--msmt_planting
get95CI_2lvl_Fun(myd = bsdd, myci = 0.95, mymod = "msmt_planting", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()



# 3 level -----------------------------------------------------------------


#--weed group
get95CI_3lvl_Fun(myd = bsdd, myci = 0.95, mymod = "weed_group", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()

#--ccterm_meth
get95CI_3lvl_Fun(myd = bsdd, myci = 0.95, mymod = "ccterm_meth", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm) %>%
  
  ggplot(aes(modlvl, est)) + 
  geom_point(size = 4, fill = "red", pch = 21) + 
  geom_segment(aes(x = modlvl, xend = modlvl,
                   y = bs_low, yend = bs_up)) + 
  geom_hline(yintercept = 0) + 
  coord_flip()



# weed group

bm_wg <- get95CI_3lvl_Fun(myd = bsd, myci = 0.95, mymod = "weed_group", myresp = "bio") %>%
  rename(modlvl = desc) %>%
  left_join(bsm)

den_wg <- get95CI_3lvl_Fun(myd = bsdd, myci = 0.95, mymod = "weed_group", myresp = "den") %>%
  rename(modlvl = desc) %>%
  left_join(bsdm)

wg <- bind_rows(bm_wg, den_wg)
