#########################
#
# Date of creation: June 7 2019
# Date last modified: June 7 2019
#                     Sept 17 (re-reunning w/new dataset)
#                     Oct 2 (added density)
#
# Author: Gina
# Purpose: Explore using decision trees on weed-rotation data
#          
# Inputs: td_cc-database-clean
#
# Outputs: 
# NOTE: Just use density data, bc that's the only one w/hetergeneity
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

# let's look at this data -------------------------------------------------

#--we lose 4 points by eliminating fall
raw <- raw0 %>%
  filter(msmt_season != "fall")

bio <- raw %>%
  filter(resp == "bio") %>%
  filter(!is.na(LRR))

den <- raw %>%
  filter(resp == "den") %>%
  filter(!is.na(LRR))

# Bootstrap modifiers ------------------------------------------------------------

# 4 modifiers
dothese <- c("cc_type2", "weed_group", 
             "ccterm_meth", "crop_follow",
             "msmt_season",
             "msmt_planting") 


# Biomass -----------------------------------------------------------------


#~~~~~~~~~~~~~~~~~
# Biomass just get means
#~~~~~~~~~~~~~~~~~

mbm_ty <- BootFirstFullModsFun(bio, dothese[1], "bio")
mbm_wg <- BootFirstFullModsFun(bio, dothese[2], "bio")
mbm_tm <- BootFirstFullModsFun(bio, dothese[3], "bio")
mbm_cf <- BootFirstFullModsFun(bio, dothese[4], "bio")

#--added later
mbm_ms <- BootFirstFullModsFun(bio, dothese[5], "bio")
mbm_mp <- BootFirstFullModsFun(bio, dothese[6], "bio")

bs_bm_mod_means <- bind_rows(mbm_ty,
                        mbm_wg,
                        mbm_tm,
                        mbm_cf,
                        mbm_ms,
                        mbm_mp)

bs_bm_mod_means %>% write_csv("_tidydata/td_bs-mods-means.csv")

#~~~~~~~~~~~~~~~~~
# Biomass full boot
#~~~~~~~~~~~~~~~~~

bm_ty <- BootModsFun(bio, dothese[1], "bio")
bm_wg <- BootModsFun(bio, dothese[2], "bio")
bm_tm <- BootModsFun(bio, dothese[3], "bio")
bm_cf <- BootModsFun(bio, dothese[4], "bio")

#--added later
bm_ms <- BootModsFun(bio, dothese[5], "bio")
bm_mp <- BootModsFun(bio, dothese[6], "bio")


bs_bm_mod <- bind_rows(bm_ty,
                       bm_wg,
                       bm_tm,
                       bm_cf,
                       bm_ms,
                       bm_mp)

bs_bm_mod %>% write_csv("_tidydata/td_bs-mods-dist.csv")


# Density -----------------------------------------------------------------

#~~~~~~~~~~~~~~~~~
# density just get means
#~~~~~~~~~~~~~~~~~

mden_ty <- BootFirstFullModsFun(den, dothese[1], "den")
mden_wg <- BootFirstFullModsFun(den, dothese[2], "den")
mden_tm <- BootFirstFullModsFun(den, dothese[3], "den")
mden_cf <- BootFirstFullModsFun(den, dothese[4], "den")

#--added later
mden_ms <- BootFirstFullModsFun(den, dothese[5], "den")
mden_mp <- BootFirstFullModsFun(den, dothese[6], "den")

bs_den_mod_means <- bind_rows(mden_ty,
                             mden_wg,
                             mden_tm,
                             mden_cf,
                             mden_ms,
                             mden_mp)

bs_den_mod_means %>% write_csv("_tidydata/td_bs-DEN-mods-means.csv")

#~~~~~~~~~~~~~~~~~
# denmass full boot
#~~~~~~~~~~~~~~~~~

den_ty <- BootModsFun(den, dothese[1], "den")
den_wg <- BootModsFun(den, dothese[2], "den")
den_tm <- BootModsFun(den, dothese[3], "den")
den_cf <- BootModsFun(den, dothese[4], "den")

#--added later
den_ms <- BootModsFun(den, dothese[5], "den")
den_mp <- BootModsFun(den, dothese[6], "den")


bs_den_mod <- bind_rows(den_ty,
                       den_wg,
                       den_tm,
                       den_cf,
                       den_ms,
                       den_mp)

bs_den_mod %>% write_csv("_tidydata/td_bs-DEN-mods-dist.csv")
