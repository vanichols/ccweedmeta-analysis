#############################
##
# Author: Gina Nichols
#
# Date Created: Oct 19
#
# Date last modified: Oct 30 2019
#
#
# Purpose: Make database for Liz to fill in w/actual planting dates
# 
# Inputs: rd_cc-database-clean
#
# Outputs: 
#
# NOtes:
#
##############################

rm(list=ls())
library(tidyverse)
library(readxl) #--to read excel files
library(janitor)


dat <- read_csv("_rawdata/rd_cc-database.csv")

a <- dat %>% 
  mutate(cc_termYEAR = NA,
         cc_pYEAR = NA) %>% 
  select(obs_no, study, pub_reference,
         loc_citystate, lat, long,
         cc_spec, cc_type, cc_termMETH, cc_bio_kgha,
         cc_pYEAR, cc_pMNTH2, cc_pDOM,
         cc_termYEAR, cc_termMNTH2, cc_termDOM) %>% 
  clean_names() 


names(a) <- gsub(x = names(a), pattern = "cc_", replacement = "")
names(a)<- gsub(x = names(a), pattern = "mnth2", replacement = "mo")
names(a)<- gsub(x = names(a), pattern = "p_", replacement = "plant_")
names(a)<- gsub(x = names(a), pattern = "year", replacement = "y")
names(a)<- gsub(x = names(a), pattern = "dom", replacement = "mday")
 
a %>% write_csv("_tidydata/td_liz.csv")

