#############################
##
# Author: Gina Nichols
#
# Date Created: Nov 20
#
# Date last modified: Nov 20
#
#
# Purpose: Process Adam Davis data to get cover crop biomasses
# 
# Inputs: Davis2019_data-rye
#
# Outputs: 
#
# NOtes:
#
##############################

rm(list=ls())
library(tidyverse)
library(readxl) #--to read excel files


# read in data ------------------------------------------------------------

#--not this was copied from the raw file Davis sent me, I eliminated things I don't care about
datraw <- read_excel("_rawdata/Davis2019_data-rye.xlsx") 


# get average biomass for each treatment I guess --------------------------

dat <-
  datraw %>% 
  filter(!is.na(coverbio)) %>% 
  group_by(year, kill, cover) %>% 
  summarise(coverbio_kgha = mean(coverbio))

dat %>% write_csv("_tidydata/td_davis-ccbio.csv")
