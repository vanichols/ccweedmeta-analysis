#############################
##
# Author: Gina Nichols
#
# Date Created: Oct 19
#
# Date last modified: Oct 23 2018 - by Gina, fix 0s
#                     June 7 2019 - (Gina) use googledrive package instead of googlesheets
#                                    added cc_type2 (2 categories of cc types)
#                     Aug 9 2019 - Gina, moved everything into cybox
#                     Sept 6 2019 - made reading google sheet it's own code file
#                     Sept 9 2019 - made dates into separate month/day columns
#                     Sept 11 2019 - Fisk cc biomasses were wrong. 
#                     Jan 2 2020 - checking one final time, updating aridity indexes
#
# Purpose: Clean data
# 
# Inputs: google sheet 'main database'
#
# Outputs: rd_cc-database
#
##############################

rm(list=ls())
library(readr) #--for write_csv
library(dplyr)
library(googledrive) #--use to read in raw data
library(here) #--used to set working directory to R project, uses relative paths
library(readxl) #--to read excel files
library(naniar) #--allows you to replace . with NA

myhere <- here()

setwd(paste0(myhere, "/_rawdata"))

files <- drive_find(type = "spreadsheet")

# Read in data from google sheet and write to raw folder------------------------------------------
drive_download("Cover crop - weeds meta-analysis", overwrite = T)


# Wrangle data and write to tidydata folder -------------------------------

dat <- read_excel("Cover crop - weeds meta-analysis.xlsx") %>%
  replace_with_na_all(condition = ~.x %in% c(".")) %>%
  
  # Change certain columns to numeric
  mutate(cc_wden_numm2 = as.numeric(cc_wden_numm2),
         ctl_wden_numm2 = as.numeric(ctl_wden_numm2),
         
         cc_wbio_gm2 = as.numeric(cc_wbio_gm2),
         ctl_wbio_gm2 = as.numeric(ctl_wbio_gm2),
         
         cc_yield_kgha = as.numeric(cc_yield_kgha),
         ctl_yield_kgha = as.numeric(ctl_yield_kgha)) %>%
  
  # Rename so it's less to type every time
  rename(ccden = cc_wden_numm2,
         ctlden = ctl_wden_numm2,
         ccbio = cc_wbio_gm2,
         ctlbio = ctl_wbio_gm2,
         
         msmt_season = time,
         msmt_planting = measurement,
         weed_group = weedspecies)

write_csv(dat, "rd_cc-database.csv")
