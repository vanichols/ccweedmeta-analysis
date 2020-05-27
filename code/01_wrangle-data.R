#############################
#
# Author: Gina Nichols
#
# Date Created: March 5 2020
#
# Date last modified: 
#
# Purpose: prepare data for analysis
# 
# Inputs: ccweeddat (from the ccweedmetapkg)
#
# Outputs: wd_wide and wd_long in working_data folder
#
# Steps: 1) calculate termination-to-planting gaps
#        2) Find and address 0 values (there are none)
#        3) Calc LRR
#        4) Adjust modifiers
#        5) Address extreme values
#
# NOtes: There are no 0s
#        Removing one outlier that drastically changed cc_bio vs LRR estimates due to very low LRR
#        
#
##############################

rm(list=ls())
library(tidyverse)
library(readxl) #--to read excel files
library(naniar) #--allows you to replace . with NA
library(lubridate) #--for dates
library(janitor) #--to clean things
#devtools::install_github("vanichols/ccweedmetapkg")
library(ccweedmetapkg) #--contains raw dataset
library(here)

raw <- as_tibble(ccweeddat)


# step 1 term-planting gaps------------------------------------------------------------------
# Estimate termination-to-planting gap
d1 <- 
  raw %>%
  #--cover crop planting dates
  mutate(
  #indicate you are estimating
    cc_pest = case_when(
      (is.na(cc_p_dom) & !is.na(cc_p_mnth2)) ~ "Y"),
    #if there is a month but no date, assign dom as 15
    cc_pDOM = ifelse( (is.na(cc_p_dom) & !is.na(cc_p_mnth)), 15, cc_p_dom),
    #paste things to make a date, pretend everything is 2018
    ccpl_lubdate = ifelse(!is.na(cc_p_dom), paste(cc_p_mnth2, cc_p_dom, "2018", sep = "/"), NA),
    ccpl_lubdate = mdy(ccpl_lubdate),
    ccpl_doy = yday(ccpl_lubdate)) %>% 
  #--cover crop termination dates
  mutate(
    # indicate you are estimating
    cc_termest = case_when(
      (is.na(cc_term_dom) & !is.na(cc_term_mnth2)) ~ "Y"),
    # if there is a month but no date, assign it as 1
    cc_term_dom = ifelse( (is.na(cc_term_dom) & !is.na(cc_term_mnth2)), 1, cc_term_dom),
    # paste things to make a date, pretend all was termed in 2019
    ccterm_lubdate = ifelse(!is.na(cc_term_dom), paste(cc_term_mnth2, cc_term_dom, "2019", sep = "/"), NA),
    ccterm_lubdate = mdy(ccterm_lubdate),
    ccterm_doy = yday(ccterm_lubdate)) %>%
  #--crop planting dates
  mutate(
    # indicate you are estimating
    crop_pest = case_when(
      (is.na(crop_p_dom) & !is.na(crop_p_mnth2)) ~ "Y"),
    # if there is a month but no date, assign it as 30
    crop_p_dom = ifelse( (is.na(crop_p_dom) & !is.na(crop_p_mnth2)), 30, crop_p_dom),
    # paste things to make a date
    croppl_lubdate = ifelse(!is.na(crop_p_dom), paste(crop_p_mnth2, crop_p_dom, "2019", sep = "/"), NA),
    croppl_lubdate = mdy(croppl_lubdate),
    croppl_doy = yday(croppl_lubdate)) %>% 
  #--calc gaps
  mutate(
    cc_growdays = time_length(interval(ymd(ccpl_lubdate), ymd(ccterm_lubdate)), "day"),
    termgap_days = time_length(interval(ymd(ccterm_lubdate), ymd(croppl_lubdate)), "day"))
  
  


# step 2 address 0s--------------------------------------------------------------
# find and address 0s

d1 %>%
  # Identify pairs of 0s (there are currently none)
  mutate(repl_1den = ifelse( (cc_wden == 0 & ctl_wden == 0), "yes", "no"),
         repl_1bio = ifelse( (cc_wbio == 0 & ctl_wbio == 0), "yes", "no")) %>%
  filter(repl_1bio == "yes" | repl_1den == "yes")
  
  
d2 <- d1


# step 3 calc log-response-ratios and weights-----------------------------------------

d3 <- 
  d2 %>% 
  #calculate LRR
  mutate(bioLRR = log(cc_wbio / ctl_wbio),
         denLRR = log(cc_wden / ctl_wden),
         yieldLRR = log(cc_yield_kgha / ctl_yield_kgha) ) %>% 
  #calc weight based on reps
  mutate(wgt = (reps * reps) / (reps + reps))
  

# step 4 adjust modifiers as desired ------------------------------------------------

d4 <- 
  d3 %>% 
  #categorize cc types
  mutate(cc_type2 = recode(cc_type,
                           brassica = "non-grass",
                           legume = "non-grass",
                           mix = "non-grass")) %>% 
  mutate(cc_bm_Mgha = cc_bm_kgha / 1000) 


# step 5 adjust extreme value ---------------------------------------------
#based on leave-one-out sensitivity analysis

secondmin <- d4 %>% filter(obs_no == 145) %>% select(bioLRR) %>% pull()
d4 %>% filter(obs_no == 76) %>% select(bioLRR) %>% pull()

d5 <-
  d4 %>% 
  mutate(bioLRR = ifelse(obs_no == 76, secondmin, bioLRR))

#check it
d5 %>% select(bioLRR) %>% arrange(bioLRR)


# Write both a long and short form of the data -------------------------------------------------

d5 %>% write_csv("working_data/wd_wide.csv")

d5_long <- 
  d5 %>% 
  gather(bioLRR, denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp,
                       "bioLRR" = "bio",
                       "denLRR" = "den")) %>%
  filter(!is.na(LRR)) 

d5_long %>% write_csv("working_data/wd_long.csv")

