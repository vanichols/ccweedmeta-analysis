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
#                     Sept 6 2019 - Made cleaning google sheet it's own code
#                     SEpt 9 2019 - eliminated MAT and MAP bc they are crap, fixed dates
#                     Sept 17 - trying to indicate where dates were estimated, not working!!!!!!!!
#                     Oct 2 - figured out dates. Looking at yield vs term-plant gap
#                     Dec 16 2019 - eliminated one outlier in bioLRR, changed to second lowest value 
#                     Jan 2 2020 - cleaning up one last time
#                     Feb 25 2020 - ok seriousy cleaning it, making it ready for datashare
#
#
# Purpose: Clean data
#          If both the ctl and cc biomass/density values are 0, change to 1s
#          Eliminate other comparisons w/0s 
#          Create 'master' database to use in all subsequent analyses
# 
# Inputs: rd_cc-database
#
# Outputs: td_cc-database-clean
#
# NOtes: There are no 0s (?)
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

datraw <- read_csv("_rawdata/rd_cc-database.csv") 

dat <- 
  datraw %>% 
  mutate(exp_type = str_remove_all(exp_type, " "),
         exp_type = str_remove_all(exp_type, "-"),
         cc_spec = str_replace_all(cc_spec, "/", "+")) %>% 
  mutate(cc_pMNTH = str_sub(cc_pMNTH, 1, 3),
         crop_pMNTH = str_sub(crop_pMNTH, 1, 3),
         cc_termMNTH = str_sub(cc_termMNTH, 1, 3)) %>% 
  mutate(cc_termMETH = ifelse(cc_termMETH == "mowed", "mowing", cc_termMETH),
         cc_termMETH = ifelse(cc_termMETH == "rolling", "roller crimper", cc_termMETH),
         cc_termMETH = ifelse(cc_termMETH == "winterkill", "winter kill", cc_termMETH),
         cc_termMETH = ifelse(cc_termMETH == "chopping", "mowing", cc_termMETH))



# make clean for datashare ------------------------------------------------

#--fix exp_types
datshare <- dat %>% 
  clean_names()

summary(as.numeric(datshare$cc_p_den_kgha))
levels(as.factor(datshare$cc_term_meth))

datshare %>% select(cc_term_mnth) %>% distinct()
tibble(var_names = names(datshare)) %>% write_csv("_datashare/cover-crop-weed-database-varnames.csv")

# Wrangle dates --------------------------------------------------

dat1 <- 
  dat %>%
  select(-MAT, -MAP, -cc_pMNTH, cc_termMNTH, -crop_pMNTH) %>%
  
  #--cover crop planting dates
  mutate(
    # indicate you are estimating
    cc_pest = case_when(
      (is.na(cc_pDOM) & !is.na(cc_pMNTH2)) ~ "Y"),
    
    # if there is a month but no date, assign it as 15
    cc_pDOM = ifelse( (is.na(cc_pDOM) & !is.na(cc_pMNTH2)),
                           15,
                           cc_pDOM),
    
    # paste things to make a date
    ccpl_lubdate = ifelse(!is.na(cc_pDOM),
                               paste(cc_pMNTH2, cc_pDOM, "2018", sep = "/"),
                               NA),
    ccpl_lubdate = mdy(ccpl_lubdate),
    ccpl_doy = yday(ccpl_lubdate)) %>%
  select(-cc_pMNTH2, -cc_pDOM, -cc_pDATE) %>%
  
  
  
  #--cover crop termination dates
  mutate(
    
    # indicate you are estimating
    cc_termest = case_when(
      (is.na(cc_termDOM) & !is.na(cc_termMNTH2)) ~ "Y"),
    
    # if there is a month but no date, assign it as 1
    cc_termDOM = ifelse( (is.na(cc_termDOM) & !is.na(cc_termMNTH2)),
                      1,
                      cc_termDOM),
    
    # paste things to make a date
    ccterm_lubdate = ifelse(!is.na(cc_termDOM),
                          paste(cc_termMNTH2, cc_termDOM, "2019", sep = "/"),
                          NA),
    
    ccterm_lubdate = mdy(ccterm_lubdate),
    ccterm_doy = yday(ccterm_lubdate)) %>%
  
  select(-cc_termMNTH2, -cc_termDOM) %>%
  

  #--crop planting dates
  mutate(
    # indicate you are estimating
    crop_pest = case_when(
      (is.na(crop_pDOM) & !is.na(crop_pMNTH2)) ~ "Y"),
    
    # if there is a month but no date, assign it as 30
    crop_pDOM = ifelse( (is.na(crop_pDOM) & !is.na(crop_pMNTH2)),
                      30,
                      crop_pDOM),
    
    # paste things to make a date
    croppl_lubdate = ifelse(!is.na(crop_pDOM),
                          paste(crop_pMNTH2, crop_pDOM, "2019", sep = "/"),
                          NA),
    croppl_lubdate = mdy(croppl_lubdate),
    croppl_doy = yday(croppl_lubdate)) %>%
  
  select(-crop_pMNTH2, -crop_pDOM)
  
  


# Address 0s --------------------------------------------------------------


dat_no0s <- 
  dat1 %>%
  # Identify pairs of 0s (there are currently none)
  mutate(repl_1den = ifelse( (ccden == 0 & ctlden == 0), "yes", "no"),
         repl_1bio = ifelse( (ccbio == 0 & ctlbio == 0), "yes", "no")) %>%
  
  # Replace pairs of 0s w/1s
  mutate(ccden = ifelse(repl_1den == "yes", 1, ccden),
         ctlden = ifelse(repl_1den == "yes", 1, ctlden),
         
         ccbio = ifelse(repl_1bio == "yes", 1, ccbio),
         ctlbio = ifelse(repl_1bio == "yes", 1, ctlbio)) %>%
  
  # Identify single 0s
  mutate(removeme = ifelse( (ccden == 0 & ctlden !=0), "yes", NA),
         removeme = ifelse( (ctlden == 0 & ccden !=0), "yes", removeme),
  
         removeme = ifelse( (ccbio == 0 & ctlbio !=0), "yes", removeme),
         removeme = ifelse( (ctlbio == 0 & ccbio !=0), "yes", removeme)) %>%
  filter(is.na(removeme))



# Calculate log response ratios -------------------------------------------


dat_LRR <- dat_no0s %>%
  
  # Calculate LRR
  mutate(bioLRR = log(ccbio / ctlbio),
         denLRR = log(ccden / ctlden),
         yieldLRR = log(cc_yield_kgha / ctl_yield_kgha) )



# Change cc_type to 2 cats ------------------------------------------------

dat_LRR2 <- dat_LRR %>%
  mutate(cc_type2 = recode(cc_type,
                           brassica = "non-grass",
                           legume = "non-grass",
                           mix = "non-grass"))

# Get days between plantings ------------------------------------------------

dat_LRR3 <- 
  dat_LRR2 %>%
  
  mutate(cc_growdays = time_length(
    interval(ymd(ccpl_lubdate), ymd(ccterm_lubdate)), 
    "day"),
    termgap_days = time_length(
      interval(ymd(ccterm_lubdate), ymd(croppl_lubdate)), 
      "day")
    ) %>%
  
  rename(ccterm_meth = cc_termMETH2,
         ccpl_meth = cc_pMETH,
         ccpl_den_kgha = cc_pDEN_kgha) %>%
  
  mutate(wgt = (reps * reps) / (reps + reps)) %>%
  
  
  select(obs_no, study, pub_reference, pub_year, loc_state, lat, long,
         
         reps, wgt, aridity_index, OM_pct, soil_type, 
         
         cc_type, cc_type2, cropsys_tillage, msmt_season, msmt_planting, weed_group,
         
         ccpl_den_kgha, ccpl_lubdate, ccpl_doy, cc_pest,
         
         ccterm_lubdate, ccterm_doy, cc_termest, ccterm_meth, 
         
         crop_follow, crop_pest,
         
         croppl_lubdate, croppl_doy,
         
         cc_bio_kgha, cc_growdays, termgap_days,
         
         
         
         bioLRR, denLRR, yieldLRR)
  



# Get ccbio in Mgha ------------------------------------------------

tdat <- 
  dat_LRR3 %>% 
  mutate(cc_bio_Mgha = cc_bio_kgha / 1000) 

# look at termination methods ---------------------------------------------

# I hate them, but leave them
tdat %>% 
  group_by(ccterm_meth) %>% 
  summarise(n = n())

# Change the one biomass outlier to next lowest value -----------------------------

secondmin <- tdat %>% filter(obs_no == 145) %>% select(bioLRR) %>% pull()
tdat %>% filter(obs_no == 76)

tdat <-
  tdat %>%
  mutate(bioLRR = ifelse(obs_no == 76, secondmin, bioLRR))

tdat %>% filter(obs_no == 76)
tdat %>% select(bioLRR) %>% arrange(bioLRR)
# density is fine


# Write to my folder -------------------------------------------------

write_csv(tdat, "_tidydata/td_cc-database-clean.csv")

tdat_long <- 
  tdat %>% 
  #select(-yieldLRR) %>%
  gather(bioLRR, denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp,
                       "bioLRR" = "bio",
                       "denLRR" = "den")) %>%
  filter(!is.na(LRR)) 

write_csv(tdat_long, "_tidydata/td_cc-database-clean-long.csv")


tdat_long %>%
  group_by(resp, cc_termest) %>%
  summarise(n = n())



# get unique studies ------------------------------------------------------

tdat %>% 
  select(pub_reference) %>% 
  unique() %>% 
  write_csv("_rawdata/rd_unique-studies.csv")


# biomasses for rafa ------------------------------------------------------


tdat %>%
  filter(cc_type2 == "grass") %>%
  select(loc_state, lat, long, 
         ccpl_doy, ccterm_doy, croppl_doy,
         cc_bio_kgha) %>%
  distinct() %>%
  write_csv("_tidydata/td_for-rafa.csv")


# explore for summarising in paper ----------------------------------------

tdat_long %>%
  #filter(cc_type == "brassica") %>%
  ggplot(aes(cc_type, LRR)) + 
  geom_point() + 
  facet_grid(resp~.)

tdat_long %>% filter(cc_termest == "Y")

tdat_long %>%
  filter(resp == "den") %>%
  filter(LRR < 0) %>%
  
  ggplot(aes(cc_bio_kgha)) + 
  geom_histogram()

tdat_long %>%
  filter(resp == "den") %>%
  filter(LRR < 0) %>%
  filter(cc_bio_kgha <10)
  
  
  
# categoricals

tdat_long %>%
  mutate_if(is.character, as.factor) %>%
  group_by(resp, crop_follow) %>%
  summarise(n = n())

tdat %>%
  filter(!is.na(yieldLRR)) %>%
  summarise(n = n())



# continuous
tdat_long %>%
  group_by(resp) %>%
  summarise(min = min(cc_bio_kgha, na.rm = T),
            max = max(cc_bio_kgha, na.rm = T))




