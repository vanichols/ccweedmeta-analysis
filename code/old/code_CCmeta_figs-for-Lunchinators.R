#############################
##
# Author: Gina Nichols
#
# Date Created: Oct 22
#
# Date last modified: Oct 30 2018 - by Andrea 
#                     Nov 1 2018 - Gina, added biomass separated by following crop graph
#                     Oct 29 2019 - updated code, new database
#
# Purpose: Create figures for ASA presentation
#
# Inputs: td_cc-database-clean
#
# Outputs: several figs
#
# NOtes: 
#
##############################


rm(list=ls())
library(tidyverse)
library(usmap) # pre-loaded maps package
library(ggridges) # makes nice distribution plots
library(ggpubr) # has nice themes


# Read in data ------------------------------------------------------------

dat <- read_csv("_tidydata/td_cc-database-clean-long.csv")%>%
  mutate(tmeth_nice = ifelse(ccterm_meth == "W", "Winterkill", NA),
         tmeth_nice = ifelse(ccterm_meth == "M", "Mechanical", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth == "H", "Herbicide", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth) == "D", "Combo Mech/Herb", tmeth_nice)



# Fig 6 - WW/LL -----------------------------------------------------

lunchdat <- 
  dat %>% 
  filter(!is.na(LRR)) %>%
  select(obs_no, yieldLRR, LRR) %>%
  rename(weedLRR = LRR)

lunchdat %>% write_csv("_tidydata/sd_yieldLRR-weedLRR.csv")

# NOTE: NAs indicate weed points without yields
(dat_lab6 <- 
  dat %>% 
  filter(!is.na(LRR)) %>%
  mutate(ww_color = ifelse( (LRR >= 0 & yieldLRR >= 0 ), "Mw-My", NA),
         ww_color = ifelse( (LRR >= 0 & yieldLRR < 0 ), "Mw-Ly", ww_color),
         ww_color = ifelse( (LRR < 0 & yieldLRR < 0 ), "Lw-Ly", ww_color),
         ww_color = ifelse( (LRR < 0 & yieldLRR >= 0 ), "Lw-My", ww_color)) %>%
  group_by(ww_color) %>%
  summarise(n = n()) )

# Reduced weeds # of points  
lessWmoreY <- as.numeric(dat_lab6 %>% filter(ww_color == "Lw-My") %>% pull(n))
lessWlessY <- as.numeric(dat_lab6 %>% filter(ww_color == "Lw-Ly") %>% pull(n))
# Inc weeds
moreWlessY <- as.numeric(dat_lab6 %>% filter(ww_color == "Mw-Ly") %>% pull(n))
moreWmoreY <- as.numeric(dat_lab6 %>% filter(ww_color == "Mw-My") %>% pull(n))



dat %>% 
  mutate(ww_color = ifelse( (LRR > 0 & yieldLRR > 0 ), "LL", NA),
         ww_color = ifelse( (LRR > 0 & yieldLRR < 0 ), "LW", ww_color),
         ww_color = ifelse( (LRR < 0 & yieldLRR < 0 ), "WW", ww_color),
         ww_color = ifelse( (LRR < 0 & yieldLRR > 0 ), "WL", ww_color)) %>%
  group_by(ww_color) %>%
  mutate(n = n()) %>%
  
  # Remove that one stinker point, just for graphing
  filter(yieldLRR > -4) %>%
  
  ggplot(aes(yieldLRR, LRR)) + 
  geom_point(aes(color = ww_color), size = 5) + 
  
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  
  coord_cartesian(xlim = c(-3, 3), ylim = c(-5, 5)) + 
  geom_text(x = -3, y = 4, label = paste0("(-) yield (+) weeds\n n = ", moreWlessY), hjust = 0, size = 6) +
  
  geom_text(x = -3, y = -4, label = paste0("(-) yield (-) weeds\n n = ", lessWlessY), hjust = 0, size = 6) + 
  
  geom_text(x = 3, y = 4, label = paste0("(+) yield (+) weeds\n n = ", moreWmoreY), hjust = 1, size = 6) + 
  
  geom_text(x = 3, y = -4, label = paste0("(+) yield (-) weeds\n n = ", lessWmoreY), hjust = 1, size = 6) + 

  guides(color = F) + 
  labs(x = "Yield Response to Cover Crop", y = "Weed Response to Cover Crop") +
  scale_color_manual(values = c("gray", "red", "green3", "gray")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("../_figs/data-Win-Win-scenariosv2.png")


# Fig 7 - cc biomass vs RR -----------------------------------------------------

dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%

  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, pch = 21, fill = "green4") + 
#  geom_smooth(method = "lm", se = F, color = "red", size = 4) +
  
#  geom_vline(xintercept = 0) + 
  
  labs(x = "Cover Crop Biomass [kg/ha]", y = "Weed Response to Cover Crop") +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("../_figs/data-CCbiomass-vs-LRR.png")

# Separate by cc type
dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  filter(cc_type %in% c("grass", "legume")) %>%
  
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, pch = 21, aes(fill= cc_type)) + 
  geom_smooth(method = "lm", se = F, color = "red", size = 3) +
  #geom_vline(xintercept = 0) + 
  
  labs(x = "Cover Crop Biomass [kg/ha]", y = "Weed Response to Cover Crop") +
  theme_light() + 
  facet_grid(cc_type~resp) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

#ggsave("../_figs/data-CCbiomass-vs-LRR-by-resp.png")

# Separate by crop_follow AND cc_type - All grasses are followed by soybeans
dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  filter(crop_follow %in% c("corn", "soybean")) %>%
  filter(cc_type %in% c("legume", "grass")) %>%
  mutate(crop_follow = recode(crop_follow, 
                              corn = "Maize",
                              soybean = "Soybean")) %>%
  
  ggplot(aes(cc_bio_kgha, LRR, fill = cc_type)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, pch = 22) + 
  #geom_smooth(method = "lm", se = F, color = "red", size = 3) +
  #geom_vline(xintercept = 0) + 
  
  labs(x = "Cover Crop Biomass [kg/ha]", y = "Weed Response to Cover Crop",
         title = "All Grass Cover Crops are Followed By Soybeans") +
  theme_bw() +
  
  scale_fill_manual(values = c("green3", "blue3"),
                    name = "CC-Type") +
  

 #scale_shape_manual(values = c(22, 21)) + 
  #                    labels = c("legume.Maize", "grass.Soybean", "legume.Soybean"),
  #                    name = "CC-Type.Following-Crop") +
  # 
  # 
  facet_grid(.~crop_follow) +
  guides(shape = F) +  
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.4)),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.4)),
        legend.title = element_text(size = rel(1.5))) 

ggsave("../_figs/data-CCbiomass-vs-LRR-by-cropfollow-and-cctype-v2.png")

dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, pch = 21, fill = "green4") + 
  geom_smooth(method = "lm", se = F, color = "red", size = 4) +
  #geom_vline(xintercept = 0) + 
  
  labs(x = "Cover Crop Biomass [kg/ha]", y = "Weed Response to Cover Crop") +
  theme_classic() + 
  facet_grid(.~cc_type) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("../_figs/data-CCbiomass-vs-LRR-by-cctype.png")

##AB changes
##removing Fisk study with low biomass numbers
dat2 <- dat[!dat$study ==5, ]

##for crop following, removing NAs and corn/soy groups - studies 11, 12, 13
dat2 <- dat2[!dat2$study ==11, ]
dat2 <- dat2[!dat2$study ==12, ]
dat2 <- dat2[!dat2$study ==13, ]

#remove outliers
dat2 <- dat2[!dat2$obs_no ==137, ] 
dat2 <- dat2[!dat2$obs_no ==49, ] 
dat2 <- dat2[!dat2$obs_no ==51, ] 
#this was the only way I could keep the dataframe from adding random NAs

dat2 %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, pch = 21, fill = "green4") + 
  geom_smooth(method = "lm", se = F, color = "red", size = 4) +
  #geom_vline(xintercept = 0) + 
  
  labs(x = "Cover Crop Biomass [kg/ha]", y = "Weed Response to Cover Crop") +
  theme_classic() + 
  facet_grid(.~crop_follow) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("../_figs/data-CCbiomass-vs-LRR-by-cropfollow.png")

