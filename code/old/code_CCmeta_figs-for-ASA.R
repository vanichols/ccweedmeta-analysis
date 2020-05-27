#############################
##
# Author: Gina Nichols
#
# Date Created: Oct 22
#
# Date last modified: Oct 30 2018 - by Andrea 
#                     Nov 1 2018 - Gina, added biomass separated by following crop graph
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

# Set working directory
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

# Read in data ------------------------------------------------------------
dat <- read_csv("../_tidydata/td_cc-database-clean.csv") %>%
  mutate(tmeth_nice = ifelse(cc_termMETH2 == "W", "Winterkill", NA),
         tmeth_nice = ifelse(cc_termMETH2 == "M", "Mechanical", tmeth_nice),
         tmeth_nice = ifelse(cc_termMETH2 == "H", "Herbicide", tmeth_nice),
         tmeth_nice = ifelse(cc_termMETH2 == "D", "Combo Mech/Herb", tmeth_nice))



# Get map data ------------------------------------------------------------


map_all <- as.tibble(map_data('state'))

map_crnblt <- map_all %>%
  filter(region %in% c("illinois", "iowa", "indiana", 
                       "michigan", "minnesota", "missouri", 
                       "nebraska", "ohio", "wisconsin"))


# Figure 1 - map ----------------------------------------------------------


ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), fill = "white", color = "gray80") +
  geom_point(data = dat, pch = 21, size = 4, color = "black", fill = "red",
             aes(x = long, y = lat)) 

ggsave("../_figs/map-sites.png")


# Figure 2 - distribution of points ----------------------------------------------------------

dat_lab <- dat %>%
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density")) %>%
  filter(!is.na(LRR)) %>%
  group_by(resp) %>%
  summarise(n = n())

dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density"),
         presp = exp(LRR) * 100 - 100) %>%
  
  ggplot(aes(LRR, resp)) + 
  
  geom_density_ridges(aes(fill = resp), alpha = 1, scale = 1.1) + 
  
  geom_text(data = dat_lab, aes(-6, resp, label = paste0("n = ", n)), 
            vjust = 1.5, color = "gray50", fontface = "italic") + 
  
 # geom_text(x = -6, y = 3, label = "Cover Crop Decrease Weeds", 
 #            fontface = "italic", color = "gray50", size = 5, hjust = 0) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) + 
  
  labs(x = "Log of Response Ratio", y = NULL) +
  guides(fill = F) + 
  scale_fill_manual(values = c("blue3", "green2")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14))

ggsave("../_figs/data-distributionV2-presentation.png")


# Fig 3 - CC species -----------------------------------------------------

dat_lab3 <- dat %>%
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density")) %>%
  filter(!is.na(LRR)) %>%
  # mix and brassica only have 2/3 points
  filter(cc_type != "mix", cc_type != "brassica") %>%
  group_by(resp, cc_type) %>%
  summarise(n = n())

dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density"),
         presp = exp(LRR) * 100 - 100) %>%
  # mix and brassica only have 2/3 points
  filter(cc_type != "mix", cc_type != "brassica") %>%
  
  # Pipe new data to ggplot2
  
  ggplot(aes(LRR, cc_type)) + 
  geom_density_ridges(aes(fill = resp), alpha = 1, scale = 1.1) + 
  geom_text(data = dat_lab3, aes(-7, cc_type, label = paste0("n = ", n)), 
            vjust = 0, hjust = 0, color = "gray50", fontface = "italic") + 
  
  #geom_text(x = -5, y = 3, label = "Cover Crops Decrease Weeds", 
  #          fontface = "italic", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) + 
  
  facet_grid(~resp) + 
  
  labs(x = "Log of Response Ratio", y = NULL) +
  guides(fill = F) + 
  scale_fill_manual(values = c("blue3", "green2")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave("../_figs/data-by-cc-type.png")


# Fig 4 - Termination method -----------------------------------------------------

# Get n values for labels
#
dat_lab4 <- dat %>%
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density")) %>%
  filter(!is.na(LRR)) %>%
  group_by(resp, cc_termMETH2, tmeth_nice) %>%
  summarise(n = n()) %>%
# D = dual, M = mechanical, H = herb, W = winterkill
  filter(!is.na(cc_termMETH2))

# Create figure
#

# Prepare data
dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density"),
         presp = exp(LRR) * 100 - 100) %>%
  filter(!is.na(cc_termMETH2)) %>%
  
# Pipe new data to ggplot  
  ggplot(aes(LRR, tmeth_nice)) + 
  geom_density_ridges(aes(fill = resp), alpha = 1, scale = 1.1) + 
  geom_text(data = dat_lab4, aes(-7, tmeth_nice, label = paste0("n = ", n)), 
            vjust = -0.5, hjust = 0, color = "gray50", fontface = "italic") + 
  
  #geom_text(x = -5, y = 3, label = "Cover Crops Decrease Weeds", 
  #          fontface = "italic", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) + 
  
  facet_grid(~resp) + 
  
  labs(x = "Log of Response Ratio", y = NULL) +
  guides(fill = F) + 
  scale_fill_manual(values = c("blue3", "green3")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))

ggsave("../_figs/data-by-term-meth2.png")



# Fig 5a - # studies by year -----------------------------------------------------

dat %>%
  group_by(study) %>%
  # Condense to 1 value per study
  summarise(pub_year = mean(pub_year)) %>%
  # Find # per year
  group_by(pub_year) %>%
  summarise(n = n()) %>%
  
  ggplot(aes(pub_year, n)) + 
  geom_col(fill= "red", color = "black", size = 2) +
  labs(y = "Number of Studies", x = NULL) +
  theme_classic() + 
  scale_x_continuous(breaks = c(1990:2018)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 0))

ggsave("../_figs/data-num-studies-by-pubyear.png")


# Fig 6 - WW/LL -----------------------------------------------------

# NOTE: NAs indicate weed points without yields
(dat_lab6 <- 
  dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
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
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
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

