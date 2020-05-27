#############################
# Purpose: Create figures for manuscript
# Author: Gina Nichols
# Date Created: Sept 9 2019
#
# Date last modified: Sept 9 2019
#                     Dec 3 2019 (added yield/weed fig, updated map)
#                     Dec 5 2019 (added mean vals to yield/weed fig)
#                     Dec 26 2019 (added modifier fig)
#                     Jan 14 2020 (add colors to mod fig)
#                     Jan 24 2020 (updated mod fig)
#                     Feb 13 2020 (another version of mod fig)
#                     March 2 2020 (short manu figs)
#
#
# Inputs: td_cc-database-clean-long, td_stats-overall
#
# Outputs: several figs
#
# NOtes: 
#
##############################


rm(list=ls())
library(tidyverse)
library(usmap) # pre-loaded maps package
library(ggpubr) # has nice themes
library(wesanderson) # colors
library(patchwork) #--multiple plots


# figure things -----------------------------------------------------------

wes <- wes_palette("Zissou1")
mypal4 <- c("royalblue", wes[c(2,3,5)])
mypal3 <- wes[c(1,4,5)]

# Read in data ------------------------------------------------------------
dat <- read_csv("working_data/wd_long.csv")


# fig supp ----------------------------------------------------------------

# Fig S1.2 map ------------------------------------------------------------


map_all <- as_tibble(map_data('state'))

map_crnblt <- map_all %>%
  filter(region %in% c("illinois", "iowa", "indiana", 
                       "michigan", "minnesota", "missouri", 
                       "nebraska", "ohio", "wisconsin", "kansas",
                       "north dakota", "south dakota"))

dat1 <- 
  dat %>%
  group_by(resp, lat, long, cropsys_tillage) %>%
  summarise(n = n()) %>% 
  mutate(cropsys_tillage = recode(cropsys_tillage,
                                  `N` = "Zero-till",
                                  `Y` = "Tilled"))

ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), 
               colour = "black", fill = "white", color = "gray80", size = 1.2) +
  geom_jitter(data = dat1, width = 0.5, colour = "black", alpha = 0.5,
              aes(x = long, y = lat, 
                  fill = cropsys_tillage,
                  pch = resp,
                  size = n)) + 
  scale_shape_manual(values = c(21, 24), labels = c("Biomass", "Density")) + 
  labs(x = "Longitude", y = "Latitude",
       shape = "Weed Response",
       size = "Number of Comparisons", 
       fill = "System Tillage") + 
  scale_fill_manual(values = c("orange2", "purple4")) +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 2)),
         pch = guide_legend(override.aes = list(size = 2))) +
  coord_quickmap() + 
  theme_minimal()  


# Don't worry, the density shape shows up when you write it
ggsave("figs/manusupp-s1_map.png")




# fig w ind study effect sizes --------------------------------------------
library(ggforce)
library(naniar)


res_study <- read_csv("stats_summaries/ss_effect-size-by-study.csv")

res_study %>% 
  mutate(resp = recode(resp, 
                       "bio" = "Weed Biomass",
                       "den" = "Weed Density")) %>% 
  ggplot(aes(reorder(pub_reference, estimate), estimate, color = resp)) + 
  geom_point(size = 4) + 
  geom_linerange(aes(ymin = cilo, ymax = cihi)) + 
  geom_hline(yintercept = 0, color = "gray80") +
  coord_flip() +
  guides(color = F) +
  scale_color_manual(values = c(wes[1], wes[5])) +
  labs(x = NULL,
       y = "Effect Size") + 
  facet_grid(resp~.) + 
  theme_bw()

ggsave("figs/manusupp-s1_study-effect-sizes.png", width = 5, height = 4)


# Fig 4 yieldLRR and weedLRR ----------------------------------------------------

mweeds <- read_csv("_tidydata/sd_mods-weeds.csv") %>% 
  filter(resp == "bio") %>% 
  filter(mod_code == "crop_follow") %>% 
  rename("crop_follow" = modlvl) 

mylds <- read_csv("_tidydata/sd_mods-yield.csv") %>% 
  filter(resp == "bio") %>% 
  filter(mod_code == "crop_follow") %>% 
  rename("crop_follow" = modlvl) 

dat %>% 
  select(LRR, yieldLRR, crop_follow) %>% 
  filter(!is.na(yieldLRR)) %>%
  group_by(crop_follow) %>% 
  arrange(crop_follow, LRR) %>%
  mutate(rnk = 1:n()) %>% 
  gather(LRR:yieldLRR, key = resp, value = val) %>%
  mutate(myhue = ifelse(resp == "LRR", resp, paste(crop_follow, resp, sep = "_")),
         meanval = ifelse(crop_follow == "corn", -0.54, -1.06)) %>% 
  #--fig
  ggplot(aes(rnk, val)) + 

  #--overall means of weeds  
  geom_point(data = mweeds, 
             aes(x = -10, y = est), color = "red", size = 3) +
  geom_segment(data = mweeds,
             aes(
               x = -10, xend = -10,
               y = cilo,
               yend = cihi),
             color = "red", size = 1.2) +
  #--overall means of yieldss  
  geom_point(data = mylds, 
             aes(x = -5, y = est), color = "black", size = 3) +
  geom_segment(data = mylds,
               aes(
                 x = -5, xend = -5,
                 y = cilo,
                 yend = cihi),
               color = "black", size = 1.2) +
  geom_bar(aes(fill = resp), stat = "identity", position = "dodge") + 
  #--labels
  geom_text(y = -3, x = 60, label = "Decrease") +
  geom_text(y = 3, x = 60, label = "Increase") +
  # deets
  coord_flip() + 
  scale_fill_manual(values = c(LRR = "red",
                               yieldLRR = "black"),
                    name = NULL,
                    labels = c(LRR = "Weeds",
                               yieldLRR = "Crop Yield")) +
  labs(title = "Effects of cover crops on yield and weeds are independent", 
       x = NULL,
       y = "Log of (Treatment / Control)") + 
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() + 
  theme() + 
  facet_grid(crop_follow~.)

ggsave("_figs/manu_fig3_yield-weeds-bycrop.png")



# not by crop -------------------------------------------------------------


mweeds2 <- read_csv("_tidydata/sd_overall-weeds.csv") 
mylds2 <- read_csv("_tidydata/sd_overall-yield.csv") 


dat %>% 
  select(resp, LRR, yieldLRR) %>%
 # filter(resp == "den") %>% 
  filter(!is.na(yieldLRR)) %>%
  group_by(resp) %>% 
  arrange(resp, LRR) %>% 
  mutate(rnk = 1:n()) %>% 
  gather(LRR:yieldLRR, key = myresp, value = val) %>%
  
  #--fig
  ggplot(aes(rnk, val)) + 
  
  #--overall means of weeds  
  geom_point(data = mweeds2, 
             aes(x = -10, y = avg), color = "red", size = 3) +
  geom_segment(data = mweeds2,
               aes(
                 x = -10, xend = -10,
                 y = cilo,
                 yend = cihi),
               color = "red", size = 1.2) +
  #--overall means of yieldss  
  geom_point(data = mylds2, 
             aes(x = -5, y = avg), color = "black", size = 3) +
  geom_segment(data = mylds2,
               aes(
                 x = -5, xend = -5,
                 y = cilo,
                 yend = cihi),
               color = "black", size = 1.2) +
  #--bars
  geom_bar(aes(fill = myresp), stat = "identity", position = "dodge") + 
  #--labels
  geom_text(y = -3, x = 30, label = "Decrease") +
  geom_text(y = 3, x = 30, label = "Increase") +
  geom_hline(yintercept = 0) +
  # deets
  coord_flip() + 
  scale_fill_manual(values = c(LRR = "red",
                               yieldLRR = "black"),
                    name = NULL,
                    labels = c(LRR = "Weeds",
                               yieldLRR = "Crop Yield")) +
  labs(#title = "Effects of cover crops on yield and weeds are independent", 
       x = NULL,
       y = "Log of (Cover-Cropped Treatment / Control)") + 
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        legend.position = "bottom") +
  facet_wrap(.~resp, scales = "free")

ggsave("_figs/manu_fig3_yield-weeds-bycrop.png")


dat %>% 
  select(resp, LRR, yieldLRR) %>%
  # filter(resp == "den") %>% 
  filter(!is.na(yieldLRR)) %>%
  group_by(resp) %>% 
  arrange(resp, LRR) %>% 
  mutate(rnk = 1:n()) %>% 
  gather(LRR:yieldLRR, key = myresp, value = val) %>%
  
  #--fig
  ggplot(aes(rnk, val)) + 
  
  #--overall means of weeds  
  geom_point(data = mweeds2, 
             aes(x = -10, y = avg), color = "red", size = 3) +
  geom_segment(data = mweeds2,
               aes(
                 x = -10, xend = -10,
                 y = cilo,
                 yend = cihi),
               color = "red", size = 1.2) +
  #--overall means of yieldss  
  geom_point(data = mylds2, 
             aes(x = -5, y = avg), color = "black", size = 3) +
  geom_segment(data = mylds2,
               aes(
                 x = -5, xend = -5,
                 y = cilo,
                 yend = cihi),
               color = "black", size = 1.2) +
  #--bars
  geom_bar(aes(fill = myresp), stat = "identity", position = "dodge") + 
  #--labels
  geom_text(y = -3, x = 30, label = "Decrease") +
  geom_text(y = 3, x = 30, label = "Increase") +
  geom_hline(yintercept = 0) +
  # deets
  coord_flip() + 
  scale_fill_manual(values = c(LRR = "red",
                               yieldLRR = "black"),
                    name = NULL,
                    labels = c(LRR = "Weeds",
                               yieldLRR = "Crop Yield")) +
  labs(#title = "Effects of cover crops on yield and weeds are independent", 
    x = NULL,
    y = "Log of (Cover-Cropped Treatment / Control)") + 
  scale_y_continuous(limits = c(-6, 6)) +
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        legend.position = "bottom") +
  facet_wrap(.~resp, scales = "free")


####################################################################
# old figs andrea ---------------------------------------------------------




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

ggsave("_figs/data-by-cc-type.png")


# Fig 4 - Termination method -----------------------------------------------------

# Get n values for labels
#
dat_lab4 <- dat %>%
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density")) %>%
  filter(!is.na(LRR)) %>%
  group_by(resp, ccterm_meth, tmeth_nice) %>%
  summarise(n = n()) %>%
# D = dual, M = mechanical, H = herb, W = winterkill
  filter(!is.na(ccterm_meth))

# Create figure
#

# Prepare data
dat %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  mutate(resp = recode(resp, 
                       bioLRR = "Weed Biomass",
                       denLRR = "Weed Density"),
         presp = exp(LRR) * 100 - 100) %>%
  filter(!is.na(ccterm_meth)) %>%
  
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

#ggsave("_figs/data-num-studies-by-pubyear.png")


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

#ggsave("../_figs/data-Win-Win-scenariosv2.png")


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

#ggsave("../_figs/data-CCbiomass-vs-LRR.png")

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

#ggsave("../_figs/data-CCbiomass-vs-LRR-by-cropfollow-and-cctype-v2.png")

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

#ggsave("../_figs/data-CCbiomass-vs-LRR-by-cctype.png")

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



# fig supp ----------------------------------------------------------------

# Get map data ------------------------------------------------------------


map_all <- as_tibble(map_data('state'))

map_crnblt <- map_all %>%
  filter(region %in% c("illinois", "iowa", "indiana", 
                       "michigan", "minnesota", "missouri", 
                       "nebraska", "ohio", "wisconsin", "kansas",
                       "north dakota", "south dakota"))


# Figure 1 - map ----------------------------------------------------------

dat1 <- 
  dat %>%
  group_by(resp, lat, long) %>%
  summarise(n = n(),
            aridity_index = mean(aridity_index))

summary(dat1) #--median aridity_index is 0.86

#--where are we missing numbers? study 13. andrea's on it. 
dat %>%
  filter(is.na(aridity_index))

ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), 
               color = "black", fill = "white", color = "gray80", size = 1.2) +
  geom_jitter(data = dat1, width = 0.5, color = "black", alpha = 0.5,
              aes(x = long, y = lat, 
                  fill = aridity_index,
                  pch = resp,
                  size = n)) + 
  scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    midpoint = 0.65
  ) + 
  scale_shape_manual(values = c(21, 24), labels = c("Biomass", "Density")) + 
  labs(x = "Longitude", y = "Latitude",
       shape = "Weed Response",
       size = "Number of Comparisons", 
       fill = "Aridity Index") + 
  coord_quickmap() + 
  theme_minimal()  

# Don't worry, the density shape shows up when you write it
ggsave("_figs/manu_fig1_map-sites.png")

# Figure 1a - map by tillage ----------------------------------------------------------

dat1 <- 
  dat %>%
  group_by(resp, lat, long, cropsys_tillage) %>%
  summarise(n = n()) %>% 
  mutate(cropsys_tillage = recode(cropsys_tillage,
                                  `N` = "Zero-till",
                                  `Y` = "Tilled"))

#--where are we missing numbers? study 13. andrea's on it. 

ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), 
               colour = "black", fill = "white", color = "gray80", size = 1.2) +
  geom_jitter(data = dat1, width = 0.5, colour = "black", alpha = 0.5,
              aes(x = long, y = lat, 
                  fill = cropsys_tillage,
                  pch = resp,
                  size = n)) + 
  scale_shape_manual(values = c(21, 24), labels = c("Biomass", "Density")) + 
  labs(x = "Longitude", y = "Latitude",
       shape = "Weed Response",
       size = "Number of Comparisons", 
       fill = "System Tillage") + 
  scale_fill_manual(values = c("orange2", "purple4")) +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 2)),
         pch = guide_legend(override.aes = list(size = 2))) +
  coord_quickmap() + 
  theme_minimal()  


# Don't worry, the density shape shows up when you write it
ggsave("_figs/manu_fig1_map-sites-tillage.png")



