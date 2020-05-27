#############################
# Purpose: Create figures for PFI farminar talkk
# Author: Gina Nichols
# Date Created: Jan 23 2019
#
# Date last modified: #
#
# NOtes: 
#
##############################


rm(list=ls())
library(tidyverse)
library(usmap) # pre-loaded maps package
library(ggpubr) # has nice themes
library(wesanderson) # colors
library(here)

setwd(here::here())
setwd("../Box/1_Gina_Projects/proj_WeedMetaCC/")


# fig things --------------------------------------------------------------

pptgreen <- "#619B44"
pptblue <- "#46B2B5"
pptpink <- "#DC1A64"
pptorange <- "#FFA726"
pptyellow <- "#FFC000"
pptgray <- "#E7E6E6"

mypietheme <- theme(strip.background = element_rect(fill = "white", color = "black"),
                    strip.text = element_text(size = rel(1.3)),
                    legend.text = element_text(size = rel(1.3)))

mytheme <- theme(legend.position = c(0.1, 0.9),
                 legend.justification = c(0,1),
                 legend.background = element_rect(color = "black"),
                 axis.text = element_text(size = rel(1.2)),
                 legend.text = element_text(size = rel(1.3)),
                 axis.title = element_text(size = rel(1.3)))

Mgha_to_tonac <- (1000) * (2.2) * (1/2000) * (1/2.47) #--essentially half it

labseedsm2 = expression('Weed Seeds m'^"-2")
labseedsft2 = expression('Weed Seeds ft'^"-2")




# Maps ----------------------------------------------------------

map_crnblt <- as_tibble(map_data('state')) %>%
  filter(region %in% c("illinois", "iowa", "indiana", 
                       "michigan", "minnesota", "missouri", 
                       "nebraska", "ohio", "wisconsin", "kansas",
                       "north dakota", "south dakota"))


dat <- read_csv("working_data/wd_long.csv")
dat1 <- 
  dat %>%
  group_by(resp, lat, long, cropsys_tillage) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(resp = recode(resp,
                       bio = "Weed Biomass Study",
                      den = "Weed Density Study"))

#--map with points
ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), 
               colour = "black", fill = pptblue, size = 1.2) +
  geom_jitter(data = dat1, width = 0.7, size = 4,
              aes(x = long, y = lat, 
                  color = resp)) + 
  labs(x = NULL, 
       y = NULL, 
       color = NULL) + 
  scale_color_manual(values = c(pptpink, pptyellow)) +
  coord_quickmap() + 
  theme_minimal()  +
  mytheme + 
  theme(legend.position = "top",
        axis.text = element_blank())

ggsave("_figs/poster-map.png", bg = "transparent")


# Modifiers ------------------------------------------------------

dat <- read_csv("working_data/wd_long.csv") %>% 
  mutate(tmeth_nice = ifelse(cc_term_meth == "W", "Winterkill", NA),
         tmeth_nice = ifelse(cc_term_meth == "M", "Mechanical", tmeth_nice),
         tmeth_nice = ifelse(cc_term_meth == "H", "Herbicide", tmeth_nice),
         tmeth_nice = ifelse(cc_term_meth == "D", "Combo Mech/Herb", tmeth_nice),
         tmeth_nice = ifelse(cc_term_meth == "none", "None", tmeth_nice))


sigmods <- read_csv("stats_summaries/ss_catmods-contrasts.csv") %>% 
  filter(p.value < 0.05) %>%
  filter(mod_code != "crop_follow") %>% 
  select(mod_code) %>%
  mutate(mod_code = recode(mod_code,
                           "cc_type2bio" = 'cc_type2')) %>% 
  unique() %>% 
  pull()

dat2b <- read_csv("stats_summaries/ss_catmods.csv")

datfig <- dat2b %>% 
  filter(mod_code %in% sigmods) %>% 
  filter(mod_code != "msmt_season") %>%
  mutate(
    mod_code2 = case_when(
      mod_code == "cc_type2" ~ "Cover Crop\nType",
      mod_code == "weed_group" ~ "Weed Type",
      mod_code == "msmt_planting" ~ "Measurement\nTiming")) %>% 
  mutate(
    resp = case_when(resp == "bio" ~ "Weed Biomass", 
    resp == "den" ~ "Weed Density")) %>% 
  mutate(
    modlvl = case_when(
      modlvl == "before" ~ "Before Planting",
      modlvl == "after" ~ "After Planting",
      TRUE ~ modlvl)) %>%
  mutate(modlvl = str_to_title(modlvl)) %>% 
  mutate(modlvl = factor(modlvl, levels = c("Non-Grass", "Grass", 
                                            "After Planting","Before Planting", 
                                            "Perennial", "Summer Annual", "Winter Annual"))) %>% 
  mutate(absest = abs(est))
 


# select mods (poster) ------------------------------------------------------------

datfig %>% 
  filter(mod_code %in% c("cc_type2", "weed_group")) %>% 
 mutate(est_pct = 100 - exp(est)*100,
         sigfill = ifelse(cihi > 0, "NS", "S"),
         sigsize = ifelse(cihi > 0, 1, 5),
         est_pct = ifelse(sigfill == "S", paste0("-", round(est_pct, 0), "%"), " ")) %>% 
  ggplot(aes(modlvl, est)) + 
  geom_linerange(aes(x = modlvl, ymin = cilo, ymax = cihi, color = mod_code2), 
                 color = "black") +
  geom_point(aes(fill = sigfill, size = sigsize), 
             pch = 21, color = "black") +
   geom_text(aes(x = modlvl, y =  est, label = est_pct),
            vjust = -0.5, hjust = 1.5, fontface = "italic", size = 5) +
  facet_grid(mod_code2 ~resp, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  labs(y = "Weed Log-Response-Ratio",
       x = NULL, 
       size = NULL) +
  guides(size = F, color = F, fill = F) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-2, 1, by = 1)) +
  scale_fill_manual(values = c("S" = pptyellow, 
                               "NS" = "black")) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.4), color = "black"),
        axis.title.x = element_text(size = rel(1.8), color = "black"),
        axis.text.x = element_blank(),
        strip.text.y = element_text(angle = 360),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background = element_rect(fill = pptblue),
        panel.background = element_rect(fill = pptgray),
        panel.grid = element_blank())

ggsave("_figs/poster-mods.png", bg = "transparent", width = 10)



