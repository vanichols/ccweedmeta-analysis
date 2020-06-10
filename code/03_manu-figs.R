#############################
# Purpose: Create figures for manuscript
# Author: Gina Nichols
# Date Created: Sept 9 2019
#
# Last modified:  March 2 2020 (short manu figs)
#                 March 18 2020 (update from comments)
#                 April 30 2020 (update from comments)
#                 June 10 2020 (make figs for pub)
#
#
# Inputs: wd_long
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



# data --------------------------------------------------------------------

datraw <- read_csv("working_data/wd_long.csv")


# Figure 1 mods + bio reg + win/win ------------------------------------------------------


# mods --------------------------------------------------------------------

#--get list of sig modifiers to include
# note here it's called cc_type2bio, change it just to 
sigmods <- read_csv("stats_summaries/ss_catmods-contrasts.csv") %>% 
  filter(p.value < 0.05) %>%
  filter(mod_code != "crop_follow") %>% 
  mutate(mod_code = ifelse(mod_code == "cc_type2bio", "cc_type2", mod_code)) %>% 
  select(mod_code) %>% 
  unique() %>% pull()

# value for each one
dat1a <- read_csv("stats_summaries/ss_catmods.csv")

#n-values for cctype2, msmt_planting, weed_group
cctype_ns <- 
  datraw %>% 
  group_by(resp, cc_type2) %>% 
  summarise(n = n()) %>% 
  rename(modlvl = cc_type2) %>% 
  mutate(mod_code2 = "Cover Crop\nType") 

msmt_ns <- 
  datraw %>% 
  group_by(resp, msmt_planting) %>% 
  summarise(n = n()) %>% 
  rename(modlvl = msmt_planting) %>% 
  mutate(mod_code2 = "Measurement\nTiming") 

wg_ns <- 
  datraw %>% 
  group_by(resp, weed_group) %>% 
  summarise(n = n()) %>%
  filter(!is.na(weed_group)) %>% 
  rename(modlvl = weed_group) %>% 
  mutate(mod_code2 = "Weed Type")

fig1a_ns <- 
  bind_rows(cctype_ns, msmt_ns, wg_ns) %>% 
  mutate(myy = ifelse(resp == "bio", -2, -1))

datfig1a <- 
  dat1a %>% 
  filter(mod_code %in% sigmods) %>% 
  filter(mod_code != "msmt_season") %>%
  #--make mod_code2 match the n-data
  mutate(
    mod_code2 = case_when(
      mod_code == "cc_type2" ~ "Cover Crop\nType",
      mod_code == "weed_group" ~ "Weed Type",
      mod_code == "msmt_planting" ~ "Measurement\nTiming"))


fig1a_all <- 
  fig1a_ns %>% 
  left_join(datfig1a) %>% 
  ungroup() %>% 
  #--make resp look nice 
  mutate(
    resp = case_when(resp == "bio" ~ "Weed Biomass", 
                     resp == "den" ~ "Weed Density")) %>% 
  #--make mod_lvls look nice
  mutate(
    modlvl = case_when(
      modlvl == "grass" ~ "Grass",
      modlvl == "non-grass" ~ "Other",
      modlvl == "before" ~ "Before Planting",
      modlvl == "after" ~ "After Planting",
      modlvl == "summer annual" ~ "Summer Annual",
      modlvl == "winter annual" ~ "Winter Annual",
      modlvl == "perennial" ~ "Perennial",
      TRUE ~ modlvl)) %>% 
  mutate(
    modlvl = str_to_title(modlvl),
    est_pct = (1 - exp(est)) * 100,
    #--I want grass to appear first
    modlvl = factor(modlvl, levels = rev(c("Grass", "Other",
                                           "Before Planting", "After Planting",
                                           "Winter Annual", "Summer Annual", "Perennial"))))



fig1a <- 
  fig1a_all %>%
  mutate(sigfill = ifelse(cihi > 0, "NS", "S"),
         sigsize = ifelse(cihi > 0, 1, 5),
         est_pct = ifelse(sigfill == "S", paste0("-", round(est_pct, 0), "%"), " ")) %>% 
  ggplot(aes(modlvl, est)) + 
  geom_hline(yintercept = 0, linetype = "dotted")  +
  geom_linerange(aes(x = modlvl, ymin = cilo, ymax = cihi, color = mod_code2), 
                 color = "black") +
  geom_point(aes(fill = sigfill, size = sigsize), 
             pch = 21, color = "black") +
  #--n values
  geom_text(aes(x = modlvl, y =  est, label = paste0("n = ", n)),
            vjust = -0.5, hjust = -0.5, fontface = "italic", size = 3) +
  #--percents
  geom_text(aes(x = modlvl, y =  est, label = est_pct),
            vjust = -0.5, hjust = 1.5, fontface = "italic", size = 3) +
  facet_grid(mod_code2 ~resp, scales = "free") +
  
  labs(y = "Weed Log-Response-Ratio",
       x = NULL, 
       size = NULL) +
  guides(size = F, color = F, fill = F) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-2, 1, by = 1)) +
  scale_fill_manual(values = c("S" = wes[5], 
                               "NS" = "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(1.2)),
        strip.text.y = element_text(angle = 0),
        strip.text = element_text(face = "bold"),
        strip.background =element_rect(fill = "white"))

fig1a
ggsave("figs/manu_1a-mods.png", width = 20, height = 12, units = "cm")


# fig 1b biomass vs lrr ---------------------------------------------------


biosm <- read_csv("working_data/wd_bio-preds.csv") %>% 
  mutate(cc_type2 = case_when(
    cc_type2 == "grass" ~ "Grass",
    cc_type2 == "non-grass" ~ "Other"))

xlab <- expression(Cover~Crop~Biomass~(Mg~ha^-1))

fig1b <- 
  biosm %>% 
  ggplot(aes(cc_bm_Mgha, predLRR, color = cc_type2, linetype = cc_type2)) + 
  #--manually defining gridlines
  geom_vline(xintercept = 0, color = "gray90") +
  geom_vline(xintercept = 2.5, color = "gray90") +
  geom_vline(xintercept = 5, color = "gray90") +
  geom_vline(xintercept = 7.5, color = "gray90") +
  
  geom_point(aes(cc_bm_Mgha, LRR), color = "gray75", size = 2) + 
  geom_line(size = 2) +
  geom_point(x = 5.0, y = -1.38, color = "red", size = 5) + 
  geom_segment(x = 5.0, xend = 5.0, 
               y = -1.38, yend = -3.69,
               color = "black") + 
  geom_segment(x = -0.5, xend = 5.0, 
               y = -1.38, yend = -1.38,
               color = "black") + 
  geom_text(x = 2.5, y = -1.72, 
            label = "75% reduction\nin weed biomass",
            vjust = 1,
            # size = geom.text.size,
            color = "black",
            fontface = "italic") + 
  labs(x = xlab,
       y = "Log of Weed Biomass Response Ratio",
       title = " ",
       color = "Cover Crop Type",
       linetype = "Cover Crop Type") +
  scale_color_manual(values = c("Grass" = wes[4],
                                "Other" = "purple4")) +
  guides(color = F, linetype = F) +
  
  theme_bw() +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5)) + 
  theme(axis.text.x = element_text(colour = c("black",
                                              "black",
                                              "red",
                                              #"black",
                                              "black")),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(), 
        #axis.text = element_text(size = rel(1.2)),
        #axis.title = element_text(size = rel(1.2)),
        
        legend.position = c(0.99, 0.99),
        legend.justification = c(1,1),
        legend.background = element_rect(color = "black"))

fig1b

ggsave("figs/manu_1b-lrr-vs-bio.png", width = 20/2, height = 20/2, units = "cm")

# fig 1c win/win ----------------------------------------------------

ydat <- read_csv("working_data/wd_long.csv")  %>% 
  filter(!is.na(yieldLRR))

ydat %>% 
  mutate(yinc = ifelse(yieldLRR > 0, "Y", "N"),
         wdec = ifelse(LRR < 0 , "Y", "N")) %>% 
  group_by(yinc, wdec) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(tot = sum(n),
         pct = n/tot*100) 


fig1c <- 
  ydat %>% 
  filter(yieldLRR > -4) %>% 
  mutate(resp = recode(resp, 
                       bio = "Weed Biomass",
                       den = "Weed Density")) %>% 
  ggplot(aes(LRR, yieldLRR)) +
  geom_rect(xmin = -4.5, xmax = 0, ymin = 0, ymax = 3.4, fill = "gray90", alpha = 0.2) +
  geom_point(aes(shape = resp), size = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "Weed Log-Response-Ratio",
       y = "Cash Crop Yield Log-Response-Ratio",
       title = " ",
       shape = NULL) +
  guides(shape = F) +
  theme_bw() +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-3, 3)) + 
  # geom_text(x = -2.2, 
  #           y = 2, 
  #           label = "Cover crops\nincrease yield,\ndecrease weeds",
  #           #fontface = "italic"
  #           ) +
  theme(legend.position = c(0.5, 0.95),
        legend.background = element_rect(color = "black"), 
        legend.direction = "horizontal") +
  # geom_text(x = -3.7, y = -3, label = "n = 41") + #down yield, down weeds
  # geom_text(x = -3.7, y = 3, label = "n = 25") + #up yield, down weeds
  # geom_text(x = 3.7, y = -3, label = "n = 32") + #down yield, up weeds
  # geom_text(x = 3.7, y = 3, label = "n = 11") + #up yield, up weeds
  geom_text(x = -2, y = -2, label = "n = 38%", fontface = "italic") + #down yield, down weeds
  geom_text(x = -2, y = 2, label = "n = 23%", fontface = "italic") + #up yield, down weeds
  geom_text(x = 2, y = -2, label = "n = 29%", fontface = "italic") + #down yield, up weeds
  geom_text(x = 2, y = 2, label = "n = 10%", fontface = "italic") + #up yield, up weeds
  geom_rug(color = wes[5])


fig1c
ggsave("figs/manu_1c-win-win.png", width = 20/2, height = 20/2, units = "cm")



# patchwork isn't going to work....
# Fig 1 -------------------------------------------------------------------
#(fig1a) / (fig1c| fig1b )
#ggsave("figs/manu_fig1.png", width = 18.3, height = 18.3, units = "cm")

library(gridExtra)

jpeg("figs/pub/manu_fig1.jpg", quality = 80, units = "cm", height = 16, width = 16, res = 600) 
grid.arrange(fig1a, fig1b, fig1c,
             widths = c(1,1),
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3)))
dev.off() 

