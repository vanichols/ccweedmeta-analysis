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

wes <- wes_palette("Zissou1")
mypal4 <- c("royalblue", wes[c(2,3,5)])
mypal3 <- wes[c(1,4,5)]

# Read in data ------------------------------------------------------------
datraw <- read_csv("_tidydata/td_cc-database-clean-long.csv")


dat <- datraw %>%
  mutate(tmeth_nice = ifelse(ccterm_meth == "W", "Winterkill", NA),
         tmeth_nice = ifelse(ccterm_meth == "M", "Mechanical", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth == "H", "Herbicide", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth == "D", "Combo Mech/Herb", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth == "none", "None", tmeth_nice))



# Figure 1 mods + win/win ------------------------------------------------------



# mods --------------------------------------------------------------------

sigmods <- read_csv("_tidydata/sd_mods-weeds-contrasts.csv") %>% 
  filter(p.value < 0.05) %>%
  filter(mod_code != "crop_follow") %>% 
  select(mod_code) %>% 
  unique() %>% pull()

dat2b <- read_csv("_tidydata/sd_mods-weeds.csv")

cctype_ns <- 
  dat %>% 
  group_by(resp, cc_type2) %>% 
  summarise(n = n()) %>% 
  mutate(mod_code2 = "Cover Crop\nType") %>% 
  ungroup() %>% 
  mutate(modlvl = recode(cc_type2,
                         grass = "Grass",
                         `non-grass` = "Other"),
         resp = recode(resp,
                       bio = "Weed Biomass",
                       den = "Weed Density")) %>% 
  select(-cc_type2)

msmt_ns <- 
  dat %>% 
  group_by(resp, msmt_planting) %>% 
  summarise(n = n()) %>% 
  mutate(mod_code2 = "Measurement\nTiming") %>% 
  ungroup() %>% 
  mutate(modlvl = recode(msmt_planting,
                         "before" = "Before\nPlanting",
                         "after" = "After\nPlanting"),
         resp = recode(resp,
                       bio = "Weed Biomass",
                       den = "Weed Density")) %>% 
  select(-msmt_planting)

wg_ns <- 
  dat %>% 
  group_by(resp, weed_group) %>% 
  summarise(n = n()) %>%
  filter(!is.na(weed_group)) %>% 
  mutate(mod_code2 = "Weed Type") %>% 
  ungroup() %>% 
  mutate(modlvl = recode(weed_group,
                         "perennial" = "Perennial",
                         "winter annual" = "Winter\nAnnual",
                         "summer annual" = "Summer\nAnnual"),
         resp = recode(resp,
                       bio = "Weed Biomass",
                       den = "Weed Density")) %>% 
  select(-weed_group)

fig_ns <- bind_rows(cctype_ns, msmt_ns, wg_ns) %>% 
  mutate(myy = ifelse(resp == "Weed Biomass", -2, -1))


datfig <- 
  dat2b %>% 
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
      modlvl == "before" ~ "Before\nPlanting",
      modlvl == "after" ~ "After\nPlanting",
      modlvl == "summer annual" ~ "Summer\nAnnual",
      modlvl == "winter annual" ~ "Winter\nAnnual",
      modlvl == "non-grass" ~ "Other",
      TRUE ~ modlvl)) %>%
  mutate(modlvl = str_to_title(modlvl)) %>% 
  mutate(absest = abs(est))

fig_all <- fig_ns %>% 
  left_join(datfig) %>% 
  mutate(modlvl = factor(modlvl, levels = c("Other", "Grass", 
                                            "After\nPlanting","Before\nPlanting", 
                                            "Perennial", "Summer\nAnnual", "Winter\nAnnual")))  %>% 
  mutate(est_pct = (1 - exp(est)) * 100)


fig1a <- 
  fig_all %>%
  mutate(sigfill = ifelse(cihi > 0, "NS", "S"),
         sigsize = ifelse(cihi > 0, 1, 5),
         est_pct = ifelse(sigfill == "S", paste0("-", round(est_pct, 0), "%"), " ")) %>% 
  ggplot(aes(modlvl, est)) + 
  geom_linerange(aes(x = modlvl, ymin = cilo, ymax = cihi, color = mod_code2), 
                 color = "black") +
  geom_point(aes(fill = sigfill, size = sigsize), 
             pch = 21, color = "black") +
  geom_text(aes(x = modlvl, y =  est, label = paste0("n = ", n)),
            vjust = -0.5, hjust = -0.5, fontface = "italic", size = 3) +
  geom_text(aes(x = modlvl, y =  est, label = est_pct),
            vjust = -0.5, hjust = 1.5, fontface = "italic", size = 3) +
    facet_grid(mod_code2 ~resp, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  labs(y = "Weed\nLog-Response-Ratio",
       x = NULL, 
       size = NULL, 
       title = "(A)") +
  guides(size = F, color = F, fill = F) +
  coord_flip() +
  #scale_x_discrete(position = "left") +
  scale_y_continuous(breaks = seq(-2, 1, by = 1)) +
  #scale_color_manual(values = mypal3) +
  #scale_fill_manual(values = mypal3) +
  scale_fill_manual(values = c("S" = wes[5], 
                               "NS" = "black")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(1.2)),
        strip.text.y = element_text(angle = 0),
        strip.text = element_text(face = "bold"),
        strip.background =element_rect(fill = "white"))

fig1a
ggsave("_figs/manu-short_fig1A_modifiers.png", width = 7, height = 5)


# win/win ----------------------------------------------------

ydat <- read_csv("_tidydata/td_cc-database-clean-long.csv")  %>% 
  filter(!is.na(yieldLRR))

ydat %>% 
  mutate(yinc = ifelse(yieldLRR > 0, "Y", "N"),
         wdec = ifelse(LRR < 0 , "Y", "N")) %>% 
  group_by(yinc, wdec) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(tot = sum(n),
         pct = n/tot*100) 
  


fig1b <- 
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
  labs(x = "Weed\nLog-Response-Ratio",
       y = "Cash Crop Yield\nLog-Response-Ratio",
       shape = NULL,
       title = "(B)") +
  theme_bw() +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-3, 3)) + 
  geom_text(x = -2, 
            y = 2, 
            label = "Cover crops increase yield\nand decrease weeds",
            fontface = "italic") +
  theme(legend.position = c(0.5, 0.95),
        legend.background = element_rect(color = "black"), 
        legend.direction = "horizontal") +
  geom_text(x = -3.8, y = -3, label = "n = 41") + #down yield, down weeds
  geom_text(x = -3.8, y = 3, label = "n = 25") + #up yield, down weeds
  geom_text(x = 3.8, y = -3, label = "n = 32") + #down yield, up weeds
  geom_text(x = 3.8, y = 3, label = "n = 11") + #up yield, up weeds
  geom_rug(color = wes[5])

fig1b  
ggsave("_figs/manu-short_fig1B_win-win.png", width = 5.2, height = 5)


# Fig 1 -------------------------------------------------------------------
fig1a + fig1b
ggsave("_figs/manu-short_fig1_mods-win-win.png", width = 9.75, height = 5)


# Figure supp? - distribution of points ----------------------------------------------------------

dat2_lab <- dat %>%
  mutate(resp = recode(resp, 
                       bio = "Weed Biomass",
                       den = "Weed Density")) %>%
  group_by(resp) %>%
  summarise(n = n())


dat2_stats <- read_csv("_tidydata/td_stats-overall.csv") %>%
  mutate(resp = recode(resp, 
                       bio = "Weed Biomass",
                       den = "Weed Density"))
        
 

myxlab <- -5
myylab <- 20

dat %>% 
  mutate(resp = recode(resp, 
                       bio = "Weed Biomass",
                       den = "Weed Density"),
         exlab = ifelse(resp == "Weed Biomass", 
                        "Cover Crop\nDecreases Weeds",
                        NA)) %>%
  
  #--plot
  ggplot(aes(LRR)) + 

  #--bad region
  geom_rect(xmin = -6.5, xmax = 0,
            ymin = 0, ymax = 33,
            fill = "gray90") + 
  
  geom_histogram(bins = 25) +

  # explanation
  geom_text(x = -6, y = 30, 
            aes(label = exlab),
            fontface = "italic", color = "gray50", size = 4, hjust = 0) +
  
  #--mean
  geom_text(data = dat2_stats, 
            aes(x = myxlab, 
                y = myylab, 
                label = paste0("Mean = ", round(pavg,1), "%")), 
            vjust = 1.5, hjust = 0,
            size = 5,
            color = "gray50", fontface = "italic") + 
  
  #--n
  geom_text(data = dat2_lab, 
            aes(x = myxlab, 
                y = myylab - 1.5, 
                label = paste0("n = ", n)), 
            vjust = 1.5, hjust = 0,
            color = "gray50", fontface = "italic") + 
  
   #--p-val
  geom_text(data = dat2_stats, 
            aes(x = myxlab, 
                y = myylab - 2.5, 
                label = paste0("p = ", round(pval, 2))), 
            vjust = 1.5, hjust = 0,
            color = "gray50", fontface = "italic") + 
  
  
  geom_vline(xintercept = 0, linetype = "solid", size = 1) + 

  geom_vline(data = dat2_stats, 
             aes(xintercept = avg),
             linetype = "dashed", size = 1, color = "red") + 
  
  labs(x = "Log of (Cover Crop Treatment / No Cover Treatment )", y = NULL) +

  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 16)) + 
  facet_grid(~resp)

ggsave("_figs/manu_fig2_dist.png")



# Fig 3 biomass vs LRR ----------------------------------------------------



biosm <- read_csv("_tidydata/sd_bio-preds.csv")

xlab <- expression(Cover~Crop~Biomass~(Mg~ha^-1))
  
biosm %>% 
  mutate(cc_type2 = str_to_title(cc_type2)) %>% 
  ggplot(aes(cc_bio_Mgha, predLRR, color = cc_type2, linetype = cc_type2)) + 
  geom_vline(xintercept = 0, color = "gray90") +
  geom_vline(xintercept = 2.5, color = "gray90") +
  geom_vline(xintercept = 5, color = "gray90") +
  geom_vline(xintercept = 7.5, color = "gray90") +
  
  geom_point(aes(cc_bio_Mgha, LRR), color = "gray75", size = 2) + 
  geom_line(size = 2) +
  geom_point(x = 5.0, y = -1.38, color = "red", size = 5) + 
  geom_segment(x = 5.0, xend = 5.0, 
               y = -1.38, yend = -3.69,
               #linetype = "dashed"
               color = "black") + 
  geom_segment(x = -0.5, xend = 5.0, 
               y = -1.38, yend = -1.38,
               #linetype = "dashed"
               color = "black") + 
  geom_text(x = 1.5, y = -1.72, 
            label = "75% reduction\nin weed biomass",
            vjust = 1,
            size = 5,
            color = "black",
            fontface = "italic") + 
  labs(x = xlab,
       y = "Log of Weed Biomass\nResponse Ratio\n",
       color = "Cover Crop Type",
       linetype = "Cover Crop Type") +
  scale_color_manual(values = c("Grass" = wes[4],
                                "Non-Grass" = "purple4")) +
  
  theme_bw() +
  scale_x_continuous(breaks = c(0, 2.5, 5, 7.5)) + 
  theme(axis.text.x = element_text(colour = c("black",
                                              "black",
                                              "red",
                                              #"black",
                                              "black")),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(), 
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)),
        
        legend.position = c(0.99, 0.99),
        legend.justification = c(1,1),
        legend.background = element_rect(color = "black"))

ggsave("_figs/manu_figX-lrr-vs-ccbio.png", width = 8, height = 5)



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



