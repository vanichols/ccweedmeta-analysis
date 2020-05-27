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


# fig things --------------------------------------------------------------

wes <- wes_palette("Zissou1")
wes2 <- wes_palette("Rushmore1")
wes3a <- wes_palette("Moonrise3")
wes3 <- wes_palette("Moonrise2")

cctrtpal <- c(wes3[4], wes3a[3])

mypal4 <- c("royalblue", (wes[c(2,3,5)]))

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

dat <- read_csv("_tidydata/td_cc-database-clean-long.csv")
dat1 <- 
  dat %>%
  group_by(resp, lat, long, cropsys_tillage) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(resp = recode(resp,
                       bio = "Weed Biomass Study",
                      den = "Weed Density Study"))


#--map without points
ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), 
               colour = "black", fill = wes[1], size = 1.2) +
  # geom_jitter(data = dat1, width = 0.5, size = 4,
  #             aes(x = long, y = lat, 
  #                 color = resp)) + 
  labs(x = NULL, 
       y = NULL, 
       color = NULL) + 
  scale_color_manual(values = c(wes[5], wes[3])) +
  coord_quickmap() + 
  theme_minimal()  +
  mytheme + 
  theme(legend.position = "top",
        axis.text = element_blank())

ggsave("_figs/talk_map-study-sites1.png")

#--map with points
ggplot() +
  geom_polygon(data = map_crnblt, aes(x = long, y = lat, group = group), 
               colour = "black", fill = wes[1], size = 1.2) +
  geom_jitter(data = dat1, width = 0.5, size = 4,
              aes(x = long, y = lat, 
                  color = resp)) + 
  labs(x = NULL, 
       y = NULL, 
       color = NULL) + 
  scale_color_manual(values = c(wes[5], wes[3])) +
  coord_quickmap() + 
  theme_minimal()  +
  mytheme + 
  theme(legend.position = "top",
        axis.text = element_blank())

ggsave("_figs/talk_map-study-sites2.png")



# Modifiers ------------------------------------------------------

datraw <- read_csv("_tidydata/td_cc-database-clean-long.csv")

dat <- datraw %>%
  mutate(tmeth_nice = ifelse(ccterm_meth == "W", "Winterkill", NA),
         tmeth_nice = ifelse(ccterm_meth == "M", "Mechanical", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth == "H", "Herbicide", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth == "D", "Combo Mech/Herb", tmeth_nice),
         tmeth_nice = ifelse(ccterm_meth == "none", "None", tmeth_nice))


sigmods <- read_csv("_tidydata/sd_mods-weeds-contrasts.csv") %>% 
  filter(p.value < 0.05) %>%
  filter(mod_code != "crop_follow") %>% 
  select(mod_code) %>% 
  unique() %>% pull()

dat2b <- read_csv("_tidydata/sd_mods-weeds.csv")

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
 

mypal3 <- c("royalblue", "green4", (wes[c(5)]))


# biomass mods ------------------------------------------------------------

datfig %>% 
  filter(resp == "Weed Biomass") %>% 
  ggplot(aes(modlvl, est)) + 
  geom_rect(xmin = 0, xmax = 4, ymin = 0, ymax = 1.5, 
            fill = "gray90", alpha = 0.5) +
  geom_rect(xmin = 0, xmax = 4, ymin = -2.5, ymax = 0, 
            fill = "darkolivegreen1", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = -1.39, color = "white") +
  geom_hline(yintercept = -0.69, color = "white") +
  geom_hline(yintercept = 0.69, color = "white") +
  geom_linerange(aes(x = modlvl, ymin = cilo, ymax = cihi), color = "black") +
  geom_point(aes(size = absest, 
                 #fill = mod_code2,
                 ), 
             fill = "black", pch = 21) +
  labs(y = NULL,
       x = NULL, 
       size = NULL) +
  guides(size = F, color = F, fill = F) +
  coord_flip() +
  scale_x_discrete(position = "left") +
  scale_y_continuous(breaks = c(-1.39, -0.69, 0, 0.69),
                     labels = c("-75%", "-50%", " ", "+50%")) +
  #scale_color_manual(values = mypal3) +
  #scale_fill_manual(values = mypal3) +
  facet_grid(mod_code2 ~resp, 
             scales = "free", 
             switch = "y"
             ) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.4)),
        axis.title.y = element_text(size = rel(1.4)),
        strip.text.y = element_text(angle = 180),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        strip.background =element_rect(fill = "white"),
        #panel.background = element_rect(fill = "darkolivegreen1"),
        panel.grid = element_blank())

ggsave("_figs/talk_modifiers-biomass.png", width = 6)

datfig %>% 
  filter(resp == "Weed Density") %>% 
  ggplot(aes(modlvl, est)) + 
  geom_rect(xmin = 0, xmax = 4, ymin = 0, ymax = 1.5, fill = "gray90", alpha = 0.5) +
  geom_rect(xmin = 0, xmax = 4, ymin = -2, ymax = 0, fill = "darkolivegreen1", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(yintercept = -1.39, color = "white") +
  geom_hline(yintercept = -0.69, color = "white") +
  geom_hline(yintercept = 0.69, color = "white") +
  geom_linerange(aes(x = modlvl, ymin = cilo, ymax = cihi), color = "black") +
  geom_point(aes(size = absest, 
                 #fill = mod_code2,
  ), 
  fill = "black", pch = 21) +
  labs(y = NULL,
       x = NULL, 
       size = NULL) +
  guides(size = F, color = F, fill = F) +
  coord_flip() +
  scale_x_discrete(position = "left") +
  scale_y_continuous(breaks = c(-1.39, -0.69, 0, 0.69),
                     labels = c("-75%", "-50%", " ", "+50%")) +
  #scale_color_manual(values = mypal3) +
  #scale_fill_manual(values = mypal3) +
  facet_grid(mod_code2 ~resp, 
             scales = "free", 
             switch = "y"
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.4)),
        axis.title.y = element_text(size = rel(1.4)),
        strip.text.y = element_text(angle = 180),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        strip.background =element_rect(fill = "white"),
        #panel.background = element_rect(fill = "darkolivegreen1"),
        panel.grid = element_blank())


ggsave("_figs/talk_modifiers-density.png", width = 6)


# all mods ------------------------------------------------------------

datfig %>% 
  mutate(estlab = paste0(round(-(100-exp(est)*100), 0), " %"),
         estcolor = ifelse(cihi<0, "darkolivegreen1", "gray90")) %>% 
  #filter(resp == "Weed Biomass") %>% 
  ggplot(aes(modlvl, est)) + 
  #geom_rect(xmin = 0, xmax = 4, ymin = 0, ymax = 1.5, 
  #          fill = "gray90", alpha = 0.5) +
  #geom_rect(xmin = 0, xmax = 4, ymin = -2.5, ymax = 0, 
  #          fill = "darkolivegreen1", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_hline(yintercept = -1.39, color = "gray90") +
  geom_hline(yintercept = -0.69, color = "gray90") +
  geom_hline(yintercept = 0.69, color = "gray90") +
  #geom_linerange(aes(x = modlvl, ymin = cilo, ymax = cihi), color = "gray50") +
  geom_col(aes(size = absest, 
                 fill = estcolor)) +
  geom_text(aes(modlvl, est *1.1, label = estlab), 
            fontface = "italic", color = "black", size = 5) +
  labs(y = NULL,
       x = NULL, 
       size = NULL) +
  guides(size = F, color = F, fill = F) +
  coord_flip() +
  scale_x_discrete(position = "left") +
  #scale_y_continuous(limits = c(-1.6, 0.5)) +
  scale_y_continuous(limits = c(-1.6, 0.69),
                     breaks = c(-1.6, -1.39, -0.69, 0, 0.69),
                     labels = c(" ", "-75%", "-50%", " ", "+50%")) +
  #scale_color_manual(values = mypal3) +
  scale_fill_manual(values =c(wes[3], 
                              #wes[5]
                              "gray50")) +
  facet_grid(mod_code2 ~resp, 
             scales = "free", 
             switch = "y"
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.4)),
        #axis.title.y = element_text(size = rel(1.4)),
        axis.text.x = element_blank(),
        strip.text.y = element_text(angle = 180),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        strip.background =element_rect(fill = "white"),
        #panel.background = element_rect(fill = "darkolivegreen1"),
        panel.grid = element_blank())

ggsave("_figs/talk_modifiers-all-v2.png", width = 10)



# select mods (poster) ------------------------------------------------------------

datfig %>% 
  filter(mod_code %in% c("cc_type2", "weed_group")) %>% 
  mutate(estlab = paste0(round(-(100-exp(est)*100), 0), " %"),
         estcolor = ifelse(cihi<0, "sig", "ns")) %>%
  mutate(estlab1 = ifelse(est>0, paste0("+", estlab), estlab)) %>% 
  ggplot(aes(modlvl, est)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_hline(yintercept = -1.39, color = "gray90") +
  geom_hline(yintercept = -0.69, color = "gray90") +
  geom_hline(yintercept = 0.69, color = "gray90") +
  geom_col(aes(size = absest, 
               fill = estcolor)) +
  geom_text(aes(modlvl, est *1.1, label = estlab1), 
            fontface = "italic", color = "black", size = 5) +
  labs(#y = "Change relative to no cover treatment",
       y = NULL,
       x = NULL, 
       size = NULL) +
  guides(size = F, color = F, fill = F) +
  coord_flip() +
  scale_x_discrete(position = "left") +
  #scale_y_continuous(limits = c(-1.6, 0.5)) +
  scale_y_continuous(limits = c(-1.6, 0.69),
                     breaks = c(-1.6, -1.39, -0.69, 0, 0.69),
                     labels = c(" ", "-75%", "-50%", " ", "+50%")) +
  #scale_color_manual(values = mypal3) +
  scale_fill_manual(values =c("sig" = wes[3], "ns" = "gray50")) +
  facet_grid(mod_code2 ~resp, 
             scales = "free", 
             switch = "y"
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.4), color = "white"),
        #axis.title.y = element_text(size = rel(1.4)),
        axis.text.x = element_blank(),
        strip.text.y = element_text(angle = 180),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        plot.background = element_rect(fill = "transparent", colour = NA),
        strip.background =element_rect(fill = "gray90"),
        #panel.background = element_rect(fill = "darkolivegreen1"),
        panel.grid = element_blank())

ggsave("_figs/talk_modifiers-cc-wdtyp.png", width = 10, bg = "transparent")






# Fig 4 yield vs weeds ----------------------------------------------------

library(ggExtra)
ydat <- read_csv("_tidydata/td_cc-database-clean-long.csv")  %>% 
  filter(!is.na(yieldLRR))

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
  labs(x = "Weeds\nLog-Response-Ratio",
       y = "Cash Crop Yield\nLog-Response-Ratio",
       shape = NULL) +
  theme_bw() +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-3, 3)) + 
  geom_text(x = -2, 
            y = 2, 
            label = "Cover crops increase yield\nand decrease weeds",
            fontface = "italic") +
  theme(legend.position = "top",
        legend.background = element_rect(color = "black")) +
  geom_text(x = -3.8, y = -3, label = "n = 41") + #down yield, down weeds
  geom_text(x = -3.8, y = 3, label = "n = 25") + #up yield, down weeds
  geom_text(x = 3.8, y = -3, label = "n = 32") + #down yield, up weeds
  geom_text(x = 3.8, y = 3, label = "n = 11") +
  geom_rug(color = "red")

ggsave("_figs/talk_yield-weeds-WW.png", width = 5.2, height = 5)


# biomass vs LRR ----------------------------------------------------

biosm <- read_csv("_tidydata/sd_bio-preds.csv")

xlab <- expression(Cover~Crop~Biomass~(Mg~ha^-1))
  
biosm %>% 
  mutate(cc_type2 = str_to_title(cc_type2)) %>% 
  ggplot() + 
  geom_vline(xintercept = 0, color = "gray90") +
  geom_vline(xintercept = 2.5, color = "gray90") +
  geom_vline(xintercept = 5, color = "gray90") +
  geom_vline(xintercept = 7.5, color = "gray90") +
  
  geom_point(aes(cc_bio_Mgha, LRR), color = "gray75", size = 2) + 
  geom_line(aes(cc_bio_Mgha, predLRR, color = cc_type2), size = 2) +
  geom_point(x = 5.0, y = -1.38, color = "red", size = 5) + 
  geom_segment(x = 5.0, xend = 5.0, 
               y = -1.38, yend = -3.69,
               color = "red", linetype = "dashed") + 
  geom_segment(x = -0.5, xend = 5.0, 
               y = -1.38, yend = -1.38,
               color = "red", linetype = "dashed") + 
  geom_text(x = 1, y = -1.72, 
            label = "75% reduction\nin weed biomass",
            vjust = 1,
            size = 4,
            color = "black",
            fontface = "italic") + 
  labs(x = xlab,
       y = "Log of Weed Biomass\nResponse Ratio\n",
       color = "Cover Crop Type") +
  scale_color_manual(values = c("Grass" = "green4",
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
        axis.text = element_text(size = rel(1.1)),
        axis.title = element_text(size = rel(1.1)),
        
        legend.position = c(0.9, 0.9),
        legend.background = element_rect(color = "black"))

ggsave("_figs/talk_lrr-vs-ccbio.png", width = 6, height = 4)
