#############################
##
# Author: Andrea Basche
#
# Date Created: Oct 25
#
# Date last modified: April 16 - by Andrea
#
# Purpose: Adding to analysis for more paper writing
#
# Inputs: td_cc-database-clean
#
# Outputs: stats and numbers
#
# NOtes: this is the updated and should be *final* version of the database
#
##############################

## for AB computers
setwd("C:/Users/abasche2/Dropbox/CC weed meta/_tidydata")


library(lme4)
library(ggplot2)
library(tidyverse)
library(lattice)
library(lmerTest)

ccweed <- read.csv("ccweedmeta_4.16.19.csv", na.strings='.', stringsAsFactors=FALSE)

ccweed$bioLRR <- log((ccweed$cc_wbio_gm2) / (ccweed$ctl_wbio_gm2))
ccweed$denLRR <- log((ccweed$cc_wden_numm2) / (ccweed$ctl_wden_numm2))


posbio <- subset(ccweed, bioLRR < 0) ##95/123 = 77%
totbio <- subset (ccweed, bioLRR < 3)
posden <- subset(ccweed, denLRR < 0) ##62/110 = 56%
totden <-subset (ccweed, denLRR <4)


##calc preliminary mean values and SEs
fit <- lmer(bioLRR ~ 1 + (1|study),
            data=ccweed
            #,weights = Wi add this back in for weights
)
summary(fit)
##mean -0.8619 , se 0.2891
##58% +/- 34%

fit <- lmer(denLRR ~ 1 + (1|study),
            data=ccweed
            #,weights = Wi add this back in for weights
)
summary(fit)
##mean 0.01793, se 0.30914
#-2% 36% SE

##groups

#term method - exclude Gieske and Hayden NA studies
ccweed2 <- ccweed[!ccweed$study ==9, ]
ccweed2 <- ccweed2[!ccweed2$study ==11, ]

fit <- lmer(bioLRR ~ cc_termMETH2 + (1|study),
            data=ccweed
            #,weights = Wi add this back in for weights
)
summary(fit)
anova(fit)

##time of measurement

fit <- lmer(denLRR ~ time + (1|study),
            data=ccweed
            #,weights = Wi add this back in for weights
)
summary(fit)
anova(fit)

##there is an almost sig dif in bioLRR AND very sig dif in denLRR
##biggest dif is the mechanical term - in particular on the weed density

fit <- lmer(denLRR ~ crop_follow + (1|study),
            data=dat2
            #,weights = Wi add this back in for weights
)
##following soy is more negative for bioLRR but non significant
##denLRR is almost identifical for the two groups

#cover crop type

fit <- lmer(bioLRR ~ cc_type+ (1|study),
            data=ccweed
            #,weights = Wi add this back in for weights
)
summary(fit)
anova(fit)

##overall distribution
ccweed1.o <- ccweed[order(ccweed$denLRR),]
ccweed2.o <- ccweed[order(ccweed$bioLRR),]

xyplot(1:nrow(ccweed1.o) ~ denLRR , data = ccweed1.o, 
       cex=1.5, #subset = BioPer< 1000,
       auto.key=TRUE,
       pch=20,
       xlab= list('Ln Change in Weed Density with Cover Crops', cex=1.5),
       ylab= list('Observation Rank', cex=1.5),
       scales = list(tck = c(-1,0), cex=1.2),
       #       key=list (text=list(c("Field Capacity", "Porosity")),x=0.65,y=0.15, pch=c(16,2),
       #                 points=TRUE, cex=1.2),
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(v=0)    
       }
)

xyplot(1:nrow(ccweed2.o) ~ bioLRR , data = ccweed2.o, 
       cex=1.5,
       auto.key=TRUE,
       pch=20,
       xlab= list('Ln Change in Weed Biomass with Cover Crops', cex=1.5),
       ylab= list('Observation Rank', cex=1.5),
       scales = list(tck = c(-1,0), cex=1.2),
       #       key=list (text=list(c("Field Capacity", "Porosity")),x=0.65,y=0.15, pch=c(16,2),
       #                 points=TRUE, cex=1.2),
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(v=0)    
       }
)


xyplot(1:nrow(ccweed1.o) ~ denLRR | cc_termMETH2 , data = ccweed1.o, 
       cex=1.5, #subset = BioPer< 1000,
       auto.key=TRUE,
       pch=20,
       xlab= list('Ln Change in Weed Density with Cover Crops', cex=1.5),
       ylab= list('Observation Rank', cex=1.5),
       scales = list(tck = c(-1,0), cex=1.2),
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(v=0)    
       }
)

xyplot(1:nrow(ccweed2.o) ~ bioLRR | cc_termMETH2, data = ccweed2.o, 
       cex=1.5,
       auto.key=TRUE,
       pch=20,
       xlab= list('Ln Change in Weed Biomass with Cover Crops', cex=1.5),
       ylab= list('Observation Rank', cex=1.5),
       scales = list(tck = c(-1,0), cex=1.2),
       #       key=list (text=list(c("Field Capacity", "Porosity")),x=0.65,y=0.15, pch=c(16,2),
       #                 points=TRUE, cex=1.2),
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(v=0)    
       }
)

xyplot(1:nrow(ccweed1.o) ~ denLRR | time , data = ccweed1.o, 
       cex=1.5, #subset = BioPer< 1000,
       auto.key=TRUE,
       pch=20,
       xlab= list('Ln Change in Weed Density with Cover Crops', cex=1.5),
       ylab= list('Observation Rank', cex=1.5),
       scales = list(tck = c(-1,0), cex=1.2),
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(v=0)    
       }
)


xyplot(1:nrow(ccweed2.o) ~ bioLRR | time, data = ccweed2.o, 
       cex=1.5,
       auto.key=TRUE,
       pch=20,
       xlab= list('Ln Change in Weed Biomass with Cover Crops', cex=1.5),
       ylab= list('Observation Rank', cex=1.5),
       scales = list(tck = c(-1,0), cex=1.2),
       #       key=list (text=list(c("Field Capacity", "Porosity")),x=0.65,y=0.15, pch=c(16,2),
       #                 points=TRUE, cex=1.2),
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(v=0)    
       }
)


ccweed2 <- ccweed[!ccweed$cc_type =="mix", ]
ccweed2 <- ccweed2[!ccweed2$cc_type =="brassica", ]
ccweed2 <- ccweed2[!ccweed2$cc_termMETH2 =="NA", ]

ccweed3 <- ccweed[!ccweed$crop_follow =="NA", ]
ccweed3 <- ccweed[!ccweed$crop_follow =="corn/soy", ]

ggplot(data = ccweed, aes(x = yieldLRR, y = cc_type, color=crop_follow)) + 
  ylab(" ") + 
  xlab("LRR yield") + 
 # geom_point(color = c("orange", "purple")) +
  geom_vline(xintercept=0, lwd=1) +
  theme_bw() +
  theme(legend.position="left") + 
  geom_point(size=3.5)

ggplot(data = ccweed2, aes(x = yieldLRR, y = cc_termMETH2, color=cc_type)) + 
  ylab(" ") + 
  xlab("LRR weed density") + 
  geom_vline(xintercept=0, lwd=1) +
  theme_bw() +
  theme(legend.position="left") + 
  geom_point(size=3.5)

##biomass plots - this is not working right now
##trying with a subset outside of the graph - something is funny with the numbers in this column
##coercing into numeric is not working


ccbio2 <- ccweed[!ccweed$pub_reference =="Fisk et al., 2001", ]
ccbio <- ccbio2[!ccbio2$study ==10, ]
##this is the legume study that had super low biomass
##and the study that is categorized as corn or soy following


xyplot (bioLRR ~ cc_bio_kgha|crop_follow, dat=ccbio, 
        type=c("p","r"), 
        ylab="Log RR Change in Weed Biomass",
        xlab="Cover Crop Biomass kg/ha",
 #       groups=cc_type,
        auto.key=TRUE,
        pch=20,
        cex=1.3,
        panel = function(x,y,...){
          panel.xyplot(x,y, ...)
          panel.abline(h=-0.69)    
        })

##using Gina's code


ccweed5 %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, pch = 21, fill = "green4") + 
  geom_smooth(method = "lm", se = F, color = "red", size = 4) +
  #geom_vline(xintercept = 0) + 
  
  labs(x = "Cover Crop Biomass [kg/ha]", y = "Weed Response to Cover Crop") +
  theme_classic() + 
  facet_grid(.~resp) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

ccweed5 %>% 
  gather(bioLRR, key = "resp", value = "LRR") %>%
  
  ggplot(aes(cc_bio_kgha, LRR)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 5, pch = 21, fill = "green4") + 
  geom_smooth(method = "lm", se = F, color = "red", size = 4) +
  #geom_vline(xintercept = 0) + 
  
  labs(x = "Cover Crop Biomass [kg/ha]", y = "Weed Response to Cover Crop") +
  theme_classic() + 
  facet_grid(.~resp) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

##win win plot

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

##trying a slightly simpler code

dat2 <- dat[!dat$crop_follow =="NA", ]
dat2 <- dat[!dat$crop_follow =="corn/soy", ]

ggplot(aes(yieldLRR, LRR)) + 
geom_point(aes(color = ww_color), size = 5) + 
  
  dat2 %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  filter(!is.na(LRR)) %>%
  ggplot(data = dat2, aes(x = yieldLRR, y = LRR, color=crop_follow) ) + 
  labs(x = "Yield Response to Cover Crop", y = "Weed Response to Cover Crop") +
  scale_color_manual(values = c("green", "yellow")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  geom_vline(xintercept=0, lwd=1) +
  theme_bw() +
  theme(legend.position="left") + 
  geom_point(size=3.5)


dat2 %>% 
  gather(bioLRR:denLRR, key = "resp", value = "LRR") %>%
  filter(!is.na(LRR)) %>%

  xyplot (bioLRR ~ yieldLRR, dat=dat2, 
        type=c("p"), 
 #      ylab="Log RR Change in Weed Biomass",
#       xlab="Cover Crop Biomass kg/ha",
         groups=crop_follow,
        auto.key=TRUE,
        pch=20,
        cex=1.3,
        panel = function(x,y,...){
          panel.xyplot(x,y, ...)
          panel.abline(h=0)   
          panel.abline(v=0)  
        })

xyplot (denLRR ~ yieldLRR, dat=ccweed, 
        type=c("p"), 
        #      ylab="Log RR Change in Weed Biomass",
        #       xlab="Cover Crop Biomass kg/ha",
        groups=cc_termMETH2,
        auto.key=TRUE,
        pch=20,
        cex=1.3,
        panel = function(x,y,...){
          panel.xyplot(x,y, ...)
          panel.abline(h=0)   
          panel.abline(v=0)  
        })

##win-win without DeBruin

ccweed2 <- ccweed[!ccweed$study ==3, ]
##40 RRs in DeBruin
debruin <- subset(ccweed, study==3)
werle <- subset(ccweed, study==13)


xyplot (denLRR ~ yieldLRR, dat=ccweed2, 
        type=c("p"), 
        #      ylab="Log RR Change in Weed Biomass",
        #       xlab="Cover Crop Biomass kg/ha",
        groups=crop_follow,
        auto.key=TRUE,
        pch=20,
        cex=1.3,
        panel = function(x,y,...){
          panel.xyplot(x,y, ...)
          panel.abline(h=0)   
          panel.abline(v=0)  
        })

##organic only analysis
##only want to include studies 2, 6, 7 & 9

ccweed <- ccweed[!ccweed$study ==1, ]
ccweed <- ccweed[!ccweed$study ==3, ]
ccweed <- ccweed[!ccweed$study ==4, ]
ccweed <- ccweed[!ccweed$study ==5, ]
ccweed <- ccweed[!ccweed$study ==8, ]
ccweed <- ccweed[!ccweed$study ==10, ]
ccweed <- ccweed[!ccweed$study ==11, ]
ccweed <- ccweed[!ccweed$study ==12, ]
ccweed <- ccweed[!ccweed$study ==13, ]
ccweed <- ccweed[!ccweed$study ==14, ]
ccweed <- ccweed[!ccweed$study ==15, ]
write.csv(ccweed,'organic.csv')



