#########################
#
# Date of creation: June 7 2019
# Date last modified: June 7 2019
#                     Sept 17 (re-reunning w/new dataset)
#
# Author: Gina
# Purpose: Explore using decision trees on weed-rotation data
#          
# Inputs: td_cc-database-clean
#
# Outputs: 
# NOTE: Just use density data, bc that's the only one w/hetergeneity
# 
#########################

rm(list = ls())
library(tidyverse)
library(rpart) # Decision tree package
library(partykit)
library(tree)
library(randomForest)
#library(gbm)
#library(caret)

raw0 <- read_csv("_tidydata/td_cc-database-clean-long.csv")


# need to think about how to evaluate this. What makes it more likely to fall into a WW category?
ydat <- raw0 %>% 
  mutate(yinc = ifelse(yieldLRR > 0, "Y", "N"),
         wdec = ifelse(LRR < 0 , "Y", "N"),
         ww = ifelse(yinc == "Y" & wdec == "Y", "Y", "N")) %>%
  select(ww, 
         aridity_index, 
         cc_type2, cropsys_tillage,  
         #msmt_planting, 
         weed_group,
         #ccpl_den_kgha, #--this is going to mean different things for different crops, eliminate it
         ccterm_meth,
         crop_follow, 
         cc_bio_kgha,
         #cc_growdays, #--not even sure what this MEANS
         #termgap_days #--100 of the points didn't even report a date. 
  ) %>%
  mutate_if(is.character, as.factor) #%>%
# filter(termgap_days > -6)   %>%   #--Hoffman let the vetch grow for a month after planting. got terrible weed control. duh. 
#filter(crop_follow != "corn/soy",  #--remove points that were averaged over crops
#      ccterm_meth != "D")        #--only 3 of these, get rid of them



ydat <- na.omit(ydat)

tyld <- ctree(ww ~ ., 
              data = ydat)
plot(tyld)

# so grasses and >4500 kg biomass put you in the Y more often?

f_tree <- tree(ww~., ydat)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_tree <- prune.tree(f_tree, best = 3)
plot(prune_tree)
text(prune_tree, pretty = 0)


# let's look at this data -------------------------------------------------

#--we lose 4 points by eliminating fall
raw <- raw0 %>%
  filter(msmt_season != "fall")


#--I acutally think msmt season is stupid. It should just be in regard to planting. Eliminate it. 
raw %>%
  ggplot(aes(weed_group)) + 
  geom_histogram(stat = "count") + 
  facet_grid(msmt_season ~ 
               msmt_planting)

# Keep only variables of interest -----------------------------------------

dat <- raw %>%
  select(resp, LRR, 
         #aridity_index, 
         cc_type2, cropsys_tillage,  
         #msmt_planting, 
         weed_group,
         #ccpl_den_kgha, #--this is going to mean different things for different crops, eliminate it
         ccterm_meth,
         crop_follow, 
         cc_bio_kgha,
         #cc_growdays, #--not even sure what this MEANS
         termgap_days #--100 of the points didn't even report a date. 
         ) %>%
  mutate_if(is.character, as.factor) #%>%
 # filter(termgap_days > -6)   %>%   #--Hoffman let the vetch grow for a month after planting. got terrible weed control. duh. 
  #filter(crop_follow != "corn/soy",  #--remove points that were averaged over crops
   #      ccterm_meth != "D")        #--only 3 of these, get rid of them

bio <- dat %>%
  filter(resp == "bio") %>%
  filter(!is.na(LRR)) %>%
  select(-resp)



bio2 <- na.omit(bio)
         
         
den <- dat %>%
  filter(resp == "den") %>%
  filter(!is.na(LRR)) %>%
  select(-resp)

den2 <- na.omit(den)


# Try party package -------------------------------------------------------

# Regression tree

#--density
tden <- ctree(LRR ~ ., 
              data = den2)
plot(tden)

#--biomass
tbio <- ctree(LRR ~ ., 
              data = bio2)
plot(tbio) #--says msmt with regard to planting is most important


# #--yield
# tyld <- ctree(yieldLRR ~ ., 
#               data = yld)
# plot(tyld)


# try tree package --------------------------------------------------------

#--bio
f_tree <- tree(LRR~., bio2)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_tree <- prune.tree(f_tree, best = 3)
plot(prune_tree)
text(prune_tree, pretty = 0)

#--den
f_tree <- tree(LRR~., den2)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b')

prune_tree <- prune.tree(f_tree, best = 3)
plot(prune_tree)
text(prune_tree, pretty = 0)


#--hmm. gap days - do we trust these values? how many didn't report it and we had to guess?

bio %>%
  ggplot(aes(termgap_days, LRR)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)



bio2 %>%
  ggplot(aes(termgap_days, LRR)) + 
  geom_point()

raw0 %>%
  filter(termgap_days <0) %>% select(study, pub_reference, termgap_days)

#--weed group
bio %>%
  ggplot(aes(weed_group, LRR)) + 
  #geom_point() + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_hline(yintercept = 0) +
  coord_flip()

#--msmt_planting
bio %>%
  ggplot(aes(msmt_planting, LRR)) + 
  #geom_point() + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_hline(yintercept = 0) +
  coord_flip()


#--msmt_planting
den %>%
  ggplot(aes(msmt_planting, LRR)) + 
  #geom_point() + 
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  geom_hline(yintercept = 0) +
  coord_flip()


# try randomforest package --------------------------------------------------------

bag_den <- randomForest(LRR~., data = den2, mtry = 4,  
                        importance = T, 
                        na.action = na.omit)
importance(bag_den)
varImpPlot(bag_den)


#--biomass has 120
bag_bio <- randomForest(LRR~., data = bio2, mtry = 4,  
                        importance = T, 
                        na.action = na.omit)
importance(bag_bio)
varImpPlot(bag_bio)

# Partial dependence plots?
Y = bio2$LRR

X = bio2 %>% 
  select(
  cropsys_tillage:termgap_days)

pdmod <- randomForest(LRR ~., bio2)
#partialPlot(pdmod, as.data.frame(wow), "PISD_diff")
#partialPlot(pdmod, as.data.frame(wow), "species_rat")
partialPlot(pdmod, as.data.frame(bio2), "cc_bio_kgha")
partialPlot(pdmod, as.data.frame(bio2), "termgap_days")



# Think about this --------------------------------------------------------

bio %>% 
  select(msmt_season, msmt_planting, weed_group) %>%
  table()



# Make predictions on the other half of the dataset
wow.pred <- predict(bag_den, wow.test) #--note that pred is NA if we have an NA. 
wow.test$predict <- wow.pred

#Calculate our own RMSE because we don't trust the black box
wow.test$MSE <- (wow.test$den - wow.test$predict)^2
sqrt(mean(wow.test$MSE, na.rm = T))

wow.test %>%
  arrange(den) %>%
  mutate(rank = 1:n()) %>%
  ggplot() + 
  geom_point(aes(rank, den), color = "red") + 
  geom_point(aes(rank, predict), color = "blue")


# Partial dependence plots?
Y = wow$den
X = wow[,c(1:9, 11:12)]

pdmod <- randomForest(den ~., wow)
#partialPlot(pdmod, as.data.frame(wow), "PISD_diff")
#partialPlot(pdmod, as.data.frame(wow), "species_rat")
partialPlot(pdmod, as.data.frame(wow), "maxstudyage")


par(mfrow=c(6,4),mar=c(4,2.2,0,0))

for(i in 1: ncol(X)){pplot<-partialPlot(pdmod,
                                        as.data.frame(wow),
                                        x.var = names(X)[i],xlab=names(X)[i],main="")}
par(mfrow=c(1,1))

importance(pdmod)




#yhat.rf = predict(bag_den, newdata = p_den[-train,])
#mean((yhat.rf - p_den[-train, "LRR"])^2, na.rm = T)

rf_den <- randomForest(LRR~., data = p_den, mtry = 3, subset = train, importance = T, 
                       na.action = na.omit)
importance(rf_den)
varImpPlot(rf_den)

yhat.rf = predict(rf_den, newdata = p_den[-train,])
mean((yhat.rf - p_den[-train, "LRR"])^2, na.rm = T)



# Run a boosted tree using gbm package (page 217 of Applied Pred M --------

# Use a gaussian distribution because our response variable is continuous

bio2 %>%
  group_by(weed_group) %>%
  summarise(n = n())

boost_den <- gbm(LRR~., data = bio2, distribution = "gaussian",
                 n.trees = 1000, interaction.depth = 5)
summary(boost_den)
pretty.gbm.tree(boost_den)

# Let's tune interaction.depth, n.trees, and shrinkages

gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 500, by = 50),
                       shrinkage = c(0.001, 0.01, 0.1),
                       n.minobsinnode = 10)

set.seed(951983)


# Train takes matrices, it doesn't like the formula interface

# wowY <- wow$den
# wowX = wow[,1:8]
# 
# gbmTune <- train(den ~., 
#                  data = wow,
#                  method = "gbm",
#                  tuneGrid = gbmGrid,
#                  verbose = FALSE)
# 
# 
# plot(gbmTune)
