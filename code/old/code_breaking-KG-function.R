#############################
# Author: Gina, using Katherine Goode's function
#
# Date Created: Dec 5 2019
#
# Date last modified:
#
# Purpose: Show KG problem when fitting a model with a forced 0,0 intercept
#
# NOtes: 
#
##################################

library(readr)
library(dplyr)
library(lmer)
library(lmerTest)
library(ggResidpanel)


# Load KG's function ------------------------------------------------------

compute_se <- function(lrr, mymod){
  
  #NOTE: I added this part
  ####
  # Extract the variance-covariance matrix for beta0 and beta1
  betas <- as.vector(summary(mymod)$coefficients[,1])
  
  # Extract the variance-covariance matrix for beta0 and beta1
  vcov <- matrix(vcov(mymod), nrow = 2)
  #####
  
  
  # Separate the betas
  b0 <- betas[1] 
  b1 <- betas[2] 
  
  # Compute the inverse prediction of weed biomass
  pred_biomass <- (lrr - b0) / b1
  
  # Create an empty 1x2 matrix to store the elements of d in
  d <- matrix(NA, nrow = 1, ncol = 2)
  
  # Compute the elements of d (partial derivatives of g(beta))
  d[1] <- -1 / b1 
  d[2] <- (-lrr + b0) / (b1^2) 
  
  # Compute the standard error of annual survival (using the delta method)
  se <- sqrt(d %*% vcov %*% t(d))
  
  # Compute the lower and upper bounds of the 95% CI for annual survival
  lower <- pred_biomass - (1.96 * se) 
  upper <- pred_biomass + (1.96 * se)
  
  # Return the log response ratio, the predicted weed biomass given the 
  # resopnse ration, the delta method standard error, and the lower 
  # and upper bounds of the 95% CI for the predicted weed biomass
  return(data.frame(lrr = lrr,
                    pred_biomass = pred_biomass, 
                    se = se, 
                    ci_lower = lower, 
                    ci_upper = upper))
  
}



# load my data ------------------------------------------------------------

wd <- read_csv("_tidydata/td_weedbio-for-KG.csv") %>% 
  mutate(study = as.character(study))

wd %>% 
  ggplot(aes(ccbio_kgha)) + 
  geom_histogram()

wd %>% 
  ggplot(aes(ccbio_kgha, lrr)) + 
  geom_point() 


# fit models --------------------------------------------------------------

#--fit a model without a forced intercept
mymod <- lmer(lrr ~ ccbio_kgha + (1|study), weights = wgt, data = wd)

# I get a warning that the predictor variables are on very different scales. 
# But there's only one predictor variable, so...ignore it I guess

summary(mymod)
resid_panel(mymod)

##--use KG's function to see where 50% weed reduction happens
log(0.5)

compute_se(lrr = -0.693, mymod = mymod)
# well that's a big range. But fine. 

#--Can I force the intercept through 0? At 0 ccbio_kgha, the lrr should be 0 (no effect)
mymod2 <- lmer(lrr ~ 0 + ccbio_kgha + (1|study), weights = wgt, data = wd)
summary(mymod)
resid_panel(mymod)

# Now when I use the function, I break it :(
compute_se(lrr = -0.693, mymod = mymod2)


