#############################
##
# Author: Gina, plus Katherine Goode's function
#
# Date Created: Sept 11 2019
#
# Date last modified: Sept 18 2019 (added code to do modifier bootstrapping)
#                     Dec 5 2019 (added KG's function
#
# Purpose: Create fucntion that fits linear models to get estimates of effect sizes
#                               shuffles groups and gets means (bootstrapping)
#                               gets 95% cis from distributions
#
# Inputs: none
#
# Outputs: some functions
#
# NOtes: 
#
##############################


library(lme4)
library(lmerTest)
library(emmeans)
library(broom)

# fit models --------------------------------------------------------------


RunModelNoModsFun <- function(mydata, resp) {
  
  ## For troubel shooting
  #mydata <- den
  #resp = "den"
  
  F.res <- lmer(LRR ~ 1 + (1|study), 
                data = mydata, 
                weights = wgt) 
  
  # Extract lmer results using contest from lmerTest package
  F.tidy <- contest(F.res, L = 1, joint = F, level = 0.95) 
  
  # Fix names
  names(F.tidy) <- c("avg", "se", "df", "t", "cilo", "cihi", "pval")
  
  F.tidy <- F.tidy %>%
    mutate(resp = resp,
           
           pavg = exp(avg) * 100 - 100,
           plo = exp(cilo) * 100 - 100,
           phi = exp(cihi) * 100 - 100
           
           ) %>%
    select(resp, pval, avg, cilo, cihi, pavg, plo, phi)
  
  return(F.tidy)
}


# Run model WITH mods -----------------------------------------------------


RunModelModsFun <- function(mydata, mymod, myresp) {
  
  
  ##### For trouble shooting, comment out when you actually run it######
  ##~~~~~~~~~~~~~~~~~
  #mydata <- den
  #mymod = "cc_type2"
  #myresp = "den"
  
  d.tmp <- 
    mydata %>% 
    select(resp, study, LRR, wgt, mymod) %>%
    rename_at(5, ~"mod") %>%         # change column name to generalizable 'mymod'
    mutate(mod= as.factor(mod)) 
  
  # Fit model to feed to emmeans
  F.res <- lmer(LRR ~ mod  + (1|study), 
                data = d.tmp, 
                weights = wgt) 
  
  # Use emmeans on lmer output
  F.lsm <- (emmeans(F.res, spec = "mod"))
  
  # Get CIs and pvals
  F.cilo <- confint(F.lsm, adjust = "none", level = 0.95)$lower.CL
  F.cihi <- confint(F.lsm, adjust = "none", level = 0.95)$upper.CL
  F.ps <- test(F.lsm, level = 0.95)$p.value
  
  # Combine emmeans' estimates, pvals, and 99% cis
  F.sum <- 
    tidy(F.lsm) %>%
    select(-conf.low, -conf.high) %>%
    mutate(p_val = F.ps,
           cilo = F.cilo,
           cihi = F.cihi, 
           resp = myresp,
           mod_code = mymod) %>%
    rename(est = estimate,
           se = std.error,
           desc = mod)
  
  return(F.sum)
  
  
}




# Compare levels of modifiers ---------------------------------------------


RunModelModsContrastFun <- function(mydata, mymod, myresp) {
  
  
  ##### For trouble shooting, comment out when you actually run it######
  #~~~~~~~~~~~~~~~~~
  #mydata <- den
  #mymod = "weed_group"
  #myresp = "den"
  
  d.tmp <- 
    mydata %>% 
    select(study, LRR, wgt, mymod) %>%
    rename_at(4, ~"mod") %>%         # change column name to generalizable 'mymod'
    mutate(mod = as.factor(mod)) 
  
  # Fit model to feed to emmeans
  F.res <- lmer(LRR ~ mod  + (1|study), 
                data = d.tmp, 
                weights = wgt) 
  
  # Use emmeans on lmer output
  F.lsm <- (emmeans(F.res, spec = "mod"))
  
  # Get comparisons of each level
  F.prs <- pairs(F.lsm)
  
  # Combine emmeans' estimates, pvals, and 99% cis
  F.sum <- 
    tidy(F.prs) %>%
    mutate(resp = myresp,
           mod_code = mymod) %>%
    rename(est = estimate,
           se = std.error)
  
  return(F.sum)
  
}


# Subsample and fit model, modifiers --------------------------------------


SubsamplelmerModsFun <- function(mydata, mymod, myresp) {
  
  
  # for testing
  #mydata = raw
  #myresp = "bio"
  #mymod = "soil_type"
  
  # NOTE: make sure mymod is in quotes, i.e. "sys_tillage", myresp = "bio"
  
  d.tmp <- mydata %>% 
    select(study, LRR, wgt, mymod) %>%
    rename_at(4, ~"mod") %>%         # change column name to generalizable 'mymod'
    filter(!is.na(mod)) %>%
    mutate(mod= as.factor(mod)) 
  
  # Find # of levels of mymod
  F.grps <- NULL
  F.lvls <- d.tmp %>% pull(mod) %>% unique()
  
  # Create subsets, resample w/in those
  for (i in 1:length(F.lvls)) {
    
    F.sub <- filter(d.tmp, mod == (F.lvls[i]))
    F.smp <- sample(1:nrow(F.sub), nrow(F.sub), replace = T)
    F.dat <- F.sub[F.smp,]
    assign(paste0("F.", i), F.dat)
    F.grps <- c(F.grps, paste0("F.", i))
  }
  
  # Combine subsets back together
  for (j in 1:length(F.lvls)) {
    
    if (j == 1) {
      F.datnew <- eval(as.name(paste0('F.', j)))
    } else {
      F.datnew <- rbind(F.datnew, eval(as.name(paste0("F.", j)))) }
  }
  
  # Run linear model on new fake dataset
  F.res <- lmer(LRR ~ mod - 1 + (1|study), data = F.datnew, weights = wgt)
  
  # Summarise results into tibble
  
  # mtidy = tidy model
  F.tidy <- as_tibble(coef(summary(F.res)))
  names(F.tidy) <- c("est", "se", "df", "t", "p_val")
  
  # mres = model results
  F.est <- bind_rows(fixef(F.res)) %>%
    gather(key = modlvl, value = est) %>%
    left_join(F.tidy, by = "est") %>%
    mutate(mod = mymod,
           resp = myresp) %>%
    select(resp, mod, modlvl, est)
  
  
  return(F.est)
}




#~~~~~~~~~~~~~~~~~
# Fit full model for Boot 
#~~~~~~~~~~~~~~~~~

BootFirstFullModsFun <- function(mydata, mymod, myresp){
  
  d_clim <- mydata %>% 
    select(study, LRR, wgt, mymod) %>%
    rename_at(4, ~"mod") %>%         # change column name to generalizable 'mymod'
    filter(!is.na(mod)) %>%          # remove 'IC' (it's complicated) values from analysis
    mutate(mod= as.factor(mod)) 
  
  
  m_clim <- lmer(LRR ~ mod-1 + (1|study), 
                 data = d_clim, 
                 weights = wgt) 
  
  
  mtidy_clim <- as.tibble(coef(summary(m_clim)))
  
  names(mtidy_clim) <- c("est", "se", "df", "t", "p_val")
  
  
  # mres = model results
  est_clim <- bind_rows(fixef(m_clim)) %>%
    gather(key = modlvl, value = est) %>%
    left_join(mtidy_clim, by = "est") %>%
    mutate(mod = mymod,
           resp = myresp) %>%
    select(resp, mod, modlvl, est)
  
  return(est_clim)
}


#~~~~~~~~~~~~~~~~~
# Do the subsampling 2500 times!, modifiers
#~~~~~~~~~~~~~~~~~


BootModsFun <- function(mydata, mymod, myresp){
  
  est_clim <- BootFirstFullModsFun(mydata, mymod, myresp)
  
  # Subsample adn fit model, 2500 times
  #~~~~~~~~~~~~~~~~~
  n = 2500
  for (i in c(1:n)) {
    
    F.est <- SubsamplelmerModsFun(mydata, mymod, myresp)
    est_clim <- bind_rows(est_clim, F.est)
    print(i)
  }
  
  return(est_clim)
  
}




# Extract CIs from distributions, 2 and 3 mods ----------------------------



## 2-level moderators
#
get95CI_2lvl_Fun <- function(myd, myci, mymod, myresp) {
  
  #Examples: myd is data, myci = 0.95, mymod = "sys_tillage", myresp = "bio"
  
  # Whole data manipulation
  #~~~~~~~~~~~~~~~~~
  myd <- 
    myd %>%
    filter(mod == mymod)
  
  lvls <- myd %>% pull(modlvl) %>% unique()
  
  # Level 1 of modifier
  #~~~~~~~~~~~~~~~~~
  myd1 <- myd %>% filter(modlvl == lvls[1]) %>% select(est)
  v1 <- nrow(myd1)  
  
  v1_low <- round( (1-myci)/2 * v1)
  v1_up <- v1 - round( (1-myci)/2 * v1)
  
  # Level 1 CI
  l1 <- myd1 %>% arrange(est) %>%
    mutate(rnk = 1:v1) %>% # Put in order
    filter(rnk > v1_low & rnk < v1_up)
  
  ci1 <- tibble(desc = lvls[1],
                bs_low = min(l1$est),
                bs_up = max(l1$est),
                mod = mymod)
  
  # Level 2 of modifier
  #~~~~~~~~~~~~~~~~~
  myd2 <- myd %>% filter(modlvl == lvls[2]) %>% select(est)
  v2 <- nrow(myd2)  
  
  v2_low <- round( (1-myci)/2 * v2)
  v2_up <- v2 - round( (1-myci)/2 * v2)
  
  # Level 1 CI
  l2 <- myd2 %>% arrange(est) %>%
    mutate(rnk = 1:v2) %>% # Put in order
    filter(rnk > v2_low & rnk < v2_up)
  
  ci2 <- tibble(desc = lvls[2],
                meanval = as.numeric(myd2[1,1]),    # First value is actual estimate
                bs_low = min(l2$est),
                bs_up = max(l2$est),
                mod = mymod)
  
  ci <- bind_rows(ci1, ci2) %>%
    mutate(resp = myresp)
  
  return(ci)
  
}




## 3-level moderators
#
get95CI_3lvl_Fun <- function(myd, myci, mymod, myresp) {
  
  #Examples: myd is data, myci = 0.95, mymod = "sys_tillage", myresp = "bio"
  
  # Whole data manipulation
  #~~~~~~~~~~~~~~~~~
  myd <- 
    myd %>%
    filter(mod == mymod)
  
  lvls <- myd %>% pull(modlvl) %>% unique()
  
  # Level 1 of modifier
  #~~~~~~~~~~~~~~~~~
  myd1 <- myd %>% filter(modlvl == lvls[1]) %>% select(est)
  v1 <- nrow(myd1)  
  
  v1_low <- round( (1-myci)/2 * v1)
  v1_up <- v1 - round( (1-myci)/2 * v1)
  
  # Level 1 CI
  l1 <- myd1 %>% arrange(est) %>%
    mutate(rnk = 1:v1) %>% # Put in order
    filter(rnk > v1_low & rnk < v1_up)
  
  ci1 <- tibble(desc = lvls[1],
                meanval = as.numeric(myd1[1,1]),    # First value is actual estimate
                bs_low = min(l1$est),
                bs_up = max(l1$est),
                mod = mymod)
  
  # Level 2 of modifier
  #~~~~~~~~~~~~~~~~~
  myd2 <- myd %>% filter(modlvl == lvls[2]) %>% select(est)
  v2 <- nrow(myd2)  
  
  v2_low <- round( (1-myci)/2 * v2)
  v2_up <- v2 - round( (1-myci)/2 * v2)
  
  # Level 1 CI
  l2 <- myd2 %>% arrange(est) %>%
    mutate(rnk = 1:v2) %>% # Put in order
    filter(rnk > v2_low & rnk < v2_up)
  
  ci2 <- tibble(desc = lvls[2],
                meanval = as.numeric(myd2[1,1]),    # First value is actual estimate
                bs_low = min(l2$est),
                bs_up = max(l2$est),
                mod = mymod)
  
  # Level 3 of modifier
  #~~~~~~~~~~~~~~~~~
  myd3 <- myd %>% filter(modlvl == lvls[3]) %>% select(est)
  v3 <- nrow(myd3)  
  
  v3_low <- round( (1-myci)/2 * v3)
  v3_up <- v3 - round( (1-myci)/2 * v3)
  
  # Level 1 CI
  l3 <- myd3 %>% arrange(est) %>%
    mutate(rnk = 1:v3) %>% # Put in order
    filter(rnk > v3_low & rnk < v3_up)
  
  ci3 <- tibble(desc = lvls[3],
                meanval = as.numeric(myd3[1,1]),    # First value is actual estimate
                bs_low = min(l3$est),
                bs_up = max(l3$est),
                mod = mymod)
  
  ci <- bind_rows(ci1, ci2, ci3) %>%
    mutate(resp = myresp)
  
  return(ci)
  
}




# Katherine Goode's function ----------------------------------------------


ComputeSE <- function(lrr, mymod){
  
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
