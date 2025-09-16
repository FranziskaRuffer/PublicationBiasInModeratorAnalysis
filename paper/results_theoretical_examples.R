# R code to reproduce the Figures and Calculations of all theoretical examples in the paper. 

#load packages
library(ggplot2)

################################################################################
##############-------------- Modelling choices ---------------##################
################################################################################

# primary study sample sizes 
N_small <- 40
N_medium <- 80
N_large <- 200
N_vec <- c(N_small, N_medium, N_large)

# publication bias (% of non-significant studies published)
PB_extreme <- 0
PB_high <- 0.05
PB_medium <- 0.2
PB_low <- 0.5
PB_no <- 1
PB_vec <- c(0, 0.05, 0.2, 0.5, 1)

# critical value based on a two-sided test, with focus on upper tail
Zcv <- qnorm(0.025, lower.tail=F) #critical value

################################################################################
##############--------- Plotting Binary Moderator ------------##################
################################################################################

# Generating Data for Figure 1: Expected effect sizes given publication bias

### Figure 1a: N = 40, I2 = 0
dat_1a <- do.call(rbind,flattenlist(
  lapply(PB_vec, function(PB) 
    lapply( seq(0,0.8,0.001), function(g) 
      exp_val(PB=PB, Zcv=Zcv, g=g, N=N_small, I2 = 0) ) ) ) ) 

dat_1a$bias <- dat_1a$E - dat_1a$g
dat_1a$PB <- factor(dat_1a$PB)

p1a <- p_bias_exp_val(data=dat_1a, title= expression(N==40*","~tau^2==0*","~I^2==0*"%"))


### Figure 1b: N = 200, tau2 = I2 = 0
# create the data for Figure 1b
dat_1b <- do.call(rbind,flattenlist(
lapply(PB_vec, function(PB) 
  lapply( seq(0,0.8,0.001), function(g) 
    exp_val(PB=PB, Zcv=Zcv, g=g, N=N_large, I2 = 0) ) ) ) ) 
dat_1b$bias <- dat_1b$E - dat_1b$g
dat_1b$PB <- factor(dat_1b$PB)

# make Figure 1b
p1b <- p_bias_exp_val(data=dat_1b, title= expression(N==200*","~tau^2==0*","~I^2==0*"%"))

# Additional example calculations discussed in the text
# if theta = 0.1, then the expected moderator effect is (PB=0):
# group A: g = 0
subset(dat_1b, PB ==0 & g ==0) #PB = 0, then E = 0.3455
round(subset(dat_1b, PB ==0 & g ==0)$E, 3) #E = 0.3455
round(subset(dat_1b, PB ==0 & g ==0)$bias, 3) # bias = 0.2455
#group B: g = 0.1
subset(dat_1b, PB ==0 & g ==0.1) #PB = 0, then E = 0.3455
round(subset(dat_1b, PB ==0 & g ==0.1)$E, 3) #E = 0.3455
round(subset(dat_1b, PB ==0 & g ==0.1)$bias, 3) # bias = 0.2455
#moderator effect
round(subset(dat_1b, PB ==0 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0 & g ==0)$E, 3)
round(subset(dat_1b, PB ==0 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0 & g ==0)$E, 3) -0.1 #bias in moderator effect

#maximum bias when PB =0.05, N = 200,  I2 = 0 at a true effect size of 
PB05N200_I2_0 <- subset(dat_1b, PB ==0.05)
PB05N200_I2_0[which(PB05N200_I2_0$bias== max(PB05N200_I2_0$bias)),]$g #g=0.1140
#if theta = 0.1, then the expected moderator effect is (PB=0.05):
# group A
subset(dat_1b, PB ==0.05 & g ==0) #PB = 0, then E = 0.1066
round(subset(dat_1b, PB ==0.05 & g ==0)$E, 3) #E = 0.1066
round(subset(dat_1b, PB ==0.05 & g ==0)$bias, 3) # bias = 0.1066
#group B
subset(dat_1b, PB ==0.05 & g ==0.1) #PB = 0, then E = 0.2634
round(subset(dat_1b, PB ==0.05 & g ==0.1)$E, 3) #E = 0.2634
round(subset(dat_1b, PB ==0.05 & g ==0.1)$bias, 3) # bias = 0.1634
#moderator effect
round(subset(dat_1b, PB ==0.05 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0.05 & g ==0)$E, 3) #mod effect = 0.1568
round(subset(dat_1b, PB ==0.05 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0.05 & g ==0)$E, 3) -0.1 #bias in moderator effect: 0.0568


### Figure 1c: N = 40, I2 = 75%
# create the data for Figure 1b
dat_1c <- do.call(rbind,flattenlist(
  lapply(PB_vec, function(PB) 
    lapply( seq(0,0.8,0.001), function(g) 
      exp_val(PB=PB, Zcv=Zcv, g=g, N=N_small, I2 = 0.75) ) ) ) ) 

# obtain the mean tau2 from this set
meantau2 <- mean(dat_1c$tau2)

