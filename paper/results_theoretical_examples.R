# R code to reproduce the Figures and Calculations of all theoretical examples in the paper.


#' Install and load the Project's R package
remotes::install_github("FranziskaRuffer/PublicationBiasInModeratorAnalysis")
library(PublicationBiasInModeratorAnalysis)

#load additional packages
library(svglite)

# helper function to flatten lists
flattenlist <- function(list){PublicationBiasInModeratorAnalysis:::flattenlist(list)}

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

##################### ----------  FIGURE 1  ---------------- ###################
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
subset(dat_1b, PB ==0 & g ==0) #PB = 0, then E = 0.311
round(subset(dat_1b, PB ==0 & g ==0)$E, 3) #E = 0.331
round(subset(dat_1b, PB ==0 & g ==0)$bias, 3) # bias = 0.331
#group B: g = 0.1
subset(dat_1b, PB ==0 & g ==0.1) #PB = 0, then E = 0.345
round(subset(dat_1b, PB ==0 & g ==0.1)$E, 3) #E = 0.345
round(subset(dat_1b, PB ==0 & g ==0.1)$bias, 3) # bias = 0.245
#moderator effect
round(subset(dat_1b, PB ==0 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0 & g ==0)$E, 3) #moderator effect = .014
round(subset(dat_1b, PB ==0 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0 & g ==0)$E, 3) -0.1 #bias in moderator effect = -.086

#maximum bias when PB =0.05, N = 200,  I2 = 0 at a true effect size of
PB05N200_I2_0 <- subset(dat_1b, PB ==0.05)
PB05N200_I2_0[which(PB05N200_I2_0$bias== max(PB05N200_I2_0$bias)),]$g #g=0.114
#if theta = 0.1, then the expected moderator effect is (PB=0.05):
# group A: g = 0
subset(dat_1b, PB ==0.05 & g ==0) #PB = 0, then E = 0.107
round(subset(dat_1b, PB ==0.05 & g ==0)$E, 3) #E = 0.107
round(subset(dat_1b, PB ==0.05 & g ==0)$bias, 3) # bias = 0.107
#group B: g = 0.1
subset(dat_1b, PB ==0.05 & g ==0.1) #PB = 0, then E = 0.263
round(subset(dat_1b, PB ==0.05 & g ==0.1)$E, 3) #E = 0.263
round(subset(dat_1b, PB ==0.05 & g ==0.1)$bias, 3) # bias = 0.163
#moderator effect
round(subset(dat_1b, PB ==0.05 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0.05 & g ==0)$E, 3) #mod effect = 0.156
round(subset(dat_1b, PB ==0.05 & g ==0.1)$E, 3) - round(subset(dat_1b, PB ==0.05 & g ==0)$E, 3) -0.1 #bias in moderator effect: 0.056


### Figure 1c: N = 40, I2 = 75%
# create the data for Figure 1b
dat_1c <- do.call(rbind,flattenlist(
  lapply(PB_vec, function(PB)
    lapply( seq(0,0.8,0.001), function(g)
      exp_val(PB=PB, Zcv=Zcv, g=g, N=N_small, I2 = 0.75) ) ) ) )

# obtain the mean tau2 from this set
meantau2 <- mean(dat_1c$tau2)

# create the data again for Figure 1b using the mean tau2
dat_1c <- do.call(rbind,flattenlist(
  lapply(PB_vec, function(PB)
    lapply( seq(0,0.8,0.001), function(g)
      exp_val(PB=PB, Zcv=Zcv, g=g, N=N_small, tau2 = meantau2) ) ) ) )
mean(dat_1c$I2)  #mean I2 is indeed 75% now

# calculate the bias in each effect size
dat_1c$bias <- dat_1c$E - dat_1c$g
dat_1c$PB <- factor(dat_1c$PB)

# make Figure 1c
p1c <- p_bias_exp_val(data=dat_1c, title= expression(N==40*","~tau^2==0.313*","~I^2==75*"%"))


### Figure 1d: N = 200, tau2 = meantau2 = .313
dat_1d <- do.call(rbind,flattenlist(
  lapply(PB_vec, function(PB)
    lapply( seq(0,0.8,0.001), function(g)
      exp_val(PB=PB, Zcv=Zcv, g=g, N=N_large, tau2 = meantau2) ) ) ) )
mean(dat_1d$I2) #93.8%

# calculate the bias in each effect size
dat_1d$bias <- dat_1d$E - dat_1d$g
dat_1d$PB <- factor(dat_1d$PB)

# make Figure 1d
p1d <- p_bias_exp_val(data=dat_1d, title= expression(N==200*","~tau^2==0.313*","~I^2==93.8*"%"))

# plot and save Figure 2 as svg
Fig1 <- plot_grid_1legend(p1a ,p1c ,p1b ,p1d)
ggplot2::ggsave(plot = Fig1, filename = "Figure1.svg",  width = 1500*0.3, height = 1400*0.3, units = "mm",limitsize = FALSE)


# maximum bias when PB =0.05, N = 200,  I2 = 0.94 at a true effect size of
PB05N200_I2_75 <- subset(dat_1d, PB ==0.05)
PB05N200_I2_75[which(dat_1d$bias== max(dat_1d$bias)),]$g #g=0

# if theta = 0.1, then the expected moderator effect is (PB=0)
# group A
subset(dat_1d, PB ==0 & g ==0) #PB = 0, then E = 0.651
round(subset(dat_1d, PB ==0 & g ==0)$E, 3) #E = 0.651
round(subset(dat_1d, PB ==0 & g ==0)$bias, 3) # bias = 0.651
# group B
subset(dat_1d, PB ==0 & g ==0.1) #PB = 0, then E = 0.679
round(subset(dat_1d, PB ==0 & g ==0.1)$E, 3) #E = 0.679
round(subset(dat_1d, PB ==0 & g ==0.1)$bias, 3) # bias = 0.579
# moderator effect
round(subset(dat_1d, PB ==0 & g ==0.1)$E - subset(dat_1d, PB ==0 & g ==0)$E, 3) #moderator effect = .029
round(subset(dat_1d, PB ==0 & g ==0.1)$E - subset(dat_1d, PB ==0 & g ==0)$E, 3) -0.1 #bias in moderator effect = -.071


# if theta = 0.1, then the expected moderator effect is (PB=0.05)
# group A: g=0
subset(dat_1d, PB ==0.05 & g ==0) #PB = 0, then E = 0.558
round(subset(dat_1d, PB ==0.05  & g ==0)$E, 3) #E = 0.558
round(subset(dat_1d, PB ==0.05  & g ==0)$bias, 3) # bias = 0.558
# group B: g=0.1
subset(dat_1d, PB ==0.05  & g ==0.1) #PB = 0, then E = 0.609
round(subset(dat_1d, PB ==0.05  & g ==0.1)$E, 3) #E = 0.609
round(subset(dat_1d, PB ==0.05  & g ==0.1)$bias, 3) # bias = 0.509
# moderator effect
round(subset(dat_1d, PB ==0.05  & g ==0.1)$E - subset(dat_1d, PB ==0.05  & g ==0)$E, 3) #moderator effect = .051
round(subset(dat_1d, PB ==0.05  & g ==0.1)$E - subset(dat_1d, PB ==0.05  & g ==0)$E, 3) -0.1 #bias in moderator effect = -.049

##################### ----------  FIGURE 2  ---------------- ###################
# Generating Data for Figure 2: Expected effect sizes given publication bias

#generate values for the plot with PB1 = 0, PB2 = 0.2, N = 80, I2=0
dat2a <- do.call(rbind,flattenlist(
  lapply(c(PB_extreme, PB_medium), function(PB)
    lapply( seq(0,0.8,0.001), function(g)
      exp_val(PB=PB, Zcv=Zcv, g=g, N=N_medium, I2 = 0) ) ) ) )

dat2a_wide <- do.call(rbind, flattenlist(
  lapply( seq(0,0.8,0.001), function(g)
    exp_val_BM(PB= c(PB_extreme,PB_medium), Zcv = Zcv, g=c(g, g),
               N=c(N_medium, N_medium), I2 = 0))
))

#bias in the moderator effect
dat2a$bias <- rep(subset(dat2a, PB == PB_extreme)$E - subset(dat2a, PB == PB_medium)$E,2)

biases <- c(dat2a_wide$diffE, dat2a_wide$diffE)

