# R code to reproduce the Figures and Calculations of all theoretical examples in the paper.


#' Install and load the Project's R package
remotes::install_github("FranziskaRuffer/PublicationBiasInModeratorAnalysis")
library(PublicationBiasInModeratorAnalysis)
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

p1a <- p_bias_exp_val(data=dat_1a, x="g", y="E",
                      title= expression(N==40*","~tau^2==0*","~I^2==0*"%"))


### Figure 1b: N = 200, tau2 = I2 = 0
# create the data for Figure 1b
dat_1b <- do.call(rbind,flattenlist(
lapply(PB_vec, function(PB)
  lapply( seq(0,0.8,0.001), function(g)
    exp_val(PB=PB, Zcv=Zcv, g=g, N=N_large, I2 = 0) ) ) ) )
dat_1b$bias <- dat_1b$E - dat_1b$g
dat_1b$PB <- factor(dat_1b$PB)

# make Figure 1b
p1b <- p_bias_exp_val(data=dat_1b,  x="g", y="E",
                      title= expression(N==200*","~tau^2==0*","~I^2==0*"%"))

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
p1c <- p_bias_exp_val(data=dat_1c,  x="g", y="E",
                      title= expression(N==40*","~tau^2==0.313*","~I^2==75*"%"))


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
p1d <- p_bias_exp_val(data=dat_1d, x="g", y="E",
                      title= expression(N==200*","~tau^2==0.313*","~I^2==93.8*"%"))

# plot and save Figure 2 as svg
Fig1 <- plot_grid_1legend(p1a ,p1c ,p1b ,p1d)
Fig1
ggplot2::ggsave(plot = Fig1,
                filename = "Figure1.svg",
                width = 1500*0.3, height = 1400*0.3, units = "mm",limitsize = FALSE)


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

#bias in the moderator effect when the moderator effect beta0=0, since gA = gB
dat2a$bias <- rep(subset(dat2a, PB == PB_extreme)$E - subset(dat2a, PB == PB_medium)$E,2)
groupB <- subset(dat2a, dat2a$PB == 0.2)
maxBiasB <- max(groupB$E - groupB$g )
groupB[which(groupB$E - groupB$g == maxBiasB),]
dat2a$PB <-factor(dat2a$PB)

#if theta = 0, then the expected moderator effect is
# group A
subset(dat2a, PB ==0 & g ==0) #PB = 0, then E = 0.524
round(subset(dat2a, PB ==0 & g ==0)$E, 3) #  E = 0.524
#group B
subset(dat2a, PB ==0.2 & g ==0) #PB = 0.2, then E = 0.048
round(subset(dat2a, PB ==0.2 & g ==0)$E, 3) # E = 0.048
#moderator effect
round(subset(dat2a, PB ==0 & g ==0)$E, 4) - round(subset(dat2a, PB ==0.2 & g ==0)$E, 4)

#zero moderator effect when thetaA=0, N=80, PB_A=0 and PB_B=0.2
round(sol.g2(N1=80, N2=80, g1=0,  Zcv=Zcv, PB1=0, PB2=0.2, I2=0),3) #when thetaB=.391
round(exp_val(PB=0.2, Zcv=Zcv, g=.3906, N=N_medium, I2 = 0)$E,3) #E_B = 0.524

p2a <- p_bias_diff_exp_val(data = dat2a, title = expression(bolditalic(a)*":"~N==80*","~I^2==0*"%"))

#plot PB1 = 0, PB2 = 0.2, N = 80, I2=0.75
dat2b <- do.call(rbind,flattenlist(
  lapply(c(PB_extreme, PB_medium), function(PB)
    lapply( seq(0,0.8,0.001), function(g)
      exp_val(PB=PB, Zcv=Zcv, g=g, N=N_medium, I2 = 0.75) ) ) ) )

dat2b$bias <- rep(subset(dat2b, PB == PB_extreme)$E - subset(dat2b, PB == PB_medium)$E,2)

groupB <- subset(dat2b, PB == 0.2)
maxBiasB <- max(groupB$E - groupB$g )
groupB[which(groupB$E - groupB$g == maxBiasB),]

dat2b$PB <-factor(dat2b$PB)

#if theta = 0, then the expected moderator effect is
# group A
subset(dat2b, PB ==0 & g ==0) #PB = 0, then E = 0.677
round(subset(dat2b, PB ==0 & g ==0)$E, 3) #E = 0.677
#group B
subset(dat2b, PB ==0.2 & g ==0) #PB = 0.2, then E = 0.268
round(subset(dat2b, PB ==0.2 & g ==0)$E, 3) # E = 0.268
#moderator effect
round(subset(dat2b, PB ==0 & g ==0)$E - subset(dat2b, PB ==0.2 & g ==0)$E, 3) #bias moderator effect = .409

#zero moderator effect when thetaA=0, N=80, PB_A=0 and PB_B=0.2
round(sol.g2(N1=80, N2=80, g1=0,  Zcv=Zcv, PB1=0, PB2=0.2, I2=0.75),3) #when thetaB=.432
round(exp_val(PB=0.2, Zcv=Zcv, g=.432, N=N_medium, I2 = 0.75)$E,3) #E_B = 0.677

# Create sub figure 2b
p2b <- p_bias_diff_exp_val(data = dat2b, title = expression(bolditalic(a)*":"~N==80*","~I^2==75*"%"))

# Create and save Figure 2
Fig2 <-plot_grid_1legend_2p(p2a, p2b)
Fig2
ggsave(plot = Fig2, filename = "Figure2.svg",  width = 1306*0.3, height = 701*0.3, units = "mm",limitsize = FALSE)


##################### ----------  FIGURE 3  ---------------- ###################
# Figure 3: Bias(beta1) = E(theta|PB, N = 40) - E(theta|PB, N = 200) - beta1
# tau2 = 0
dat_1a$E_b1 <- dat_1a$E - dat_1b$E
beta1 = 0
dat_1a$bias_b1 <- dat_1a$E_b1 - beta1
dat_1a$b1 <- dat_1a$g - dat_1b$g  #always zero
#-> so E(b1) -beta1 == bias(b1)
dat_1a$bias <- dat_1a$bias_b1
p3a <- p_bias_exp_val(dat_1a, x = "g", y="bias_b1",
                      title = expression(bolditalic(a)*":"~N[A]==40~"vs"~N[B]==200*","~tau^2==0) )

# tau2 = 0.313
dat_1c$E_b1 <- dat_1c$E - dat_1d$E
dat_1c$bias_b1 <- dat_1c$E_b1 -beta1
dat_1c$b1 <- dat_1c$g - dat_1c$g  #always zero
#-> so E(b1) == bias(b1)
dat_1c$PB <- factor(dat_1c$PB)
p3b <- p_bias_exp_val(dat_1c, x = "g", y="bias_b1",
                      title = expression(bolditalic(b)*":"~N[A]==40~"vs"~N[B]==200*","~tau^2==0.313) )

#Create Figure 3 and save as svg
Fig3 <- plot_grid_1legend_2p(p3a, p3b)
ggsave(plot = Fig3, filename = "Figure3.svg",  width = 2000*0.3, height = 923*0.3, units = "mm",limitsize = FALSE)


##################### ----------  FIGURE 4  ---------------- ###################
# create data for Figure 4
dat4 <- do.call(rbind,flattenlist(
  lapply(c(40,80), function(N)
    lapply( seq(0,1,0.01), function(g)
      exp_val(PB=0.05, Zcv=Zcv, g=g, N=N, I2=0) ) ) ) )
N2 <- 80
J2 <- (1 - 3/ (4 * (N2-2) - 1))

#calculating g in group 2, s.t. the moderator effect is hidden for several different
#true effect sizes in group 1
# g1 = 0.3
g2a <- sol.g2(N1=40, N2=80, g1=0.3,  Zcv=1.96, PB1=0.05, PB2=0.05, I2=0)
d2a <- g2a / J2
vd2a <- N2/(N2/2)^2 + d2a^2 / (2*N2)
mod2a_sol <- exp_val(PB=0.05, Zcv=Zcv, g=g2a, N=80, I2=0)

# g1 = 0.2
g2b <- sol.g2(N1=40, N2=80, g1=0.2,  Zcv=1.96, PB1=0.05, PB2=0.05, I2=0)
d2b <- g2b / J2
vd2b <- N2/(N2/2)^2 + d2b^2 / (2*N2)
mod2b_sol <- exp_val(PB=0.05, Zcv=Zcv, g=g2b, N=80, I2=0)

# g1 = 0.4
g2c <- sol.g2(N1=40, N2=80, g1=0.4,  Zcv=1.96, PB1=0.05, PB2=0.05, I2=0)
d2c <- g2c / J2
vd2c <- N2/(N2/2)^2 + d2c^2 / (2*N2)
mod2c_sol <- exp_val(PB=0.05, Zcv=Zcv, g=g2c, N=80, I2=0)

# g1 = 0.1
g2d <- sol.g2(N1=40, N2=80, g1=0.1,  Zcv=1.96, PB1=0.05, PB2=0.05, I2=0)
d2d <- g2d / J2
vd2d <- N2/(N2/2)^2 + d2d^2 / (2*N2)
mod2d_sol <- exp_val(PB=0.05, Zcv=Zcv, g=g2d, N=80, I2=0)

#Attach the values to the data
dat4 <- rbind(dat4, mod2a_sol, mod2b_sol,  mod2c_sol,  mod2d_sol)

#General examples plug-in
e <- 0.8
i <- sol.g2(N1=40, N2=80, g1=e,  Zcv=1.96, PB1=0.05, PB2=0.05, I2=0)
modsol <- exp_val(PB=0.05, Zcv=Zcv, g=i, N=80, I2=0)
dat4 <- rbind(dat4, modsol)
line1 <- dat4[which(dat4$N==40 & dat4$g==e),]
line2 <- dat4[which(dat4$N==80 & dat4$g==i),]
diff <- line2$g - line1$g

#coordinates for red arrows in Figure 4
coords <- data.frame(g1 = line1$g, g2 = line2$g,
                     E1 = line1$E, E2 = line2$E)
coords


# Create Figure  4
dat4$N <-factor(dat4$N)
size_text = 8
axis_text = 30
p4a <- ggplot(data=dat4,aes(x=g, y=E, linetype=N)) +
  geom_abline(intercept = 0, slope = 1, color="gray", linewidth=0.9)+
  geom_line(linewidth=1.2) +
  labs(y = expression(theta[i]^PB),
       x = expression(theta[i]),
       title = expression(PB==0.05*","~I^2==0*"%"))+
  scale_linetype_manual(values = c(1, 3), labels= c(expression("A:"~N[a]==40),
                                                    expression("B:"~N[b]==80)),
                        name = "Moderator Group") +
  theme(legend.text = element_text(size = axis_text-4),
        legend.key.size = unit(3,"line"),
        legend.title = element_text(size = axis_text-4),  # Adjust vertical position of legend title
        legend.position = "bottom",           # Position legend at the bottom
        title = element_text(size = axis_text-4),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.background = element_rect(fill = "white"),  # Set plot background color to white
        panel.background = element_rect(fill = "white"),  # Set panel background color to white
        panel.border = element_rect(color = "black", fill = NA),  # Set panel border color to black
        plot.margin = margin(10, 20, 10, 10, "pt"),
        axis.title.x = element_text(color = "black", size=axis_text),
        axis.title.y = element_text(color = "black", size=axis_text),
        axis.text.y = element_text(size=axis_text),
        axis.text.x = element_text(size=axis_text))  + # Add margin around the plot
  guides(linetype = guide_legend(title.position = "top"))  +# Place legend title on top
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 1, by = 0.1), expand=c(0,0)) +  # Customize x-axis ticks
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), expand=c(0,0))  # Customize y-axis ticks
p4a


line1a <- dat4[which(dat4$N==40 & dat4$g==0.3),]
line2a <- dat4[which(dat4$N==80 & dat4$g==g2a),]
diffa <- line1a$g - line2a$g

coordsa <- data.frame(g1 = line1a$g, g2 = line2a$g,
                      E1 = line1a$E, E2 = line2a$E)
coordsa_low <- data.frame(g1 = line1a$g, g2 = line2a$g,
                          E1 = 0, E2 = 0)

p4b <- p4a +
  geom_segment(aes(x =g1, y = E1, xend = g2, yend = E2),
               data = coordsa, linetype = "solid", colour="grey30",linewidth=1.5,
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_segment(aes(x = g2, y = E2, xend = g1, yend = E1),
               data = coordsa, linetype = "solid", colour="grey30",
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_text(inherit.aes = FALSE, parse=TRUE, x = 0.42, y = 0.69, size = size_text,
            label= "~theta[a] - theta[b] == -0.257")

p4b

line1b <- dat4[which(dat4$N==40 & dat4$g==0.2),]
line2b <- dat4[which(dat4$N==80 & dat4$g==g2b),]
diffb <- line1b$g - line2b$g

coordsb <- data.frame(g1 = line1b$g, g2 = line2b$g,
                      E1 = line1b$E, E2 = line2b$E)
coordsb_low <- data.frame(g1 = line1b$g, g2 = line2b$g,
                          E1 = 0, E2 = 0)
p4c <- p4b +
  geom_segment(aes(x =g1, y = E1, xend = g2, yend = E2),
               data = coordsb, linetype = "solid", colour="grey30",linewidth=1.5,
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_segment(aes(x = g2, y = E2, xend = g1, yend = E1),
               data = coordsb, linetype = "solid", colour="grey30",
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_text(inherit.aes = FALSE,parse=TRUE, x = 0.30, y = 0.58, size = size_text,
            label= "~theta[a] - theta[b] ==-0.156")
p4c


line1c <- dat4[which(dat4$N==40 & dat4$g==0.4),]
line2c <- dat4[which(dat4$N==80 & dat4$g==g2c),]
diffc <- line1c$g - line2c$g

coordsc <- data.frame(g1 = line1c$g, g2 = line2c$g,
                      E1 = line1c$E, E2 = line2c$E)
coordsc_low <- data.frame(g1 = line1c$g, g2 = line2c$g,
                          E1 = 0, E2 = 0)
p4d <- p4c +
  geom_segment(aes(x =g1, y = E1, xend = g2, yend = E2),
               data = coordsc, linetype = "solid", colour="grey30",linewidth=1.5,
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_segment(aes(x = g2, y = E2, xend = g1, yend = E1),
               data = coordsc, linetype = "solid", colour="grey30",
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_text(inherit.aes = FALSE,parse=TRUE, x = 0.55, y = 0.77, size = size_text,
            label=  "~theta[a] - theta[b] ==-0.279")
p4d


line1d <- dat4[which(dat4$N==40 & dat4$g==0.1),]
line2d <- dat4[which(dat4$N==80 & dat4$g==g2d),]
diffd <- line1d$g - line2d$g

coordsd <- data.frame(g1 = line1d$g, g2 = line2d$g,
                      E1 = line1d$E, E2 = line2d$E)
coordsc_low <- data.frame(g1 = line1d$g, g2 = line2d$g,
                          E1 = 0, E2 = 0)
Fig4 <- p4d +
  geom_segment(aes(x =g1, y = E1, xend = g2, yend = E2),
               data = coordsd, linetype = "solid", colour="grey30",linewidth=1.5,
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_segment(aes(x = g2, y = E2, xend = g1, yend = E1),
               data = coordsd, linetype = "solid", colour="grey30",
               arrow = arrow(length = unit(0.11, "inches"), type = "closed", ends = "last")) +
  geom_text(inherit.aes = FALSE,parse=TRUE, x = 0.2, y = 0.40, size = size_text,
            label= "~theta[a] - theta[b] ==-0.061")
Fig4

ggsave(plot = Fig4, filename = "Figure4.svg",  width = 1250*0.3, height = 1300*0.3, units = "mm",limitsize = FALSE)

### END OF THE SCRIPT ###
