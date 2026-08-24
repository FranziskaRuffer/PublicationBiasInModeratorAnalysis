#' This script contains the code to reproduce the Figures for the two applied
#' examples in the accompanying paper.

#' Install and load the Project's R package
remotes::install_github("FranziskaRuffer/PublicationBiasInModeratorAnalysis")
library(PublicationBiasInModeratorAnalysis)

#' Load the required libraries
library(metafor)
library(metadat)
#loading package to save figures as svg
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("svglite", quietly = TRUE)) {
  install.packages("svglite")
}
library(svglite)

#' #############################################################################
#' # Example Binary Moderator: Red Romance Effect (Lehmann et al. (2017))
#' Show more information about the meta-analysis
?dat.lehmann2018

#' Select the studies with a female sample which have information about the
#' pre-registration status
dat <- subset(dat.lehmann2018, Gender=="Females"  &  is.na(Preregistered)==FALSE)

#' descriptive information
nrow(dat)  # 36 studies
table(dat$Preregistered)  # 8 studies are preregistered, 28 are not
table(dat$Preregistered, dat$PRPublication) #16 published in a peer-reviewed journal

#' # Random effects model
rema <- rma(yi=yi, vi = vi, data=dat)
summary(rema)

#' Publication Bias Analyses Random Effects Model
#' Selection model (no moderator)
selmodel(rema, "stepfun", steps = c(0.025, 1))
#' Egger's test & funnel plot
funnel(rema)
regtest(rema, model ="lm")  # funnel plot asymmetry
summary(lm(formula = yi ~ I(sqrt(vi)), data = dat, weights = 1/(vi)))

#' # Mixed-effects model with "Preregistered" as moderator
dat$Preregistered <- ifelse(dat$Preregistered=="Pre-Registered",1 ,0)
mema <- rma(yi=yi, vi = vi, data=dat, mods=~ Preregistered)
summary(mema)
regplot(mema)

#' Publication Bias analyses Mixed-Effects Model
#' Selection model
selmodel(mema, "stepfun", steps = c(0.025, 1))
#'    -> selection model fits better than mema model
#'    -> 9 significant effect sizes

#' Additonal: Subgroup analysis: Preregistered vs non-preregistered
rema_prereg <- rma(yi=yi, vi = vi, data=subset(dat, Preregistered==1))
summary(rema_prereg)  # no heterogeneity
rema_nonprereg <- rma(yi=yi, vi = vi, data=subset(dat, Preregistered==0))
summary(rema_nonprereg)  # heterogeneous effect sizes (I2 = 60%)
# -> location-scale model
lsma <- rma(yi=yi, vi = vi, data=dat, mods=~ Preregistered, scale=~ Preregistered)
lsma  #tau2 are not significantly different between pre-reg and non-prereg studies
round(unique(lsma$tau2.f), 3)  #two tau2 parameters rounded


#' Making sure that the naming is correct for the PBanalysis_plots() function
#' at.risk.of.PB indicates studies that are not at risk of publication bias: 'FALSE'
#' including pre-registered studies and non-published studies
dat$at.risk.of.PB <- ifelse(dat$Preregistered==1 |dat$PRPublication=="No" , 'FALSE', 'TRUE')
mods <- dat$Preregistered

#' default parameter setting in the shiny app
beta1 = 0
I2res = c(0, 0.25, 0.5, 0.75)
tau2eres = c(0, .01, .04, .11)
PP = c(0, 0.05, 0.2, 0.5, 1)
Zcv <- qnorm(0.025, lower.tail=F) #testing for a positive effect

#' Creating the three default figures from the shiny app manually
#' and saving them as svg
Fig5 <- PBanalysis_plots(dat = dat, mods = mods, mem = mema,Zcv = Zcv, beta0 =0,
                         heterogeneity = "tau2res", mod.title = "Moderator: Pre-registered (no=0, yes=1)")
#Fig5
svglite::svglite(filename = "Figure5.svg",  width = 850*0.3 / 25.4,
                 height = 707*0.3 / 25.4)
print(Fig5)
dev.off()

# Figure 7
Fig6 <- PBanalysis_plots(dat = dat, mods = mods, mem = mema,Zcv = Zcv, beta0 =as.numeric(rema$beta)/2,
                         heterogeneity = "tau2res", mod.title = "Moderator: Pre-registered (no=0, yes=1)")
svglite::svglite(filename = "Figure6.svg",  width = 850*0.3 / 25.4,
                 height = 707*0.3 / 25.4)
print(Fig6)
dev.off()

# In Figure 6, certain scenarios return an intercept close to the observed,
# while yielding a less negative moderator effect. These scenarios are:
# Fig6 estimates: beta0 = 0.065, PP = 0.05, tau2 = 0.01
exp_given_PB <- do.call(rbind, PublicationBiasInModeratorAnalysis:::flattenlist(
  lapply(1:nrow(dat), function(i)  {
  exp_val_MA(PP=.05, Zcv=Zcv, vg= dat$vi[i], vgvec=dat$vi, tau2=0.01, x1=mods[i],
             x1vec = mods, beta0= as.numeric(rema$beta)/2, beta1= 0, at.risk.of.PB = dat$at.risk.of.PB[i],
             lower.tail = FALSE)
} )))
betas <- PublicationBiasInModeratorAnalysis:::betas_PB(exp_given_PB)
(beta_info <- round(data.frame("beta0PB" = betas[1], "beta1PB" = betas[2],
                        "tau2" = 0.01, "PP" = .05), 3))

# Fig6 estimates: beta0 = 0.065, PP = 0.05, tau2 = 0.04
exp_given_PB <- do.call(rbind, PublicationBiasInModeratorAnalysis:::flattenlist(
  lapply(1:nrow(dat), function(i)  {
    exp_val_MA(PP=.05, Zcv=Zcv, vg= dat$vi[i], vgvec=dat$vi, tau2=0.04, x1=mods[i],
               x1vec = mods, beta0= as.numeric(rema$beta)/2, beta1= 0, at.risk.of.PB = dat$at.risk.of.PB[i],
               lower.tail = FALSE)
  } )))
betas <- PublicationBiasInModeratorAnalysis:::betas_PB(exp_given_PB)
(beta_info <- round(data.frame("beta0PB" = betas[1], "beta1PB" = betas[2],
                         "tau2" = 0.04, "PP" = .05),3))

# Fig6 estimates: beta0 = 0.065, PP = 0.2, tau2 = 0.11
exp_given_PB <- do.call(rbind, PublicationBiasInModeratorAnalysis:::flattenlist(
  lapply(1:nrow(dat), function(i)  {
    exp_val_MA(PP=.2, Zcv=Zcv, vg= dat$vi[i], vgvec=dat$vi, tau2=0.11, x1=mods[i],
               x1vec = mods, beta0= as.numeric(rema$beta)/2, beta1= 0, at.risk.of.PB = dat$at.risk.of.PB[i],
               lower.tail = FALSE)
  } )))
betas <- PublicationBiasInModeratorAnalysis:::betas_PB(exp_given_PB)
(beta_info <- round(data.frame("beta0PB" = betas[1], "beta1PB" = betas[2],
                         "tau2" = 0.11, "PP" = .2),3))

# In Figure 6, the observed moderator effect of -.293 could only be approached
# when assuming extreme publication bias (PP=0) and considerable heterogeneity (tau2=.11).
# In this case, however, the intercept is overestimated (observed b0 = .194).
exp_given_PB <- do.call(rbind, PublicationBiasInModeratorAnalysis:::flattenlist(
  lapply(1:nrow(dat), function(i)  {
    exp_val_MA(PP=0, Zcv=Zcv, vg= dat$vi[i], vgvec=dat$vi, tau2=0.11, x1=mods[i],
               x1vec = mods, beta0= as.numeric(rema$beta)/2, beta1= 0, at.risk.of.PB = dat$at.risk.of.PB[i],
               lower.tail = FALSE)
  } )))
betas <- PublicationBiasInModeratorAnalysis:::betas_PB(exp_given_PB)
(beta_info <- round(data.frame("beta0PB" = betas[1], "beta1PB" = betas[2],
                               "tau2" = 0.11, "PP" = 0),3))

# Figure 7
Fig7 <- PBanalysis_plots(dat = dat, mods = mods, mem = mema,Zcv = Zcv, beta0 =as.numeric(rema$beta),
                         heterogeneity = "tau2res", mod.title = "Moderator: Pre-registered (no=0, yes=1)")
svglite::svglite(filename = "Figure7.svg",  width = 850*0.3 / 25.4,
                 height = 707*0.3 / 25.4)
print(Fig7)
dev.off()


# Alternatively, you can call the pb_mods_App() shiny app and upload the data as .csv, .tsv or txt. file
#' Note: Specify another directory, if you do not want to save the data in your current directory!
write.csv(dat, "dat.lehmann2018.csv")
pb_mods_App()  # select the data file and variables in the app, check the manual tab for help


#' #############################################################################
#' # Continuous Moderator: Primary Care effectiveness intervention (Baskerville et al. (2012))
#' Show more information about the meta-analysis
?dat.baskerville2012

#' select the studies with a female sample which have information about the
#' pre-registration status
dat <- subset(dat.baskerville2012, is.na(pperf)==FALSE)

#' Descriptive information
nrow(dat)  # 21 studies
table(dat$pperf)  #moderator ranges from 3 to 40. while most lie below 20

#' Renaming some variables to fit the PBanalysis_plot() function
dat$vi <- dat$se^2
dat$yi <- dat$smd
dat$at.risk.of.PB <- rep(TRUE, nrow(dat))  #no information about pre-registration (and only published studies)

#' random effects model
res <- rma(smd, sei=se, data=dat)
summary(res)

#' Publication Bias Analysis Random Effects Model
#' selection model (no moderator)
sel <- selmodel(res, "stepfun", steps = c(0.025, 1))
#' Egger's test & funnel plot
funnel(res)
regtest(res, model="lm")

#' mixed-effects model
#' moderator: facilitator to practice ratio
mem <- rma(smd, sei=se, data=dat, mods=~pperf)
summary(mem)
regplot(mem)

#' sensitivity analysis excluding pperf=40
mem_sens <- rma(smd, sei=se, data=subset(dat, pperf!=40), mods=~pperf)
summary(mem_sens)

#' selection model with pperf moderator
selmodel(mem, "stepfun", steps = c(0.025, 1))
selmodel(mem_sens, "stepfun", steps = c(0.025, 1))


#' create Figure 8
p1 <- PublicationBiasInModeratorAnalysis:::individual_plots(dat = dat, mods = dat$pperf, heterogeneity = "tau2res", tau2res=0,
                                                           mem = mem,Zcv = Zcv, beta0 =0, ind = "a")
p2 <- PublicationBiasInModeratorAnalysis:::individual_plots(dat = dat, mods = dat$pperf, heterogeneity = "tau2res", tau2res=0.01 ,
                                                           mem = mem,Zcv = Zcv, beta0 =0, ind = "b")
p3 <- PublicationBiasInModeratorAnalysis:::individual_plots(dat = dat, mods = dat$pperf, heterogeneity = "tau2res", tau2res=0 ,
                                                           mem = mem,Zcv = Zcv, beta0 =as.numeric(res$beta)/2, ind = "c")
p4 <- PublicationBiasInModeratorAnalysis:::individual_plots(dat = dat, mods = dat$pperf, heterogeneity = "tau2res", tau2res=0.01 ,
                                                           mem = mem,Zcv = Zcv, beta0 =as.numeric(res$beta)/2, ind = "d")
p5 <- PublicationBiasInModeratorAnalysis:::individual_plots(dat = dat, mods = dat$pperf, heterogeneity = "tau2res", tau2res=0,
                                                           mem = mem,Zcv = Zcv, beta0 =as.numeric(res$beta), ind = "e")
p6 <- PublicationBiasInModeratorAnalysis:::individual_plots(dat = dat, mods = dat$pperf, heterogeneity = "tau2res", tau2res=0.01 ,
                                                           mem = mem,Zcv = Zcv, beta0 =as.numeric(res$beta), ind = "f")

Fig8 <- plot_grid_1legend_6(p1, p2, p3, p4, p5, p6)

svglite::svglite(filename = "Figure8.svg",  width = 850*0.3 / 25.4,
                 height = 1200*0.3 / 25.4)
print(Fig8)
dev.off()



#' Alternatively, you can call the pb_mods_App() shiny app and upload the data as .csv or .tsv file
#' Note: Specify another directory, if you do not want ot save the data in your current directory!
write.csv(dat, "dat.baskerville2012.csv")
pb_mods_App() # select the data file and variables in the app, check the manual tab for help



