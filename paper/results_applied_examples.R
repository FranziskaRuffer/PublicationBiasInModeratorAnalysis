#' This script contains the code to reproduce the Figures for the two applied
#' examples in the accompanying paper.
#'
#' #

#' Install and load the Project's R package
remotes::install_github("FranziskaRuffer/PublicationBiasInModeratorAnalysis")
library(PublicationBiasInModeratorAnalysis)

#' Load the correct versions of the required R packages
renv::restore()
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
#' NoPB indicates studies that did not face publication selection: (TRUE)
#' including pre-registered studies and non-published studies
dat$NoPB <- ifelse(dat$Preregistered==1 |dat$PRPublication=="No" , TRUE, FALSE)
mods <- dat$Preregistered

#' default parameter setting in the shiny app
beta1 = 0
I2res = c(0, 0.25, 0.5, 0.75)
PB = c(0, 0.05, 0.2, 0.5, 1)
Zcv <- qnorm(0.025, lower.tail=F) #testing for a positive effect

#' Creating the three default figures from the shiny app manually
PBanalysis_plots(dat = dat, mods = mods, mem = mema,Zcv = Zcv, beta0 =as.numeric(rema$beta), mod.title = "Moderator: Pre-Registered (No=0, Yes=1)")
PBanalysis_plots(dat = dat, mods = mods, mem = mema,Zcv = Zcv, beta0 =as.numeric(rema$beta)/2, mod.title = "Moderator: Pre-Registered (No=0, Yes=1)")
PBanalysis_plots(dat = dat, mods = mods, mem = mema,Zcv = Zcv, beta0 =0, mod.title = "Moderator: Pre-Registered (No=0, Yes=1)")

# Alternatively, you can call the pb_mods_App() shiny app and upload the data as .csv or .tsv file
#' Note: Specify another directory, if you do not want ot save the data in your current directory!
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
dat$NoPB <- rep(FALSE, nrow(dat))  #no information about pre-registration (and only published studies)

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


#' Shiny app publication bias plots
PBanalysis_plots(dat = dat, mods = dat$pperf, mem = mem,Zcv = Zcv, beta0 =as.numeric(res$beta), mod.title = "Moderator: Number of Practices per Facilitator")
PBanalysis_plots(dat = dat, mods = dat$pperf, mem = mem,Zcv = Zcv, beta0 =as.numeric(res$beta)/2, mod.title = "Moderator: Number of Practices per Facilitator")
PBanalysis_plots(dat = dat, mods = dat$pperf, mem = mem,Zcv = Zcv, beta0 =0, mod.title = "Moderator: Number of Practices per Facilitator")

#' Alternatively, you can call the pb_mods_App() shiny app and upload the data as .csv or .tsv file
#' Note: Specify another directory, if you do not want ot save the data in your current directory!
write.csv(dat, "dat.baskerville2012.csv")
pb_mods_App() # select the data file and variables in the app, check the manual tab for help



