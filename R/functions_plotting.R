#' Plotting Function Figure 1
#'
#' @export
p_bias_diff_exp_val <- function(data, title, size_text = 8, axis_text = 23){
  ggplot(data=data,aes(x=g, y=E, linetype=PB)) +
  geom_abline(intercept = 0, slope = 1, color="#9E9E9E", linewidth=0.9)+
  geom_line(linewidth=1.2) +
  geom_line(aes(y = bias, color="Bias"), linewidth=1.2) +
  labs( x = expression(theta[i]),
        y=expression(theta[i]^PB),
         color="",
        title = title) +
  scale_linetype_manual(values = c(1, 2),
                        labels = c(expression("A:"~PB[a]==0),
                                   expression("B:"~PB[b]==0.2)),
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
        panel.border = element_rect(color = "#000000", fill = NA),  # Set panel border color to black
        plot.margin = margin(10, 10, 10, 10, "pt"),
        axis.title.x = element_text(color = "#000000", size=axis_text-2),
        axis.title.y = element_text(color = "#000000", size=axis_text-2),
        axis.text.y = element_text(size=axis_text-2),
        axis.text.x = element_text(size=axis_text-2),
        axis.line.y.right = element_line(color = "#DF536B"),
        axis.ticks.y.right = element_line(color = "#DF536B"),
        axis.text.y.right = element_text(color = "#DF536B", size=axis_text-2),
        axis.title.y.right = element_text(color = "#DF536B", size=axis_text-2) )  + # Add margin around the plot
  guides(linetype = guide_legend(title.position = "top"),
         colour = guide_legend(title.position = "top"))  +# Place legend title on top
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.1), expand=c(0,0)) +  # Customize x-axis ticks
  scale_color_manual(values = c("#DF536B", "#000000"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name="Bias",breaks = seq(0,1, by = 0.1)),
                     limits = c(-0.01, 1), breaks = seq(0, 1, by = 0.1), expand=c(0,0))

}


#' Plotting Function Figure 2
#'
#' @export
p_bias_exp_val <- function(data, x, y,  title, size_text = 8, axis_text = 23){
  ggplot2::ggplot(data=data,aes(x=get(x, data), y=get(y, data), linetype=PB)) +
    geom_line(linewidth=1.2) +
    labs( x = expression(theta[i]) ,
          y= "Bias",
          title = title) +
    scale_linetype_manual(values = 1:5,
                          labels = c("0", "0.05", "0.2", "0.5", "1"),
                          name = "Proportion of published non-significant studies [PB]") +
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
          plot.margin = margin(10, 15, 10, 10, "pt"),
          axis.title.x = element_text(color = "black", size=axis_text-2),
          axis.title.y = element_text(color = "black", size=axis_text-2),
          axis.text.y = element_text(size=axis_text-2),
          axis.text.x = element_text(size=axis_text-2))  + # Add margin around the plot
    guides(linetype = guide_legend(title.position = "top"),
           colour = guide_legend(title.position = "top"))  + # Place legend title on top
    scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.1), expand=c(0,0)) +  # Customize x-axis ticks
    scale_y_continuous(
      limits = c(-0.01, 1), breaks = seq(0, 1, by = 0.1), expand=c(0,0))
}

#' Shiny app final default plot
#' @export
PBanalysis_plots <- function(dat, mods, mem, Zcv, beta0=0, beta1=0, mod.title="Moderator value",
                             I2res=c(0, 0.25, 0.5, 0.75), PB=c(0, 0.05, 0.2, 0.5, 1)){

  exp_given_PB =do.call(rbind, flattenlist(
    lapply(I2res, function(i2res){
      lapply(PB, function(pb){
        lapply(1:nrow(dat), function(i)  {
          exp_val_MA(PB=pb, Zcv=Zcv, vg= dat$vi[i], vgvec=dat$vi, I2=i2res, x1=mods[i],
                    x1vec = mods, beta0= beta0, beta1= beta1, NoPB = dat$NoPB[i])
        } )
      })
    } )))

  PBbiased_betas <- do.call(rbind, flattenlist(
    lapply(PB, function(pb){
      lapply(I2res, function(i2){
        betas <- betas_PB(subset(exp_given_PB, PB == pb & I2 ==i2))
        beta_info <- data.frame("beta0PB" = betas[1], "beta1PB" = betas[2],
                                "I2" = i2, "PB" = pb)
        return(beta_info)
      })
    })))

  PBbiased_betas$PB <- factor(PBbiased_betas$PB)
  PB_dat <- merge(exp_given_PB,PBbiased_betas,by=c("I2","PB"))
  exp_given_PB$PB <- factor(exp_given_PB$PB)
  original <- data.frame("vg" = dat$vi, "typ_v_T"=rep(NA, nrow(dat)), "tau2"=rep(mem$tau2, nrow(dat)),
                         "I2"=rep(0, nrow(dat)), "PB" = rep("original",nrow(dat)), "g"=rep(NA, nrow(dat)),
                         x1 = mods, beta0=rep(beta0, nrow(dat)), beta1 = rep(beta1, nrow(dat)),
                         ycv = rep(NA, nrow(dat)), NoPB = dat$NoPB,  E_sig = rep(NA, nrow(dat)), E_nsig = rep(NA, nrow(dat)),
                         E = dat$yi)
  PB_dat <- rbind(exp_given_PB, original )
  weights <- 1/(original$vg + original$tau2)
  rel_weights <- weights/max(weights) * 4 +0.5

  #creating the plots
  I20_PBorig_b <- data.frame("beta0PB" =mem$beta[1], "beta1PB" =mem$beta[2], "I2" = 0, "PB" = "original"  )
  PB_I2_0 <-  rbind(subset(PBbiased_betas, I2 ==0 ), I20_PBorig_b )
  p1 <- plot_contmod(data=subset(PB_dat, I2 == 0 & PB == "original"), I2_PB =PB_I2_0, beta1=beta1, beta0 = beta0, I2=0, weights=rel_weights, mod.title = mod.title )

  I20_PBorig_b <- data.frame("beta0PB" =mem$beta[1], "beta1PB" =mem$beta[2], "I2" = 0.25, "PB" = "original"  )
  PB_dat$I2[which(PB_dat$PB =="original")] <- 0.25
  PB_I2_25 <-  rbind(subset(PBbiased_betas, I2 ==0.25), I20_PBorig_b )
  p2 <- plot_contmod(data=subset(PB_dat, I2 == 0.25 & PB == "original"), I2_PB =PB_I2_25, beta1=beta1, beta0 = beta0, I2=0.25, weights=rel_weights, mod.title = mod.title  )

  I20_PBorig_b <- data.frame("beta0PB" =mem$beta[1], "beta1PB" =mem$beta[2], "I2" = 0.5, "PB" = "original"  )
  PB_dat$I2[which(PB_dat$PB =="original")] <- 0.5
  PB_I2_50 <-  rbind(subset(PBbiased_betas, I2 ==0.5), I20_PBorig_b )
  p3 <- plot_contmod(data=subset(PB_dat, I2 == 0.5 & PB == "original"), I2_PB =PB_I2_50, beta1=beta1, beta0 = beta0, I2=0.5, weights=rel_weights, mod.title = mod.title  )


  I20_PBorig_b <- data.frame("beta0PB" =mem$beta[1], "beta1PB" =mem$beta[2], "I2" = 0.75, "PB" = "original"  )
  PB_dat$I2[which(PB_dat$PB =="original")] <- 0.75
  PB_I2_75 <-  rbind(subset(PBbiased_betas, I2 ==0.75), I20_PBorig_b )
  p4 <- plot_contmod(data=subset(PB_dat, I2 == 0.75 & PB == "original"), I2_PB =PB_I2_75, beta1=beta1, beta0 = beta0, I2=0.75, weights=rel_weights, mod.title = mod.title  )


  p5 <- plot_grid_1legend(p1, p3, p2, p4)
  return(p5)
}

#' Shiny app additional plot
#' @export
Plot_additional_analysis <- function(data, beta0, beta1, I2, PB, mem, betasPB, mod.title = "Moderator value"){

  weights <- 1/(data$vi + mem$tau2)
  rel.weights <- weights/max(weights) * 4 +0.5

  title <- substitute(beta[0] == val1 * "," ~ beta[1]==val2 * "," ~ PB == val3 ~ "and" ~ I^2 == val4 * "%",
                      list(val1 = round(beta0, 3), val2 = round(beta1, 3), val3 = PB,  val4 = I2*100))
  p<-ggplot2::ggplot(data = data, aes(x = x1, y = yi,   shape = NoPB)) +
    geom_point(size = rel.weights, alpha = 0.5) +
    ggtitle(title) +
    labs(x =mod.title,
         y = "Effect Sizes") +
    #observed slope
    geom_abline(aes(slope = mem$beta[2,1], intercept = mem$beta[1,1], colour ="original", linetype="original"),linewidth=0.75, show.legend = TRUE) +
    #sensitivity analysis slope
    geom_abline(aes(slope = betasPB[2,1], intercept = betasPB[1,1], colour ="sensitivity", linetype="sensitivity"),linewidth=0.75, show.legend = TRUE) +

    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 17),
                       name = "No Publication Bias") +
    # Manual color scale for PB
    scale_color_manual(
      values = c("original" = "black", "sensitivity" = "#E66100"),
      name = "Analysis"
    ) +
    scale_linetype_manual(
      values = c("original" = "solid", "sensitivity" = "dashed"),
      name = "Analysis"
    ) +
    # Remove the line from the shape legend
    guides(shape = guide_legend(override.aes = list(linetype = 0, size = 1.5))) +# Set linetype = 0 (no line), size = 4 (adjust point size)
    guides(
      colour = guide_legend("Analysis",  title.position = "top" ),
      linetype = guide_legend("Analysis", title.position = "top", override.aes = list(linewidth = 0.75) ),
      shape = guide_legend(title = "No Publication Bias", title.position = "top", override.aes = list(linetype = 0,  size = 2))
    ) +
    theme(legend.position = "bottom")

  if (length(unique(data$x1))==2) {
    p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))
  }
  return(p)
}

#' @noRd
plot_contmod <- function(data, I2_PB,  beta0, beta1, I2, weights, mod.title = "Moderator value" ){
  ind <- ifelse(I2 == 0, "a", ifelse(I2 ==0.25, "b", ifelse(I2 ==0.5, "c", "d")))

  title <- substitute(bolditalic(val0) * ":"~beta[0] == val1 * "," ~ beta[1]==val2 * "," ~  "and" ~ I^2 == val3 * "%",
                      list(val0 = ind, val1 = round(beta0, 3), val2 = round(beta1, 3), val3 = I2*100))

  p<-ggplot2::ggplot(data = data, aes(x = x1, y = E,   shape = NoPB)) +
    geom_point(size = weights, alpha = 0.5) +
    ggtitle(title) +
    labs(x =mod.title,
         y = "Effect Sizes") +
    #geom_abline(data = subset(PBbiased_betas, I2 ==0.5), aes(slope = beta1PB, intercept = beta0PB, colour = PB)) +
    geom_abline(aes(slope = I2_PB$beta1PB[1], intercept = I2_PB$beta0PB[1], colour = I2_PB$PB[1], linetype=I2_PB$PB[1]),linewidth=0.75, show.legend = TRUE) +
    geom_abline(aes(slope = I2_PB$beta1PB[2], intercept = I2_PB$beta0PB[2], colour = I2_PB$PB[2], linetype=I2_PB$PB[2]),linewidth=0.75, show.legend = TRUE) +
    geom_abline(aes(slope = I2_PB$beta1PB[3], intercept = I2_PB$beta0PB[3], colour = I2_PB$PB[3], linetype=I2_PB$PB[3]),linewidth=0.75, show.legend = TRUE) +
    geom_abline(aes(slope = I2_PB$beta1PB[4], intercept = I2_PB$beta0PB[4], colour = I2_PB$PB[4], linetype=I2_PB$PB[4]),linewidth=0.75, show.legend = TRUE) +
    geom_abline(aes(slope = beta1, intercept = beta0, colour = I2_PB$PB[5],  linetype=I2_PB$PB[5]),linewidth=0.75, show.legend = TRUE) +
    geom_abline(aes(slope = I2_PB$beta1PB[6], intercept = I2_PB$beta0PB[6], colour = I2_PB$PB[6], linetype=I2_PB$PB[6]), linewidth=0.75,show.legend = TRUE) +

    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 17),
                       name = "No Publication Bias") +
    # Manual color scale for PB
    scale_color_manual(
      values = c("0" = "red", "0.05" = "#E66100", "0.2" = "#006CD1", "0.5" = "#5D3A9B", "1" = "darkgray", "original" = "black"),
      name = "Publication Bias"
    ) +
    scale_linetype_manual(
      values = c("0" = "dotted", "0.05" = "longdash", "0.2" = "dotdash", "0.5" = "dashed", "1" = "twodash", "original" = "solid"),
      name = "Publication Bias"
    ) +
    # Remove the line from the shape legend
    guides(shape = guide_legend(override.aes = list(linetype = 0, size = 1.5))) +# Set linetype = 0 (no line), size = 4 (adjust point size)
    guides(
      colour = guide_legend("Publication Bias",  title.position = "top" ),
      linetype = guide_legend("Publication Bias", title.position = "top", override.aes = list(linewidth = 0.75) ),
      shape = guide_legend(title = "No Publication Bias", title.position = "top", override.aes = list(linetype = 0,  size = 2))
    ) +
    theme(legend.position = "bottom")

  if (length(unique(data$x1))==2) {
    p <- p + scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))
  }

  return(p)
}

#' Generates Figure with 4 sub Figures and one legend
#' @export
plot_grid_1legend <- function(p1, p2, p3, p4, legend){
  g <- ggplot2::ggplotGrob(p1)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")][[1]]
  p <- cowplot::plot_grid(
    cowplot::plot_grid(p1 + ggplot2::theme(legend.position = "none"),
              p2 + ggplot2::theme(legend.position = "none"),
              p3 + ggplot2::theme(legend.position = "none"),
              p4 + ggplot2::theme(legend.position = "none"), ncol = 2),  # 2x2 grid of plots
    legend,  # Add extracted legend below
    ncol = 1,  # Arrange in one column (plots on top, legend below)
    rel_heights = c(3, 0.4)  # Adjust relative height of grid vs legend
  )
  return(p)
}

#' Generates Figure with 2 sub Figures in a row and one legend
#' @export
plot_grid_1legend_2p <- function(p1, p2, legend){
  g <- ggplot2::ggplotGrob(p1)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")][[1]]
  p <- cowplot::plot_grid(
    cowplot::plot_grid(p1 + theme(legend.position = "none"),
              p2 + theme(legend.position = "none"), ncol = 2),  #
    legend,  # Add extracted legend below
    ncol = 1,  # Arrange in one column (plots on top, legend below)
    rel_heights = c(3, 0.4)  # Adjust relative height of grid vs legend
  )
  return(p)
}
