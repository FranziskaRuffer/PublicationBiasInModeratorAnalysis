# Plotting Functions

p_bias_exp_val <- function(data, title, size_text = 8, axis_text = 23){
  ggplot(data=data,aes(x=g, y=bias, linetype=PB)) + 
    geom_line(linewidth=1.2) +
    labs( x = expression(theta[i]) , 
          y= "Bias",
          title = title) +
    scale_linetype_manual(values = 1:5,
                          labels = c("0", "0.05", "0.2", "0.5", "1"),
                          name = "Proportion of published non-signigicant studies [PB]") +
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

plot_grid_1legend <- function(p1, p2, p3, p4, legend){
  g <- ggplotGrob(p1)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")][[1]]
  p <- plot_grid(
    plot_grid(p1 + theme(legend.position = "none"),
              p2 + theme(legend.position = "none"),
              p3 + theme(legend.position = "none"),
              p4 + theme(legend.position = "none"), ncol = 2),  # 2x2 grid of plots
    legend,  # Add extracted legend below
    ncol = 1,  # Arrange in one column (plots on top, legend below)
    rel_heights = c(3, 0.4)  # Adjust relative height of grid vs legend
  )
  return(p)
}

plot_grid_1legend_2p <- function(p1, p2, legend){
  g <- ggplotGrob(p1)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")][[1]]
  p <- plot_grid(
    plot_grid(p1 + theme(legend.position = "none"),
              p2 + theme(legend.position = "none"), ncol = 2),  # 
    legend,  # Add extracted legend below
    ncol = 1,  # Arrange in one column (plots on top, legend below)
    rel_heights = c(3, 0.4)  # Adjust relative height of grid vs legend
  )
  return(p)
}
