# Set custom `ggplot2` theme 1
theme_1 <- function(..., base_size = 14) {
  ggplot2::theme(
    
    # drop minor grid lines
    panel.grid.minor = element_blank(),
    
    # change major grid lines
    panel.grid.major =  element_line(color = "#d0d0d0"),
    
    # fill the plot and panel spaces with grey and remove border
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    
    # change legend background
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
    
    # remove strip background
    strip.background = element_blank(),
    
    # adjust the margins of plots and remove axis ticks
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    axis.ticks = element_blank(),
    
    # change the color of axis lines
    axis.line = element_line(color = "#272822"),
    
    # change text family, size, and adjust position of titles and legend
    text = element_text(family = "Syne", size = base_size),
    axis.text = element_text(color = "#3D3E38", size = base_size),
    axis.title = element_text(color = "#272822", size = rel(1.20)),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle = 90),
    legend.text = element_text(size = rel(1), color = "#272822"),
    legend.title = element_text(size = rel(1), color = "#272822"),
    # plot.title = element_text(color = "#272822", family = "Syne", size = rel(1.80), hjust = 0),
    # plot.title.position = "plot",
    # plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
    # plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
    # strip.text = element_text(color = "#272822", size = rel(1.33), face = "bold"),
    # plot.title.position = "plot",
    ...
  )
}