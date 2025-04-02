theme_crea_new <- function(
    base_family = "Source Sans Pro",
    fontsize1 = 16,
    fontsize2 = 12,
    fontsize3 = 10,
    fontsize4 = 8,
    grey1 = "grey20",
    grey2 = "grey70",
    grey3 = "grey95",
    strip_background = NA, # pal_crea[["White"]],
    strip_color = pal_crea[["Dark.blue"]],
    title_color = pal_crea[["Dark.blue"]],
    strip_align = "left",
    linewidth_mapped = F) {


  # Install font and set up showtext
  # Install font (takes less time from the second time onward)
  try(sysfonts::font_add_google(base_family, regular = "400", bold = "600"))
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)

  ggplot2::theme_minimal(base_family = base_family, base_size = fontsize2) + # Adjust size & font
    ggplot2::theme(

      # Background and box around the chart
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),


      # Panel border
      panel.border = ggplot2::element_rect(color = grey3, fill = NA, linewidth = 1),

      # Grid lines
      panel.grid.major = ggplot2::element_line(color = grey3, linewidth = 0.4), # Lighter gray for major grid lines
      panel.grid.minor = ggplot2::element_blank(),

      # Titles
      plot.title = ggplot2::element_text(color = title_color, size = fontsize1, face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(color = grey1, size = fontsize2, margin = ggplot2::margin(b = 15)),

      # Axis
      axis.title = ggplot2::element_text(color = grey1, size = fontsize3),
      axis.text = ggplot2::element_text(color = grey1, size = fontsize3),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),

      # Facet strips (if you're using facets)
      strip.background = ggplot2::element_rect(fill = strip_background, color = NA),
      strip.text = ggplot2::element_text(
        color = strip_color,
        size = fontsize2,
        margin = ggplot2::margin(4, 0, 4, 0),
        face = "bold",
        hjust = ifelse(strip_align == "center", 0.5, 0)
      ),

      # Legend
      legend.position = "bottom", # Legend at the bottom
      legend.direction = "horizontal", # Spread out in one line
      legend.box = "horizontal",
      legend.title = ggplot2::element_text(color = grey1, size = fontsize2),
      legend.text = ggplot2::element_text(color = grey1, size = fontsize2),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),

      # Caption
      plot.caption = ggplot2::element_text(
        color = grey1, size = fontsize2, hjust = 0,
        margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)
      ),
      plot.caption.position = "plot",

      # Box around the plot area
      plot.margin = ggplot2::margin(10, 10, 10, 10), # Add some margin around the plot
      legend.margin = ggplot2::margin(0, 0, 0, 0),

      # Add space between facets
      strip.placement = "outside",
      panel.spacing = ggplot2::unit(0.5, "cm") # Add vertical space between facets
    )
}
