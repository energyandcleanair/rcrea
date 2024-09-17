theme_params <- list(
  grey1 = "grey20",
  grey2 = "grey70",
  grey3 = "grey95",
  lightblue = rcrea::pal_crea[["Light.blue"]],
  darkblue = rcrea::pal_crea[["Dark.blue"]],
  fontsize1 = 15,
  fontsize2 = 12,
  fontsize3 = 10,
  fontsize4 = 8
)

theme_crea_new <- theme_minimal(base_family = "Lato", base_size = 14) +  # Adjust size & font
  theme(

    # Background and box around the chart
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "grey99", color = NA),


    # Panel border
    panel.border = element_blank(),

    # Grid lines
    panel.grid.major = element_line(color = theme_params$grey3),  # Lighter gray for major grid lines
    panel.grid.minor = element_blank(),

    # Titles
    plot.title = element_text(color = theme_params$darkblue, size = theme_params$fontsize1, face = "bold"),
    plot.subtitle = element_text(color = theme_params$grey1, size = theme_params$fontsize2, margin = margin(b = 10)),

    # Axis
    axis.title = element_text(color = theme_params$grey1, size = theme_params$fontsize3),
    axis.text = element_text(color = theme_params$grey1, size = theme_params$fontsize4),

    # Facet strips (if you're using facets)
    strip.background = element_rect(fill = theme_params$lightblue, color = NA),
    strip.text = element_text(color = "white", size = theme_params$fontsize3, margin = margin(4, 0, 4, 0)),

    # Legend
    legend.position = "top",  # Legend at the bottom
    legend.direction = "horizontal",  # Spread out in one line
    legend.box = "horizontal",
    legend.title = element_text(color = theme_params$grey1, size=theme_params$fontsize2),
    legend.text = element_text(color = theme_params$grey1, size=theme_params$fontsize2),
    legend.key = element_rect(fill = "white", color = NA),

    # Caption
    plot.caption = element_text(color = theme_params$grey1, size = theme_params$fontsize3, hjust = 0,
                                margin = margin(t = 20, r = 0, b = 0, l = 0)),

    # Box around the plot area
    plot.margin = margin(10, 10, 10, 10),  # Add some margin around the plot
    legend.margin = margin(0, 0, 0, 0),

    # Add space between facets
    strip.placement = "outside",
    panel.spacing.y = unit(0.5, "cm")  # Add vertical space between facets
  )
