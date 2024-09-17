add_logo_old <- function(plt,
                     footer_height=.0,
                     logo_vjust = -.4, logo_hjust=.79,
                     logo_scale = 1,
                     logo_height = 0.03*logo_scale, logo_width = 0.15*logo_scale,
                     logo_y = 0, logo_x = 1,
                     logo_negative=F,
                     png = F, ...){
  library(cowplot)
  library(magick)

  file_logo <- ifelse(logo_negative, "crea_logo_negative.png",
                      ifelse(png, 'crea_logo.png', "CREA-logo-simple.svg"))
  img <- image_read(system.file("extdata", file_logo, package="rcrea"))

  # Set the canvas where you are going to draw the plot and the image
  ggdraw() +
    # Draw the plot in the canvas setting the x and y positions, which go from 0,0
    # (lower left corner) to 1,1 (upper right corner) and set the width and height of
    # the plot. It's advisable that x + width = 1 and y + height = 1, to avoid clipping
    # the plot
    draw_plot(plt,x = 0, y = footer_height, width = 1, height = (1-footer_height)) +
    # Draw image in the canvas using the same concept as for the plot. Might need to
    # play with the x, y, width and height values to obtain the desired result
    draw_image(img,x = logo_x, y = logo_y, vjust=logo_vjust, hjust=logo_hjust, width = logo_width, height = logo_height) ->
    plt
  print(plt)
  return(plt)
}

#save png with defaults, adding crea logo
quicksave_old <- function(file, plot = last_plot(), pointsize=.75, width=8, height=6, scale=1.33, bg='white',
                      logo=T, preview=T,
                      device = NULL, path = NULL, units = c("in", "cm", "mm", "px"), dpi = 300, limitsize = TRUE,
                      ...) {
  if(logo) plot <- add_logo_old(plot, ...)
  ggsave(file, plot=plot, width=width, height=height, scale=scale, bg=bg,
         device = device, path = path, units = units, dpi = dpi, limitsize = limitsize)

  if(preview) plot_image_old(file)
}

#plot an image in the plot window
plot_image_old <- function(file) {
  library(cowplot)
  library(magick)
  img <- image_read(file)

  preview_img <- ggdraw() + draw_image(img)
  print(preview_img)
}
