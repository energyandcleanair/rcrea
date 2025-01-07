# Function to save a plot with an optional logo and preview
#' Title
#'
#' @param file The file path to save the plot to
#' @param plot The plot to save (default is the last plot created)
#' @param width Width of the image (in inches)
#' @param height Height of the image (in inches)
#' @param scale Scale factor for the plot given to ggsave
#' @param bg Background color of the image
#' @param logo Add a logo to the image
#' @param preview Show a preview of the image after saving
#' @param logo_scale Size of the logo relative to the plot height
#' @param logo_position tr, tl, br, bl (top right etc.)
#' @param logo_placement_margin Margin around logo, for its placement only (not moving anything in the plot)
#' @param increase_plot_margin_around_logo Add margin in the plot around logo. Useful if logo overlaps with plot.
#'  Expressed as the fraction of the margin that would be necessarily if there was no white space whatsoever.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
quicksave <- function(file,
                      plot = last_plot(),
                      width = 10,
                      height = 7,
                      scale = 1,
                      bg = 'white',
                      logo = TRUE,
                      preview = TRUE,
                      preview_use_plot = FALSE,
                      prewiew_resize = FALSE,
                      logo_scale = 0.035,
                      logo_position = "br",
                      logo_placement_margin = 0.01,
                      increase_plot_margin_around_logo = 0,
                      dpi = 300,
                      ...) {

  # To prevent small texts
  showtext::showtext_opts(dpi = dpi)

  # Modify the plot to include extra space for the logo if logo = TRUE
  if (logo & (increase_plot_margin_around_logo > 0)) {
    # Adjust the plot margin based on the logo position using a dedicated function
    plot <- adjust_plot_margin(plot, height, scale, logo_scale, logo_position, multiplier = increase_plot_margin_around_logo)
  }

  # Save the plot
  ggsave(file, plot = plot, width = width, height = height, scale = scale, bg = bg, dpi=dpi, ...)

  # Add logo after saving
  if (logo) {
    add_logo(file, logo_scale = logo_scale, logo_position = logo_position, logo_margin = logo_placement_margin)
  }

  # Optional: preview the image
  if (preview) {
    plot_image(file,
               use_plot = preview_use_plot,
               resize = prewiew_resize,
               dpi = dpi)
  }
}


adjust_plot_margin <- function(plot, height, scale, logo_scale, logo_position, multiplier=1) {

    if (ggplot2::is.ggplot(plot)) {
    # Convert inches to points (1 inch = 72.27 points)
    plot_height_in <- height * scale
    plot_height_pt <- plot_height_in * 72.27
    logo_height_pt <- logo_scale * plot_height_pt

    # Get existing plot margins from the plot's theme
    current_theme <- plot$theme
    if (!is.null(current_theme$plot.margin)) {
      current_margins <- current_theme$plot.margin
    } else {
      # Use default margins if not set
      current_margins <- ggplot2::theme_get()$plot.margin
      if (is.null(current_margins)) {
        current_margins <- unit(c(5.5, 5.5, 5.5, 5.5), "pt")
      }
    }

    # Convert margins to numeric values in points
    current_margins_numeric <- grid::convertUnit(current_margins, "pt", valueOnly = TRUE)

    # Adjust the plot margin based on the logo position
    if (logo_position %in% c("bl", "br")) {
      current_margins_numeric[3] <- current_margins_numeric[3] + logo_height_pt
    } else if (logo_position %in% c("tl", "tr")) {
      current_margins_numeric[1] <- current_margins_numeric[1] + logo_height_pt
    }

    # Set the new margins
    plot <- plot + theme(plot.margin = unit(current_margins_numeric * multiplier, "pt"))
  } else {
    # Do nothing if plot is not a ggplot object
  }
  return(plot)
}


# Function to add a high-resolution logo to an image
add_logo <- function(file,logo_scale = 0.15, logo_position = "br",
                     logo_negative = FALSE, png = FALSE, density = 300, logo_margin = 0.01, ...) {
  # Choose the logo file
  file_logo <- ifelse(logo_negative, "crea_logo_negative.png",
                      ifelse(png, 'crea_logo.png', "CREA-logo-simple.svg"))

  # Read the plot image
  img_plot <- image_read(file)

  # Get the path to the logo file
  logo_path <- system.file("extdata", file_logo, package = "rcrea")

  # Check if the logo file exists
  if (logo_path == "") {
    stop("Logo file not found in the 'extdata' directory of the 'rcrea' package.")
  }

  # Format the density parameter as "300x300"
  density_value <- paste0(density, "x", density)

  # Read the SVG logo with increased density for higher resolution
  if (tolower(tools::file_ext(logo_path)) == "svg") {
    img_logo <- image_read(logo_path, density = density_value)
  } else {
    img_logo <- image_read(logo_path)
  }

  # Get dimensions and aspect ratio
  plot_info <- image_info(img_plot)
  logo_info <- image_info(img_logo)
  logo_aspect_ratio <- logo_info$width / logo_info$height

  # Calculate logo dimensions proportional to the plot size
  logo_height_px <- logo_scale * plot_info$height
  logo_width_px <- logo_height_px * logo_aspect_ratio

  # Resize the logo
  img_logo <- image_scale(img_logo, paste0(logo_width_px, "x", logo_height_px))

  # Determine the offset based on the logo position
  margin_px <- logo_margin * plot_info$width  # Margin around the logo

  position_coords <- switch(logo_position,
                            "tl" = c(margin_px, margin_px),
                            "tr" = c(plot_info$width - logo_width_px - margin_px, margin_px),
                            "bl" = c(margin_px, plot_info$height - logo_height_px - margin_px),
                            "br" = c(plot_info$width - logo_width_px - margin_px, plot_info$height - logo_height_px - margin_px),
                            {
                              warning("Invalid logo_position specified. Defaulting to 'br' (bottom-right).")
                              c(plot_info$width - logo_width_px - margin_px, plot_info$height - logo_height_px - margin_px)
                            })

  # Add the logo to the specified position
  final_img <- image_composite(img_plot, img_logo,
                               offset = paste0("+", round(position_coords[1]), "+", round(position_coords[2])))

  # Overwrite the file with the logo added
  image_write(final_img, path = file)
}

# Function to preview the image
#'
#' @param file filepath of the image
#' @param use_plot use the plot panel of Rstudio rather than Viewer
#' @param resize When using viewer, reduces the size of the image to fit the viewer
#'
#' @return
#' @export
#'
#' @examples
plot_image <- function(file, use_plot=F, resize=F, dpi=300) {

  img <- image_read(file)

  if (use_plot) {
    # Display the image in the plot panel of RStudio
    # using grid raster
    raster_img <- as.raster(img)
    grid::grid.raster(raster_img)
  } else {

    if(resize){
      # By default, magick considers this is density 72
      # Get height
      height <- as.numeric(image_info(img)$height)
      density_x <- image_info(img)$density
      density <- as.numeric(stringr::str_split(density_x, "x")[[1]][1])
      new_height <- height * density / dpi
      img <- image_scale(img, paste0("x", new_height))
    }
    # Display the image in the RStudio Viewer
    print(img)
  }
}
