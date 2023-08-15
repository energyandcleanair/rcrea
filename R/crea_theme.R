theme_crea <- function(base_size=11,
                       family='OpenSans',
                       ...) {
  (ggthemes::theme_calc(base_size=base_size) +
     ggplot2::theme(#title = element_text(family='SourceSansPro'),
       plot.title = element_text(size=rel(1.5), face='bold', color=unname(pal_crea['Dark.blue'])),
       plot.subtitle = element_text(color='#060606'),
       plot.caption = element_text(#face='italic',
                                   color=pal_crea['Dark.blue'],
                                   hjust = 0,
                                   lineheight = 1.1),
       strip.background = element_rect(fill=pal_crea['Blue'],
                                       linetype=0),
       plot.background = element_rect(color='white', fill='white'),
       # text = element_text(family=family),
       panel.grid.major.y = element_line(size=0.1),
       panel.border = element_rect(colour="white"),
       axis.line = element_line(size=0.1),
       ...))
}


theme_crea_old <- function(base_size=11, ...) {
  (ggthemes::theme_calc(base_size=base_size) +
     ggplot2::theme(#title = element_text(family='SourceSansPro'),
       plot.title = element_text(size=rel(1.5), face='bold', color=unname(pal_crea['Dark.blue'])),
       plot.subtitle = element_text(face='italic', color='black'),
       plot.caption = element_text(face='italic', color=pal_crea['Dark.blue']),
       strip.background = element_rect(fill=pal_crea['Blue'],
                                       linetype=0),
       plot.background = element_rect(color='white', fill='white'),
       ...))
}


getpal <- function(x) {
  x %<>% textConnection %>% readLines
  x %<>% trimws()
  x %>% gsub('.*#', '#', .) -> pal
  x %>% gsub(' #.*', '', .) %>% make.names -> names(pal)
  return(pal)
}

pal_crea <-
  "Dark blue #35416C
  Blue #8cc9D0
  Light blue #cce7eb
  Turquoise #27a59c
  Green #75b44c
  Dark gray #333333
  Light gray #cacaca
  Yellow #fff2cc
  Orange #f6b26b
  Red #cc0000
  Dark red #990000
  Black #000000
  Dark violet #351c75
  Dark purple #741b47" %>% getpal

pal_crea.change <-
  "Dark blue #35416C
Blue #8cc9D0
Light blue #cce7eb
Yellow #fff2cc
Orange #f6b26b
Red #cc0000
Dark red #990000" %>% getpal

 pal_crea.heatmap <-
  "Black #000000
Dark violet #351c75
Dark purple #741b47
Dark red #990000
Red #cc0000
Orange #f6b26b
Yellow #fff2cc" %>% getpal

pal_crea.dramatic <- pal_crea.heatmap[c(4,2,3,1,5,6,7)]

pal_crea.electricity <- c(
  "Solar"= pal_crea["Yellow"],
  "Wind"= pal_crea["Blue"],
  "Other Renewables"= pal_crea["Light.blue"],
  "Hydro"= pal_crea["Dark.blue"],
  "Biomass"= pal_crea["Greem"],
  "Nuclear"= pal_crea["Dark.red"],
  "Fossil Gas"= pal_crea["Light.gray"],
  "Coal"=pal_crea["Dark.gray"]
)

crea_palettes <- list(CREA = pal_crea,
                      change = pal_crea.change,
                      heatmap = pal_crea.heatmap,
                      dramatic = pal_crea.dramatic,
                      electricity = pal_crea.electricity)

getcols <- function(pal, alpha=1, col.index=T, darken=0) {
  cols <- paste0(crea_palettes[[pal]][col.index],
         format(as.hexmode(round(alpha*255, 0)), width=2))

  if(darken > 0) cols <- colorspace::darken(cols, amount=darken)
  if(darken < 0) cols <- colorspace::lighten(cols, amount=-darken)
  return(cols)
}

makepal <- function(pal, alpha=1, col.index=T, darken=0) {
  cols <- getcols(pal=pal, alpha=alpha, col.index=col.index, darken=darken)
  scales::manual_pal(unname(cols))
}

makegrad <- function(pal, alpha=1, bias=1, reverse.order=F, col.index=T, darken=0) {
  cols <- getcols(pal=pal, alpha=alpha, col.index=col.index, darken=darken)
  if(reverse.order) cols <- rev(cols)
  function(x) { scales::col_numeric(unname(cols), c(0,1))(x^bias) }
}


scale_color_crea_d <- function(palette = "CREA", alpha = 1, col.index=T, ...) {
  discrete_scale("colour", palette, makepal(palette, alpha, col.index), ...)
}

scale_fill_crea_d <- function(palette = "CREA", alpha = 1, col.index=T,...) {
  discrete_scale("fill", palette, makepal(palette, alpha, col.index), ...)
}

scale_color_crea_c <- function(palette = "CREA", alpha = 1, reverse.order=F, bias=1, ...) {
  continuous_scale("colour", palette, makegrad(palette, alpha=alpha, reverse.order=reverse.order, bias=bias), ...)
}

scale_fill_crea_c <- function(palette = "CREA", alpha = 1, reverse.order=F, bias=1, ...) {
  continuous_scale("fill", palette, makegrad(palette, alpha=alpha, reverse.order=reverse.order, bias=bias), ...)
}

scale_y_crea_zero <- function(mult_high=0.1){
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult=c(0, mult_high)))
}

# Specific scales ---------------------------------------------------------
scale_fill_electricity <- scale_fill_manual(values=pal_crea.electricity)
scale_color_electricity <- scale_color_manual(values=pal_crea.electricity)

crea.theme <- function(colors='CREA', reverse.order=F) {
  pars = standard.theme(color = FALSE)
  pars$strip.background$col <- pal_crea['Blue']
  pars$par.main.text$col <- pal_crea['Dark.blue']
  pars$par.main.text$cex <- 1.5
  cols <- colorRampPalette(crea_palettes[[colors]])(100)
  if(reverse.order) cols %<>% rev
  pars$regions$col <- cols
  return(pars)
}

#add CREA logo to plot
add_logo <- function(plt,
                     footer_height=.0,
                     logo_vjust = -.4, logo_hjust=.79,
                     logo_scale = 1,
                     logo_height = 0.03*logo_scale, logo_width = 0.15*logo_scale,
                     logo_y = 0, logo_x = 1,
                     logo_negative=F, ...){
  require(cowplot)
  require(magick)

  file_logo <- ifelse(logo_negative, "crea_logo_negative.png", "crea_logo.svg")
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
quicksave <- function(file, plot = last_plot(), pointsize=.75, width=8, height=6, scale=1.33, bg='white',
                      logo=T, preview=T, logo_negative=F,
                      device = NULL, path = NULL, units = c("in", "cm", "mm", "px"), dpi = 300, limitsize = TRUE,
                      ...) {
  if(logo) plot <- add_logo(plot, ...)
  ggsave(file, plot=plot, width=width, height=height, scale=scale, bg=bg,
         device = device, path = path, units = units, dpi = dpi, limitsize = limitsize)

  if(preview) plot_image(file)
}

#plot an image in the plot window
plot_image <- function(file) {
  require(cowplot)
  require(magick)
  img <- image_read(file)

  preview_img <- ggdraw() + draw_image(img)
  print(preview_img)
}

# attach(CREAtheme)

c('ribbon', 'col', 'area', 'bar') %>% lapply(ggplot2::update_geom_defaults, list(fill = pal_crea[2])) -> t1
c('line', 'point') %>% lapply(ggplot2::update_geom_defaults, list(color = pal_crea[1])) -> t1
rm(t1)
rm(getpal)

# print('CREA theme loaded')
