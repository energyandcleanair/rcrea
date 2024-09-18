guides_crea <- function(linewidth_mapped=F){

  if(!linewidth_mapped){
    guides(
      color = guide_legend(
        override.aes = list(
          linewidth = 1,  # Thicker lines in legend
          size = 2       # Larger points in legend
        )
      )
    )
  } else{
    NULL
  }
}
