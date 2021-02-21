# Creating a standard plot theme to be used across my website 
my_theme <- function() {
  theme_bw()
}

# Creating a color palette to use this way if I change my hugo theme I can change all my plots 


#' My colors 
#' 
#' When given a color names, passes my version of that color 
#'
#' @param ... color or vector of colors 
#'
my_colors <- function(...){
  my_colors <- c(
    `red`        = "#E84C4F",
    `green`      = "#949619",
    `blue`       = "#379AAF",
    `orange`     = "#F66456",
    `yellow`     = "#DBB129",
    `light brown` = "#B36642",
    `black`  = "#233047")
  cols <- c(...)
  if(is.null(cols))
    return(my_colors)
  my_colors[cols]
}

#' Palette constructor 
#'
#' @param palette name of palette to use, default main 
#' @param ... Additional arguments to pass to colorRampPalette()
#'
my_palette <- function(palette = "main", ...){
  pal_ls <- list(
    `main`  = my_colors("red","yellow", "green", "blue"),
    
    `cool`  = my_colors("blue", "green"),
    
    `hot`   = my_colors("yellow", "orange", "red"),
    
    `mixed` = my_colors( "green", "blue",  "red", "yellow", "light brown"),
    
    `grey`  = my_colors("light grey", "dark grey")
  )
  pal <- pal_ls[[palette]]
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for my colors
#'
#' @param palette Character name of palette in my_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_blog <- function(palette = "main", discrete = TRUE, ...) {
  pal <- my_palette(palette = palette)
  
  if (discrete) {
    discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_blog <- function(palette = "main", discrete = TRUE, ...) {
  pal <- my_palette(palette = palette)
  
  if (discrete) {
    discrete_scale("fill", paste0("my_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
