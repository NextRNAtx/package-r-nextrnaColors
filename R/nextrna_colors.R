#' selected nextrna-themed colors
nextrna_colors <- c(
  `purple`        = "#662F8E",
  `orange`      = "#F69126",
  `teal`       = "#0d8273",
  `blue`     = "#0656c2",
  `red`     = "#800109",
  `yellow` = "#ffc425",
  `pink`  = "#d60d89")

#' Function to extract nextrna colors as hex codes
#'
#' @param ... Character names of nextrna_colors
#'
nextrna_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (nextrna_colors)

  nextrna_colors[cols]
}

#' possible nextrna color palettes
nextrna_palettes <- list(
  `main`  = nextrna_cols("purple", "orange", "teal"),
  `cool`  = nextrna_cols("purple", "teal", "blue"),
  `hot`   = nextrna_cols("yellow", "orange", "red"),
  `mixed` = nextrna_cols("purple", "teal", "yellow", "orange", "red")
)

#' Return function to interpolate a nextrna color palette
#'
#' @param palette Character name of palette in nextrna_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#
nextrna_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- nextrna_palettes[[palette]]
  if (reverse) pal <- rev(pal)
    colorRampPalette(pal, ...)
}

#' Color scale constructor for nextrna colors
#'
#' @param palette Character name of palette in nextrna_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_nextrna <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- nextrna_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("nextrna_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for nextrna colors
#'
#' @param palette Character name of palette in nextrna_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_nextrna <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- nextrna_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("nextrna_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
