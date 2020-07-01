#' Add text to ternary diagram
#'
#' Takes point coordinates or matrix/data.frame/tibble of coordinates in standard 3D space and adds text in ternary space.
#'
#' @inheritParams ternary_points
#' @param ... Other parameters to pass to \code{text()} (including \code{labels}).

#' @export
ternary_text = function(x, ...){
  text(ternary_coords(x), ...)
}
