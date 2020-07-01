#' Add points to ternary diagram
#'
#' Pass a vector of coordinates in 3D space, or a matrix/data.frame/tibble of 3D coordinates \code{x}, and add points to existing ternary plot.   
#'
#' @param x The point(s) to plot, as a vector or as a three-column matrix/data.frame/tibble. 
#' @param ... Other parameters to pass to \code{points()}.
#' @export
ternary_points = function(x, ...){
  points(ternary_coords(x), ...)
}
