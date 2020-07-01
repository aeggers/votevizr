#' Add polygon to ternary diagram
#'
#' Takes three-column matrix/data.frame/tibble of points in standard 3D space and plots the corresponding polygon in ternary space.   
#'
#' @param x A matrix/data.frame/tibble of points: three columns, one row per point.
#' @param ... Arguments to pass to \code{polygon()}. 

#' @export
ternary_polygon = function(x, ...){
  polygon(ternary_coords(x), ...)
}
