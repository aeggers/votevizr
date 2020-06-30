#' Add polygon to ternary diagram
#'
#' Takes matrix of points in standard 3D space (three columns) and plots the corresponding polygon in ternary space.   
#'
#' @param point.mat A matrix of points: three columns, one row per point.
#' @param border Color of border, or NA to omit border.
#' @param border.lwd Linewidth of border.
#' @param col Color to fill polygon. 

#' @export
add_ternary_polygon = function(point.mat, border = NULL, border.lwd = 1, col = NA){
  xs = apply(point.mat, 1, simplex.x)
  ys = apply(point.mat, 1, simplex.y)
  polygon(xs, ys, border = border, col = col, lwd = border.lwd)
}
