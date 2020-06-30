#' Add point on ternary diagram
#'
#' Takes a point in standard 3D space and plots it in ternary space.   
#'
#' @param point The point, as a triple. Can be normalized to shares or not.
#' @param x.offset,y.offset Amount by which point should be displaced horizontally/vertically.
#' @param ... Other parameters to pass to \code{points()}.

#' @export
add_ternary_point = function(point, x.offset = 0, y.offset = 0, ...){
  points(x = simplex.x(point) + x.offset, y = simplex.y(point) + y.offset, ...)
}
