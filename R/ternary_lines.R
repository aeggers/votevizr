#' Draw lines in ternary space
#'
#' Pass a matrix of 3D coordinates \code{x} and draw a line connecting them. 
#'
#' @param x A matrix, data.frame, or tibble of 3D coordinates.
#' @param overhang Amount by which line should be extended 
#' beyond start and end points -- useful for plotting gridlines that go past the edges of the simplex.
#' @param ... Other arguments to be passed to \code{lines()}.
#'     
#' @export
ternary_lines = function(x, overhang = 0, ...){
  x <- ternary_coords(x)
  stopifnot(nrow(x) >= 2)
  if(overhang > 0){
    theta_1 <- atan((x[1,2] - x[2,2])/(x[1,1] - x[2,1]))
    n <- nrow(x)
    theta_2 <- atan((x[n,2] - x[n-1,2])/(x[n,1] - x[n-1,1]))
    x <- rbind(
      x[1,] + overhang*c(cos(theta_1), sin(theta_1)), 
      x,
      x[nrow(x),] + overhang*c(cos(theta_2), sin(theta_2)))
  }
  lines(x, ...)
}