#' Draw gridlines for ternary diagram
#'
#' Adds three sets of gridlines to a ternary diagram, one for each 
#' vertex.  
#'
#' @param at The levels to plot.
#' @param overhang Amount by which line should extend beyond diagram edge.
#' @param label.cex Size of text labeling levels. 

#' @export
add_ternary_gridlines <- function(at = c(.25, .5, .75), overhang = .05, label.cex = .5, ...){
  for(v in at){
    add_ternary_lines(point.1 = c(v, 0, 1 - v), point.2 = c(v, 1-v, 0), overhang = overhang, col = "gray", lty = 2, ...)
    pt = c(v, 0, 1 - v)
    text(x = simplex.x(pt) - overhang/sqrt(3), simplex.y(pt) + overhang*2/sqrt(3), labels = paste0("", v), cex = label.cex, pos = 3, ...)
    add_ternary_lines(point.1 = c(0, v, 1 - v), point.2 = c(1 - v, v, 0), overhang = overhang, col = "gray", lty = 2, ...)
    pt = c(0, v, 1-v)
    text(x = simplex.x(pt) + overhang/sqrt(3), simplex.y(pt) + overhang*2/sqrt(3), labels = paste0("", v), cex = label.cex, pos = 3, ...)
    add_ternary_lines(point.1 = c(0, 1- v, v), point.2 = c(1 - v, 0, v), overhang = overhang, col = "gray", lty = 2, ...)
    pt = c(0, 1-v, v)
    text(x = simplex.x(pt) + overhang, simplex.y(pt), labels = paste0("", v), cex = label.cex, pos = 4, ...)
  } 
}