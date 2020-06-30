#' Draw empty ternary diagram
#'
#' Draw an empty ternary diagram to be filled by other plotting commands.. 
#'
#' @param space Amount of space to add around vertices.
#' @param label.offset Distance from vertices to vertex labels. 
#' @param vertex.labels Labels for vertices, in (lower-left, top, lower-right) order.
#' @param main Label for plot.
#' @param xlim The horizontal coordinates of the bottom-left and bottom-right vertices, respectively.
#' @param ylim The vertical coordinates of the bottom vertcies and top vertex, respectively.    
#' 
#' @export
plot_blank_ternary <- function(space = .1, label.offset = .05, vertex.labels = c("A", "B", "C"), main = NULL, xlim = c(0, 1), ylim = c(0, sqrt(3/4))){
  xs = xlim + c(-space, space); ys = ylim + sqrt(3/4)*c(-space, space)
  plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main)
  add_ternary_boundary()
  add_ternary_text(c(1,0,0), labels = vertex.labels[1], x.offset = -label.offset, y.offset = -sqrt(3/4)*label.offset)
  add_ternary_text(c(0,0,1), labels = vertex.labels[2], x.offset = 0, y.offset = sqrt(3/4)*label.offset)
  add_ternary_text(c(0,1,0), labels = vertex.labels[3], x.offset = label.offset, y.offset = -sqrt(3/4)*label.offset)
}
