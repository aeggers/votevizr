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
plot_blank_ternary_v2 <- function(xlim = c(0, 1), ylim = c(0, sqrt(3/4)), padding = .1, offset = .05, vertex_labels = c("A", "B", "C"), main = NULL, margins = c(1,1,1,1), vertex_labels_cex = 1){
  par(mar = margins)
  xs <- xlim + padding*c(-1, 1); ys = ylim + padding*sqrt(3/4)*c(-1, 1)
  plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main)
  ternary_polygon(x = diag(3))
  ternary_text(x = diag(3), labels = vertex_labels, pos = c(4,3,2), offset = offset, cex = vertex_labels_cex)
}
