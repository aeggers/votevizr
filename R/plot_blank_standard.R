#' Draw empty canvas in standard coordinates
#'
#' Draw an empty diagram to be filled by other plotting commands. 
#'
#' @inheritParams plot_blank_ternary_v2
#' @param label.offset Distance from vertices to vertex labels. 
#' @param vertex.labels Labels for vertices, in (lower-left, top, lower-right) order.
#' @param main Label for plot.
#' @param xlim The horizontal coordinates of the bottom-left and bottom-right vertices, respectively.
#' @param ylim The vertical coordinates of the bottom vertcies and top vertex, respectively.    
#' 
#' @export
plot_blank_standard <- function(xlim = c(0, 1), ylim = c(0, 1), padding = .1, offset = .05, vertex_labels = c("A", "B", "C"), main = NULL, margins = c(2,2,1,1), vertex_labels_cex = 1){
  par(mar = margins)
  plot(x = xlim + padding*c(-1,1), y = ylim + padding*c(-1,1), type = "n", axes = F, xlab = "", ylab = "", main = main)
  axis(1, pos = 0)
  axis(2, pos = 0)
  mtext(side = 1, vertex_labels[1], cex = vertex_labels_cex)
  mtext(side = 2, vertex_labels[2], cex = vertex_labels_cex)
}
