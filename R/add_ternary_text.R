#' Add text to ternary diagram
#'
#' Takes point in standard 3D space and adds text in ternary space.   
#'
#' @inheritParams add_ternary_point
#' @param labels Label for text.
#' @param ... Other parameters to pass to \code{text()}.

#' @export
add_ternary_text = function(point, labels, x.offset = 0, y.offset = 0, ...){
  text(x = simplex.x(point) + x.offset, y = simplex.y(point) + y.offset, labels = labels, ...)
}
