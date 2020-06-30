#' Draw lines and arrows in ternary space
#'
#' Pass a pair of points in standard coordinates (x, y, z) 
#' to draw lines or arrows in ternary/barycentric coordinates. 
#'
#' @param point.1,point.2 The start and end points, as triples; in the
#' election context.
#' @param overhang Amount by which line should be extended in each direction.
#' Useful for plotting gridlines that go past the edges of the simplex.
#' @param ... Other arguments to be passed to \code{lines()} or
#' \code{arrows()}.
#'     
#' @name add_ternary_lines
NULL

#' @rdname add_ternary_lines
#' @export
add_ternary_lines = function(point.1, point.2, overhang = 0, ...){
  xs = c(simplex.x(point.1), simplex.x(point.2)) 
  ys = c(simplex.y(point.1), simplex.y(point.2)) 
  if(overhang > 0){
    if(xs[1] == xs[2]){
      ys = sort(ys) + c(-overhang, overhang)
    }else if(ys[1] == ys[2]){
      xs = sort(xs) + c(-overhang, overhang)
    }else{
      delta.x = xs[2] - xs[1]
      delta.y = ys[2] - ys[1]
      delta.length = sqrt(delta.x^2 + delta.y^2)
      length.ratio = overhang/delta.length
      xs = xs + length.ratio*delta.x*c(-1,1)
      ys = ys + length.ratio*delta.y*c(-1,1)
    }
  }
  lines(x = xs, y = ys, ...)
}

#' @rdname add_ternary_lines
#' @export
add_ternary_arrow = function(point.1, point.2, length = .075, angle = 30, ...){
  arrows(x0 = simplex.x(point.1), y0 = simplex.y(point.1), x1 = simplex.x(point.2), y1 = simplex.y(point.2), length = length, angle = angle, ...)
}