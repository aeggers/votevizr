#' Draw boundary of ternary diagram
#'
#' Add lines to make triangle border around ternary diagram. 
#'
#' @param ... Arguments to pass to \code{lines()}.
#'     
#' @export
add_ternary_boundary = function(...){
  bca.vertex = c(1, 0, 0)
  cba.vertex = c(0, 1, 0)
  bac.vertex = c(0, 0, 1)
  bac.v.x = simplex.x(bac.vertex); bac.v.y = simplex.y(bac.vertex)
  bca.v.x = simplex.x(bca.vertex); bca.v.y = simplex.y(bca.vertex)
  cba.v.x = simplex.x(cba.vertex); cba.v.y = simplex.y(cba.vertex) 
  
  lines(c(bac.v.x, cba.v.x), c(bac.v.y, cba.v.y), ...)
  lines(c(bca.v.x, cba.v.x), c(bca.v.y, cba.v.y), ...)
  lines(c(bac.v.x, bca.v.x), c(bac.v.y, bca.v.y), ...)
}