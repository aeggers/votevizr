#' Convert counts/shares for three elements to barycentric coordinates
#'
#' These functions perform the translation from standard
#' coordinates to barycentric coordinates. 
#'
#' @param x A triple representing votes or vote shares for three 
#'     candidates. Translated to correspond to lower left, lower right,
#'     and top vertices of ternary diagram (respectively).
#'     
#' @return The x coordinate (for \code{simplex.x} and \code{simplex.y})
#'     or the x and y coordinates (for \code{simplex.xy}).
#' @name ternary
NULL
    
#' @rdname ternary
#' @export
simplex.x <- function(x){
  if(sum(x) == 0){return(.5)}
  (x[2] + 0.5*x[3])/sum(x)
}

#' @rdname ternary
#' @export
simplex.y <- function(x){
  if(sum(x) == 0){return(sqrt(.75)*(1/3))}  
  (sqrt(0.75)*x[3])/sum(x)
} 
 
#' @rdname ternary
#' @export
simplex.xy <- function(x){
  c(simplex.x(x), simplex.y(x))
}
