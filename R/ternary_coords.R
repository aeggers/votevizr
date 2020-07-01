#' Convert 3D coordinates to 2D ternary space
#'
#' Converts coordinate triples (supplied as length-3 vector, 3-column data.frame/tibble, or 3-column matrix) to two-column matrix. Coordinates assigned to lower-right, top, and lower-left, respectively. May be supplied as counts (non-normalized) or shares (normalized).
#'
#' @param x Vector, matrix, or data.frame/tibble of coordinates in 3D. 

#' @export
ternary_coords <- function(x){
  # check class of input -- seems fragile? 
  if(class(x) %in% c("integer", "numeric")){
    stopifnot(length(x) == 3)
    x <- matrix(x, nrow = 1)
  }else if(class(x) %in% c("tibble", "data.frame")){
    stopifnot(ncol(x) == 3)
    x <- as.matrix(x)
  }
  # normalize so that all coordinates are on the simplex
  x <- x/apply(x, 1, sum)
  # convert to ternary
  cbind(x[,1] + .5*x[,2], sqrt(3/4)*x[,2])
}