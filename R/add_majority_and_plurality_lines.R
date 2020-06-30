#' Draw significant lines for majority and plurality
#'
#' Add lines to a ternary plot showing where a candidate wins a majority/plurality or ties 
#'
#' @param overhang Amount by which lines should be extended beyond edge of ternary diagram.
#'     
#' @name majority_and_plurality_lines
NULL

#' @rdname majority_and_plurality_lines
#' @export
add_majority_win_lines = function(overhang = 0, ...){
  add_ternary_lines(c(1/2, 1/2, 0), c(0, 1/2, 1/2), overhang = overhang, ...)
  add_ternary_lines(c(0, 1/2, 1/2), c(1/2, 0, 1/2), overhang = overhang, ...)
  add_ternary_lines(c(1/2, 0, 1/2), c(1/2, 1/2, 0), overhang = overhang, ...)
}

#' @rdname majority_and_plurality_lines
#' @export
add_plurality_tie_lines = function(overhang = 0, ...){
  add_ternary_lines(c(1/2, 1/2, 0), c(0, 0, 1), overhang = overhang, ...)
  add_ternary_lines(c(0, 1/2, 1/2), c(1, 0, 0), overhang = overhang, ...)
  add_ternary_lines(c(1/2, 0, 1/2), c(0, 1, 0), overhang = overhang, ...)
}

#' @rdname majority_and_plurality_lines
#' @export
add_plurality_win_lines = function(overhang = 0, ...){
  add_ternary_lines(c(1/2, 1/2, 0), rep(1, 3)/3, overhang = overhang, ...)
  add_ternary_lines(c(0, 1/2, 1/2), rep(1, 3)/3, overhang = overhang, ...)
  add_ternary_lines(c(1/2, 0, 1/2), rep(1, 3)/3, overhang = overhang, ...)
}