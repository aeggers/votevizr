#' Functions to support plotting Condorcet methods 
#'
#' These functions are used by \code{plot.condorcet.result()}. 
#' @param v.vec A vector of ballot counts or shares of length 9. If candidates 
#'     are A, B, and C, then the components indicate the count/share
#'     of ballots ranking the candidates (AB, AC, BA, BC, CA, CB, A, B, C). 
#' @param point A length-three vector of shares. 

#' @return \code{get_kemeny_point()} returns the vector of first-preference location (in standard 3D coordinates) such that, given the pattern of lower rankings, there would be a three-way tie according to the Kemeny-Young method. That is, there is a Condorcet cycle such that each candidate loses to one other candidate by the same margin. Extracting this point allows us to draw the result using Kemeny-Young method. 
#' 
#' \code{a.beats.b.at()} returns a logical.  
#' @name condorcet_method_utilities
NULL

#' @rdname condorcet_method_utilities
#' @export
get_kemeny_point = function(v.vec){
  
  v_a = sum(v.vec[c(1:2, 7)])
  v_b = sum(v.vec[c(3:4, 8)])
  v_c = sum(v.vec[c(5:6, 9)])
  pac = v.vec[2]/v_a
  pab = v.vec[1]/v_a
  pba = v.vec[3]/v_b
  pbc = v.vec[4]/v_b
  pca = v.vec[5]/v_c
  pcb = v.vec[6]/v_c
  
  # intercept and slope of the ab.bc kemeny line
  I = (-1 + pcb - pca)/((pac - pab) + (pcb - pca))
  M = (3 - pcb + pca)/((pac - pab) + (pcb - pca))
  # equation: v_a = I + v_b M
  
  # attributes of AC.AB Kemeny tie line 
  X = (3 + pcb - pca)
  Y = (3 + pbc - pba)
  # equation: Xv_c + Y v_b = 2
  
  # point of intersection 
  v_b_star = ((X-2)/X - I)/(M - (Y-X)/X)
  v_a_star = I + v_b_star*M
  c(v_a_star, 1 - v_a_star - v_b_star, v_b_star)
}

#' @rdname condorcet_method_utilities
#' @export
a.beats.b.at = function(v.vec, point){
  # Does a beat b at the given fp point, given the pattern of preferences in v_vec? If so (and if this is a cycle), this is a forward cycle.
  point[1] + point[2]*(v.vec[5]/sum(v.vec[c(5,6,9)])) > .5
}