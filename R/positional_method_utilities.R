#' Locate endpoints of positional tie lines 
#'
#' These functions are used to plot positional election results. They locate the points where positional tie lines intersect edges of the ternary diagram.  
#'
#' @param pxy,pyx,pzx,pzy Second preference shares. e.g. \code{pxy} is the proportion of ballots that rank \code{y} second, given that they rank \code{x} first.
#' @param s The score given to second-ranked candidates in the positional method. \code{s} = 0 corresponds to plurality, \code{s}=.5 corresponds to Borda count. 
#' @param p_vec  A vector of second preference shares. If candidates are A, B, and C (plotted at lower left, top, and lower right), then \code{p_vec} is (pAB, pAC, pBA, pBC, pCA, pCB) [I think -- check this].

#' @name positional_tie_line_utils
NULL

#' @rdname positional_tie_line_utils
#' @export
from_point_in_xyz_form_v2 = function(pxy, pyx, s){
  # we are calculating a tie between x and y
  # this is the point where there is such a tie and z is zero
  # P_vec is in form: p_xy, p_yx, p_zx
  x_star = (1 - s*pyx)/(2 - s*(pxy + pyx)) 
  c(x_star, 1 - x_star, 0)
}

#' @rdname positional_tie_line_utils
#' @export
to_point_in_xyz_form_v2 = function(pxy, pyx, pzx, pzy, s){
  # we are calculating a tie between x and y
  # these are the points where there is such a tie and x or y is zero
  # only one of these is in the simplex 
  y.star = s*(pzx - pzy)/(1 - s*(pyx - pzx + pzy))
  with_x_0 = c(0, y.star, 1 - y.star)
  x.star = s*(pzy - pzx)/(1 - s*(pxy + pzx - pzy))
  with_y_0 = c(x.star, 0, 1 - x.star)
  if(min(with_x_0) == 0){
    with_x_0
  }else{
    with_y_0
  }
}

#' @rdname positional_tie_line_utils
#' @export
point_mat_in_xyz_form_v2 = function(p_vec, s){
  rbind(from_point_in_xyz_form_v2(p_vec[1], p_vec[3], s), 
        to_point_in_xyz_form_v2(p_vec[1], p_vec[3], p_vec[5], p_vec[6], s)
  )
}

#' @rdname positional_tie_line_utils
#' @export
ternary_point_mats_from_p_vec_and_s_v2 = function(p_vec, s){
  ab_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec, s) # abc => xyz
  ab_point_mat = ab_point_mat_xyz[,c(1,3,2)] # standard reorganization -- the top vertex comes third. 
  ac_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec[c(2,1,5,6,3,4)], s) # acb => xyz
  ac_point_mat = ac_point_mat_xyz # already correct. 
  bc_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec[c(4,3,6,5,1,2)], s) # bca => xyz
  bc_point_mat = bc_point_mat_xyz[ , c(3,2,1)] 
  list(ab_point_mat, ac_point_mat, bc_point_mat)
}

ternary_point_mats_from_p_vec_and_s_v3 = function(p_vec, s){
  ab_point_mat = point_mat_in_xyz_form_v2(p_vec, s) # abc => xyz
  ac_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec[c(2,1,5,6,3,4)], s) # acb => xyz
  ac_point_mat = ac_point_mat_xyz[ , c(1, 3, 2)] # converting back to abc 
  bc_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec[c(4,3,6,5,1,2)], s) # bca => xyz
  bc_point_mat = bc_point_mat_xyz[ , c(3, 1, 2)] 
  list(ab_point_mat, ac_point_mat, bc_point_mat)
}
