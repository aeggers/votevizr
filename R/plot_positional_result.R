#' Plot result of positional election 
#'
#' Given ballot counts/shares, represent the result of a three-candidate 
#' positional election on a ternary diagram. The dot shows the proportion of top ranks each candidate 
#' received; the division of the triangle into win regions shows 
#' the proportion of top ranks each candidate would need to win the election
#' (given the pattern of lower rankings).
#' 
#' A positional method is one where voters rank candidates and candidates are awarded points depending on the rankings they receive. Given three candidates, a top ranking receives one point, a bottom ranking receives no points, and a second ranking receives \code{s} points. 
#' 
#' Special cases of positional methods are plurality (\code{s=0}), Borda count (\code{s=.5}), and anti-plurality (\code{s=1}). Each of these methods gets a specific plotting function, but in the background it just passes the appropriate  \code{s} argument to the more general method \code{plot_positional_result()}.   
#' @inheritParams plot_rcv_result 
#' @param s The score (between 0 and 1) given to candidates for second rankings, where 1 is given to top rankings and 0 to bottom rankings. 0 is plurality, .5 is Borda, 1 is anti-plurality. 
#' 
#' @param draw_positional_tie_lines Draw positional tie lines for each pair of candidates. 
#' @return In addition to making the plot (if suppress_plot = F), the function returns a list consisting of a list of \code{positional_tie_line_endpoints} (one per pair of candidates) and the \code{intersection_point} of these tie lines. 
#' @examples
#' out <- plot_positional_result_v2(result = c(20, 3, 8, 8, 8, 11), s = 2/5)
#' ternary_points(out$intersection_point, col = "red", cex = .5)
#' @name plot_positional_result
NULL

#' @rdname plot_positional_result
#' @export
plot_plurality_result_v2 = function(result, ...){
  plot_positional_result_v2(result, s = 0, ...)
}

#' @rdname plot_positional_result
#' @export
plot_borda_result_v2 = function(result, ...){
  plot_positional_result_v2(result, s = .5, ...)
}

#' @rdname plot_positional_result
#' @export
plot_antiplurality_result_v2 = function(result, ...){
  plot_positional_result_v2(result, s = 1, ...)
}

#' @rdname plot_positional_result
#' @export
plot_positional_result_v2 = function(result, s = .5, show_first_pref_result = T, fp_result_col = "black", fp_result_cex = 1, shading_cols = c("#E5F5E099", "#A1D99B99", "#31A35499"), vertex_labels = c("A", "B", "C"), vertex_labels_cex = 1, offset = .1, margins = c(1,1,1,1), main = NULL, suppress_plot = F, new = T, fp_win_region_border = "darkgray", padding = .1, xlim = c(0,1), ylim = c(0, sqrt(3/4)), draw_positional_tie_lines = F, tie_line_col = "gray"){
  
  v_vec <- convert_result_to_vector_of_vote_shares(result)
  
  stopifnot(s >= 0 & s <= 1)
  
  # FP vote shares
  fp_vec = c(sum(v_vec[c(1,2,7)]), sum(v_vec[c(3,4,8)]), sum(v_vec[c(5,6,9)]))
  
  # conditional proportions 
  pAB = v_vec[1]/fp_vec[1]
  pAC = v_vec[2]/fp_vec[1]
  pBA = v_vec[3]/fp_vec[2]
  pBC = v_vec[4]/fp_vec[2]
  pCA = v_vec[5]/fp_vec[3]
  pCB = v_vec[6]/fp_vec[3]
  
  tpms = ternary_point_mats_from_p_vec_and_s_v3(c(pAB, pAC, pBA, pBC, pCA, pCB), s)  
  
  # intersection: where A, B, and C all get the same score
  # we solve for A in both equations
  d1 = 1 - s*(pAB - pCB + pCA) 
  s1 = (1 - s*(pBA + pCB - pCA))
  i1 = s*(pCB - pCA)
  
  d2 = 2 - s*(pAC + pCA) 
  s2 = (s*(pBC + pCA - pBA) - 1)
  i2 = (1 - s*pCA)
  
  # then substitute one into the other to solve for v_B
  v_b.star = ((i1/d1)*d2 - i2)/(s2 - (d2*s1/d1))
  v_a.star = (v_b.star*s1 + i1)/d1
  intersection.point = c(v_a.star, v_b.star, 1 - v_b.star - v_a.star)
  
  out.list = list("intersection_point" = intersection.point, "positional_tie_line_endpoints" = tpms)
  if(suppress_plot){return(out.list)}
  
  # basic plot
  if(new){
    plot_blank_ternary_v2(xlim = xlim, ylim = ylim, padding = padding, offset = offset, vertex_labels = vertex_labels, main = main, margins = margins, vertex_labels_cex = vertex_labels_cex)
  }
  
  if(draw_positional_tie_lines){
    for(j in 1:3){
      ternary_lines(x = tpms[[j]], col = tie_line_col)
    }
  }
  
  # vertices
  A.vertex = c(1,0,0)
  B.vertex = c(0,1,0)
  C.vertex = c(0,0,1)
  
  # winning areas 
  if(min(intersection.point) >= 0){
    ternary_polygon(rbind(A.vertex, tpms[[2]][1,], intersection.point, tpms[[1]][1,], A.vertex), col = shading_cols[1], border = fp_win_region_border)
    ternary_polygon(rbind(B.vertex, tpms[[1]][1,], intersection.point, tpms[[3]][1,], B.vertex), col = shading_cols[2], border = fp_win_region_border)
    ternary_polygon(rbind(C.vertex, tpms[[2]][1,], intersection.point, tpms[[3]][1,], C.vertex), col = shading_cols[3], border = fp_win_region_border)
  }else if(intersection.point[2] < 0){
    ternary_polygon(rbind(A.vertex, tpms[[1]][1,], tpms[[1]][2,], A.vertex), col = shading_cols[1], border = fp_win_region_border)
    ternary_polygon(rbind(B.vertex, tpms[[1]][1,], tpms[[1]][2,], tpms[[3]][2,], tpms[[3]][1,], B.vertex), col = shading_cols[2], border = fp_win_region_border)
    ternary_polygon(rbind(C.vertex, tpms[[3]][1,], tpms[[3]][2,], C.vertex), col = shading_cols[3], border = fp_win_region_border)
  }else if(intersection.point[1] < 0){
    ternary_polygon(rbind(A.vertex, tpms[[1]][1,], tpms[[1]][2,], tpms[[2]][2,],  tpms[[2]][1,], A.vertex), col = shading_cols[1], border = fp_win_region_border)
    ternary_polygon(rbind(B.vertex, tpms[[1]][1,], tpms[[1]][2,], B.vertex), col = shading_cols[2], border = fp_win_region_border)
    ternary_polygon(rbind(C.vertex, tpms[[2]][1,], tpms[[2]][2,], C.vertex), col = shading_cols[3], border = fp_win_region_border)
  }else if(intersection.point[3] < 0){
    ternary_polygon(rbind(A.vertex, tpms[[2]][1,], tpms[[2]][2,], A.vertex), col = shading_cols[1], border = fp_win_region_border)
    ternary_polygon(rbind(B.vertex, tpms[[3]][1,], tpms[[3]][2,], B.vertex), col = shading_cols[2], border = fp_win_region_border)
    ternary_polygon(rbind(C.vertex, tpms[[2]][1,], tpms[[2]][2,], tpms[[3]][2,], tpms[[3]][1,], C.vertex), col = shading_cols[3], border = fp_win_region_border)
  }
  
  # FP result
  if(show_first_pref_result){
    ternary_points(fp_vec, pch = 19, cex = fp_result_cex, col = fp_result_col)
  }
  
  out.list
  
}