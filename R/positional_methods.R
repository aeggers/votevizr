#' Plot result of positional election 
#'
#' Given ballot counts/shares, represent the result of a three-candidate 
#' positional election on a ternary diagram. The dot shows the proportion of top ranks each candidate 
#' received; the division of the triangle into win regions shows 
#' the proportion of top ranks each candidate would need to win the election
#' (given the pattern of lower rankings).
#' 
#' A positional method is one where voters rank candidates and candidates are awared points depending on the rankings they receive. Given three candidates, a top ranking receives one point, a bottom ranking receives no points, and a second ranking receives \code{s} points. 
#' 
#' Special cases of positional methods are plurality (\code{s=0}), Borda count (\code{s=.5}), and anti-plurality (\code{s=1}). Each of these methods gets a specific plotting function, but in the background it just passes the appropriate  \code{positional.s} argument to the more general method \code{plot_positional_result()}.   
#' @inheritParams plot_rcv_result 
#' @param positional.s The point value (between 0 and 1) given to candidates for second rankings, where 1 is given to top rankings and 0 to bottom rankings. 0 is plurality, .5 is Borda, 1 is anti-plurality. 
#' 
#' @param draw.positional.tie.lines Draw positional tie lines for each pair of candidates. 
#' @return In addition to making the plot (if plot_it = T), the function returns a list consisting of a list of \code{positional.tie.line.endpoints} (one per pair of candidates) and the \code{intersection.point} of these tie lines. 
#' @examples
#' out <- plot_positional_result(result = c(20, 3, 8, 8, 8, 11), positional.s = 2/5)
#' add.ternary.point(out$intersection.point, col = "red", cex = .5)
#' @name plot_positional_results
NULL

#' @rdname plot_positional_results
#' @export
plot_plurality_result = function(result, ...){
  plot_positional_result(result, positional.s = 0, ...)
}

#' @rdname plot_positional_results
#' @export
plot_borda_result = function(result, ...){
  plot_positional_result(result, positional.s = .5, ...)
}

#' @rdname plot_positional_results
#' @export
plot_antiplurality_result = function(result, ...){
  plot_positional_result(result, positional.s = 1, ...)
}

#' @rdname plot_positional_results
#' @export
plot_positional_result = function(result, positional.s = .5, add.fp.result = T, fp.result.col = "black", fp.result.cex = 1, shading.cols = c("#E495A566", "#86B87566", "#7DB0DD66"), secondary.line.col = "gray", secondary.line.lwd = 2, vertex.labels = c("A", "B", "C"), main = NULL, plot_it = T, new = T, border = "black", space = .1, xlim = c(0,1), ylim = c(0, sqrt(3/4)), draw.positional.tie.lines = F){
  
  v.vec <- convert_result_to_vector_of_vote_shares(result)
  
  stopifnot(positional.s >= 0 & positional.s <= 1)
  
  # basic plot
  if(plot_it & new){
    xs = xlim + c(-space, space); ys = ylim + sqrt(3/4)*c(-space, space)
    plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main)
    add.ternary.boundary()
  }
  
  # FP vote shares
  fp.vec = c(sum(v.vec[c(1,2,7)]), sum(v.vec[c(3,4,8)]), sum(v.vec[c(5,6,9)]))
  
  # conditional proportions 
  pAB = v.vec[1]/fp.vec[1]
  pAC = v.vec[2]/fp.vec[1]
  pBA = v.vec[3]/fp.vec[2]
  pBC = v.vec[4]/fp.vec[2]
  pCA = v.vec[5]/fp.vec[3]
  pCB = v.vec[6]/fp.vec[3]
  
  tpms = ternary_point_mats_from_p_vec_and_s_v2(c(pAB, pAC, pBA, pBC, pCA, pCB), positional.s)  

    if(plot_it & draw.positional.tie.lines){
    for(j in 1:3){
      add.ternary.lines(tpms[[j]][1,], tpms[[j]][2,], col = secondary.line.col)
    }
  }
  
  # intersection: where A, B, and C all get the same score
  # we solve for A in both equations
  d1 = 1 - positional.s*(pAB - pCB + pCA) 
  s1 = (1 - positional.s*(pBA + pCB - pCA))
  i1 = positional.s*(pCB - pCA)
  
  d2 = 2 - positional.s*(pAC + pCA) 
  s2 = (positional.s*(pBC + pCA - pBA) - 1)
  i2 = (1 - positional.s*pCA)
  
  # then substitute one into the other to solve for v_B
  v_b.star = ((i1/d1)*d2 - i2)/(s2 - (d2*s1/d1))
  v_a.star = (v_b.star*s1 + i1)/d1
  intersection.point = c(v_a.star, 1 - v_b.star - v_a.star, v_b.star)
  
  out.list = list("intersection.point" = intersection.point, "positional.tie.line.endpoints" = tpms)
  if(!plot_it){return(out.list)}
  
  # vertices
  A.vertex = c(1,0,0)
  B.vertex = c(0,0,1)
  C.vertex = c(0,1,0)
  
  # winning areas 
  if(min(intersection.point) >= 0){
    add.ternary.polygon(rbind(A.vertex, tpms[[2]][1,], intersection.point, tpms[[1]][1,], A.vertex), col = shading.cols[1], border = border)
    add.ternary.polygon(rbind(B.vertex, tpms[[1]][1,], intersection.point, tpms[[3]][1,], B.vertex), col = shading.cols[2], border = border)
    add.ternary.polygon(rbind(C.vertex, tpms[[2]][1,], intersection.point, tpms[[3]][1,], C.vertex), col = shading.cols[3], border = border)
  }else if(intersection.point[3] < 0){
    add.ternary.polygon(rbind(A.vertex, tpms[[1]][1,], tpms[[1]][2,], A.vertex), col = shading.cols[1], border = border)
    add.ternary.polygon(rbind(B.vertex, tpms[[1]][1,], tpms[[1]][2,], tpms[[3]][2,], tpms[[3]][1,], B.vertex), col = shading.cols[2], border = border)
    add.ternary.polygon(rbind(C.vertex, tpms[[3]][1,], tpms[[3]][2,], C.vertex), col = shading.cols[3], border = border)
  }else if(intersection.point[1] < 0){
    add.ternary.polygon(rbind(A.vertex, tpms[[1]][1,], tpms[[1]][2,], tpms[[2]][2,],  tpms[[2]][1,], A.vertex), col = shading.cols[1], border = border)
    add.ternary.polygon(rbind(B.vertex, tpms[[1]][1,], tpms[[1]][2,], B.vertex), col = shading.cols[2], border = border)
    add.ternary.polygon(rbind(C.vertex, tpms[[2]][1,], tpms[[2]][2,], C.vertex), col = shading.cols[3], border = border)
  }else if(intersection.point[2] < 0){
    add.ternary.polygon(rbind(A.vertex, tpms[[2]][1,], tpms[[2]][2,], A.vertex), col = shading.cols[1], border = border)
    add.ternary.polygon(rbind(B.vertex, tpms[[3]][1,], tpms[[3]][2,], B.vertex), col = shading.cols[2], border = border)
    add.ternary.polygon(rbind(C.vertex, tpms[[2]][1,], tpms[[2]][2,], tpms[[3]][2,], tpms[[3]][1,], C.vertex), col = shading.cols[3], border = border)
  }
  
  
  # FP result
  if(add.fp.result){
    add.ternary.point(fp.vec[c(1,3,2)], pch = 19, cex = fp.result.cex, col = fp.result.col)
  }
  
  if(new){
    label.offset = .05
    add.ternary.text(c(1,0,0), vertex.labels[1], x.offset = -label.offset, y.offset = -sqrt(3/4)*label.offset)
    add.ternary.text(c(0, 0,1), vertex.labels[2], x.offset = 0, y.offset = sqrt(3/4)*label.offset)
    add.ternary.text(c(0,1,0), vertex.labels[3], x.offset = label.offset, y.offset = -sqrt(3/4)*label.offset)
  }
  
  out.list
  
}