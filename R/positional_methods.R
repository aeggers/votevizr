
#### plurality, Borda and other positional systems ####   

from_point_in_xyz_form = function(p_vec, s){
  # we are calculating a tie between x and y
  # this is the point where there is such a tie and z is zero
  # P_vec is in form: p_xy, p_yx, p_zx
  x_star = 1/(1 + (1 - s*p_vec[1])/(1 - s*p_vec[2]))
  c(x_star, 1 - x_star, 0)
}

from_point_in_xyz_form_v2 = function(pxy, pyx, s){
  # we are calculating a tie between x and y
  # this is the point where there is such a tie and z is zero
  # P_vec is in form: p_xy, p_yx, p_zx
  x_star = (1 - s*pyx)/(2 - s*(pxy + pyx)) 
  c(x_star, 1 - x_star, 0)
}

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

point_mat_in_xyz_form_v2 = function(p_vec, s){
  rbind(from_point_in_xyz_form_v2(p_vec[1], p_vec[3], s), 
        to_point_in_xyz_form_v2(p_vec[1], p_vec[3], p_vec[5], p_vec[6], s)
  )
}

to_point_in_xyz_form = function(p_vec, s){
  # we are calculating a tie between x and y
  # these are the points where there is such a tie and x or y is zero
  # only one of these is in the simplex 
  y.star = (s*(2*p_vec[3] - 1))/(1 + s*(2*p_vec[3] - p_vec[2] - 1))
  #   y.star = (s*(1 - 2*p_vec[3]))/(1 - s*(1 + 2*p_vec[3] - p_vec[2]))
  with_x_0 = c(0, y.star, 1 - y.star)
  x.star = (-s*(2*p_vec[3] - 1))/(1 + s*(1 - 2*p_vec[3] - p_vec[1]))
  #  x.star = (s*(1 - 2*p_vec[3]))/(1 + s*(1 - 2*p_vec[3] - p_vec[1]))
  with_y_0 = c(x.star, 0, 1 - x.star)
  if(min(with_x_0) == 0){
    with_x_0
  }else{
    with_y_0
  }
}

point_mat_in_xyz_form = function(p_vec, s){
  rbind(from_point_in_xyz_form(p_vec, s), 
        to_point_in_xyz_form(p_vec, s)
  )
}

ternary_point_mats_from_p_vec_and_s_v2 = function(p_vec, s){
  ab_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec, s) # abc => xyz
  ab_point_mat = ab_point_mat_xyz[,c(1,3,2)] # standard reorganization -- the top vertex comes third. 
  ac_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec[c(2,1,5,6,3,4)], s) # acb => xyz
  ac_point_mat = ac_point_mat_xyz # already correct. 
  bc_point_mat_xyz = point_mat_in_xyz_form_v2(p_vec[c(4,3,6,5,1,2)], s) # bca => xyz
  bc_point_mat = bc_point_mat_xyz[ , c(3,2,1)] 
  list(ab_point_mat, ac_point_mat, bc_point_mat)
}

ternary_point_mats_from_p_vec_and_s = function(p_vec, s){
  ab_point_mat_xyz = point_mat_in_xyz_form(p_vec, s)
  ab_point_mat = ab_point_mat_xyz[,c(1,3,2)]
  ac_point_mat_xyz = point_mat_in_xyz_form(c(1 - p_vec[1], p_vec[3], p_vec[2]), s)
  ac_point_mat = ac_point_mat_xyz
  bc_point_mat_xyz = point_mat_in_xyz_form(c(1 - p_vec[2], 1 - p_vec[3], p_vec[1]), s)
  bc_point_mat = bc_point_mat_xyz[, c(3, 2, 1)]
  list(ab_point_mat, ac_point_mat, bc_point_mat)
}

plot.plurality.result = function(v.vec, ...){
  plot.positional.result(v.vec, positional.s = 0, ...)
}

plot.borda.result = function(v.vec, ...){
  plot.positional.result(v.vec, positional.s = .5, ...)
}

plot.antiplurality.result = function(v.vec, ...){
  plot.positional.result(v.vec, positional.s = 1, ...)
}

plot.positional.result = function(v.vec, positional.s = .5, secondary.line.col = "gray", secondary.line.lwd = 2, vertex.labels = c("A", "B", "C"), shading.cols = c("#E495A566", "#86B87566", "#7DB0DD66"), main = NULL, new = T, border = "black", fp.result.col = "black", fp.result.cex = 1, space = .1, clipped = F, add.fp.result = F, plot.pairwise.lines = F, highlight.positional.tie.lines = F, highlight.color = rgb(1,0,0,alpha = .5), highlight.lwd = 4, highlight.lend = 0, plot = T){
  
  # s is the value given to the second option on a ballot. 0 is plurality, 1/2 is Borda, 1 is anti-plurality.
  
  # basic plot
  if(plot & new){
    xs = c(0, 1) + c(-space, space); ys = c(0, sqrt(3/4)) + sqrt(3/4)*c(-space, space)
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
  # tpms = ternary_point_mats_from_p_vec_and_s(c(pAB, pBA, pCA), positional.s)
  if(plot & plot.pairwise.lines){
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
  
  out.list = list("intersection.point" = intersection.point, "tpms" = tpms)
  if(!plot){return(out.list)}
  
  # vertices
  A.vertex = c(1,0,0)
  B.vertex = c(0,0,1)
  C.vertex = c(0,1,0)
  
  # winning areas 
  if(min(intersection.point) >= 0){
    add.ternary.polygon(rbind(A.vertex, tpms[[2]][1,], intersection.point, tpms[[1]][1,], A.vertex), col = shading.cols[1], border = border)
    add.ternary.polygon(rbind(B.vertex, tpms[[1]][1,], intersection.point, tpms[[3]][1,], B.vertex), col = shading.cols[2], border = border)
    add.ternary.polygon(rbind(C.vertex, tpms[[2]][1,], intersection.point, tpms[[3]][1,], C.vertex), col = shading.cols[3], border = border)
    if(highlight.positional.tie.lines){  # not yet dealing with the other cases
      for(k in 1:3){
        add.ternary.lines(tpms[[k]][1,], intersection.point, col = highlight.color, lwd = highlight.lwd, lend = highlight.lend)
      }
    }
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