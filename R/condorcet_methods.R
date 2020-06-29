### condorcet 

plot.condorcet.result = function(v.vec, from.v.vec = NULL, secondary.line.col = "gray", secondary.line.lwd = 2, vertex.labels = c("A", "B", "C"), shading.cols = rainbow_hcl(3, alpha = .4), main = NULL, new = T, border = "black", fp.result.col = "black", fp.result.cex = 1, space = .1, clipped = F, clipped.xs = c(1/5, 3/5), clipped.ys = c(1/4, 1/2), add.fp.result = F, plot.pairwise.lines = F, in.cycle = "empty", kemeny.point = F){
  
  # basic plot
  if(new){
    xs = c(0, 1) + c(-space, space); ys = c(0, sqrt(3/4)) + sqrt(3/4)*c(-space, space)
    if(clipped){
      xs = clipped.xs + c(-space/2, space/2)  # c(1/4, 3/4)
      ys = clipped.ys + sqrt(3/4)*c(-space/2, space/2)  # c(0, sqrt(3/4)/2) 
    }
    plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main)
    add.ternary.boundary()
  }
  
  # FP vote shares
  fp.vec = c(sum(v.vec[c(1,2,7)]), sum(v.vec[c(3,4,8)]), sum(v.vec[c(5,6,9)]))
  
  # margins 
  mBC = (v.vec[1] - v.vec[2])/fp.vec[1]  # to what extend does A's second pref favor B over C?
  mAC = (v.vec[3] - v.vec[4])/fp.vec[2]  # to what extend does B's second pref favor A over C?
  mAB = (v.vec[5] - v.vec[6])/fp.vec[3]  # to what extend does C's second pref favor A over B?
  
  # majority two-way tie points
  AB.tie.C.0 = c(1/2, 0, 1/2)
  AC.tie.B.0 = c(1/2, 1/2, 0)
  BC.tie.A.0 = c(0, 1/2, 1/2)
  
  # vertices
  A.vertex = c(1,0,0)
  B.vertex = c(0,0,1)
  C.vertex = c(0,1,0)
  
  if(plot.pairwise.lines){
    # AB majority tie line 
    ab.point = c(0, 1 - mAB/(1 + mAB), mAB/(1 + mAB))
    if(mAB < 0){
      ab.point = c(1 - 1/(1-mAB), 1/(1-mAB), 0)
    }
    add.ternary.lines(AB.tie.C.0, ab.point, col = secondary.line.col)
    
    # AC majority tie line 
    ac.point = c(0, mAC/(1 + mAC), 1 - mAC/(1 + mAC))
    if(mAC < 0){
      ac.point = c(1 - 1/(1-mAC), 0, 1/(1-mAC))
    }
    add.ternary.lines(AC.tie.B.0, ac.point, col = secondary.line.col)
    
    # BC majority tie line 
    bc.point = c(1/(1+mBC), 1 - 1/(1+mBC), 0)
    if(mBC < 0){
      bc.point = c(1/(1 - mBC), 0, 1 - 1/(1-mBC))
    }
    add.ternary.lines(BC.tie.A.0, bc.point, col = secondary.line.col)
  }
  
  # intersections 
  AB.AC.int.B = ((1 + mAB)/2)/(1 + mAB + (1+mAC)*(1-mAB)/2)
  if(mAB == 1){mAB = .999} # this resolves a divide-by-zero situation
  AB.AC.int.A = (AB.AC.int.B*(1 + mAB) - mAB)/(1 - mAB)
  AB.AC.int = c(AB.AC.int.A, 1 - AB.AC.int.A - AB.AC.int.B, AB.AC.int.B)
  
  AB.BC.int.A = (1 - (2*mAB/(1 + mAB)))/(2*(1-mAB)/(1 + mAB) + 1 + mBC)
  AB.BC.int.B = 1/2*(1 - AB.BC.int.A*(1 + mBC))
  AB.BC.int = c(AB.BC.int.A, 1 - AB.BC.int.A - AB.BC.int.B, AB.BC.int.B)
  
  AC.BC.int.A = ((1 - mAC)/2)/(2 - (1 + mBC)*(1 + mAC)/2)
  AC.BC.int.B = (1/2)*(1 - AC.BC.int.A*(1 + mBC))
  AC.BC.int = c(AC.BC.int.A, 1 - AC.BC.int.A - AC.BC.int.B, AC.BC.int.B)
  
  # define cycle mat
  cycle_mat = rbind(AC.BC.int, AB.AC.int, AB.BC.int)
  
  # assemble points for the polygons we want to draw 
  kp = get_kemeny_point(v.vec)
  forward.cycle = a.beats.b.at(kp, v.vec) # logical.
  vertex_mat = rbind(A.vertex, B.vertex, C.vertex)
  first_edge_mat = rbind(AB.tie.C.0, AB.tie.C.0, AC.tie.B.0)
  second_edge_mat = rbind(AC.tie.B.0, BC.tie.A.0, BC.tie.A.0)
  first_inner_intersection_mat = rbind(AB.AC.int, AB.AC.int, AC.BC.int)
  second_inner_intersection_mat = rbind(AC.BC.int, AB.BC.int, AB.BC.int)
  
  if(!forward.cycle){
    first_inner_intersection_mat = rbind(AB.BC.int, AB.BC.int, AB.AC.int)
    second_inner_intersection_mat = rbind(AB.AC.int, AC.BC.int, AC.BC.int)
  }
  # now the middle point
  
  # for methods that specify another approach, we start by checking who the winner is within the triangle
  #   midpoint = apply(cycle_mat, 2, mean)
  #   midpoint_mat = rbind(midpoint, midpoint, midpoint)
  #   alpha = .01
  #   just_in_the_cycle_mat = alpha*midpoint_mat + (1 - alpha)*cycle_mat
  
  # eventually TODO: determine Borda/IRV winner within the cycle.   
  if(in.cycle == "borda"){
    cat("Haven't done this yet"); 1/0
  }else if(in.cycle == "IRV"){
    cat("Haven't done this yet"); 1/0
  } 
  
  if(in.cycle == "kemeny"){
    k_mat = rbind(kp, kp, kp)
    # special ornamentation in this case?
    #add.ternary.lines2(AC.BC.int, AB.AC.int, lty = 1, overhang = 0, lwd = .5)
    #add.ternary.lines2(AC.BC.int, AB.BC.int, lty = 1, overhang = 0, lwd = .5)
    # add.ternary.lines2(AB.AC.int, AB.BC.int, lty = 1, overhang = 0, lwd = .5)
  }else if((in.cycle == "A" & forward.cycle) | (in.cycle == "C" & !forward.cycle)){
    k_mat = rbind(AB.BC.int, AB.BC.int, AB.BC.int)
  }else if((in.cycle == "C" & forward.cycle) | (in.cycle == "B" & !forward.cycle)){
    k_mat = rbind(AB.AC.int, AB.AC.int, AB.AC.int)
  }else if((in.cycle == "B" & forward.cycle) | (in.cycle == "A" & !forward.cycle)){
    k_mat = rbind(AC.BC.int, AC.BC.int, AC.BC.int)
  }else if(in.cycle == "empty"){
    k_mat = rbind(apply(rbind(AB.AC.int, AC.BC.int), 2, mean),
                  apply(rbind(AB.AC.int, AB.BC.int), 2, mean),
                  apply(rbind(AC.BC.int, AB.BC.int), 2, mean))
  }else{cat("Need to specify what to do in cycle -- empty?"); 1/0} # else if(in.cycle == "IRV"){
  # would be nice to do this, but not necessary for now. 
  
  # plan for doing central triangle: there is always a point k for each. and the order of points to draw is always (your vertex, edge intersection, majority tie line intersection (MTLI) 1, your.k, MTLI 2, edge intersection, your vertex). the order of the MTLIs depends on the direction of the cycle. [how do I determine that?]  
  # if kemeny, k is the kemeny point for all. the order of vertices AC.tie.B.0 vs AB.tie.C.0 depends on the direction of the cycle
  # if leaving cycle open, it is the average of the two intersections for this option.
  # if filling with a given alternative, it is the second MTLI in the sequence unless you are the chosen alternative, in which case it is the other MTLI. 
  # if IRV, what do I do? I will: get the cycle from condorcet (but don't draw), draw the IRV figure, then put white everywhere around the cycle, then add the condorcet figure with open cycle area. I think I can do that, but will not do it now.  
  
  # winning areas 
  these_shading_cols = shading.cols # [c(1,3,2)]
  for(j in 1:3){
    add.ternary.polygon(rbind(vertex_mat[j,], first_edge_mat[j,], first_inner_intersection_mat[j, ], k_mat[j,], second_inner_intersection_mat[j,], second_edge_mat[j,], vertex_mat[j,]), col = these_shading_cols[j], border = border)
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
  
  cycle_mat
}

get_kemeny_point = function(v_vec){
  
  v_a = sum(v_vec[c(1:2, 7)])
  v_b = sum(v_vec[c(3:4, 8)])
  v_c = sum(v_vec[c(5:6, 9)])
  pac = v_vec[2]/v_a
  pab = v_vec[1]/v_a
  pba = v_vec[3]/v_b
  pbc = v_vec[4]/v_b
  pca = v_vec[5]/v_c
  pcb = v_vec[6]/v_c
  
  # intercept and slope of the ab.bc kemeny line
  I = (-1 + pcb - pca)/((pac - pab) + (pcb - pca))
  M = (3 - pcb + pca)/((pac - pab) + (pcb - pca))
  # equation: v_a = I + v_b M
  
  # to draw: 
  # v_a_star = (I+M)/(1+M)
  # point_1 = c(v_a_star, 0, 1 - v_a_star)
  # v_b_star = -I/M
  # point_2 = c(0, 1 - v_b_star, v_b_star)
  
  # attributes of AC.AB Kemeny tie line 
  X = (3 + pcb - pca)
  Y = (3 + pbc - pba)
  # equation: Xv_c + Y v_b = 2
  
  # to plot it: 
  # if v_c = 0: 
  # v_b_star = 2/Y
  # point_1_a = c(1 - v_b_star, 0, v_b_star)
  # if v_v = 0
  # v_c_star = 2/X
  # point_2_a = c(1 - v_c_star, v_c_star, 0)
  
  # point of intersection 
  v_b_star = ((X-2)/X - I)/(M - (Y-X)/X)
  v_a_star = I + v_b_star*M
  c(v_a_star, 1 - v_a_star - v_b_star, v_b_star)
}

a.beats.b.at = function(point, v_vec){
  # Does a beat b at the given fp point, given the pattern of preferences in v_vec? If so (and if this is a cycle), this is a forward cycle.
  point[1] + point[2]*(v_vec[5]/sum(v_vec[c(5,6,9)])) > .5
}