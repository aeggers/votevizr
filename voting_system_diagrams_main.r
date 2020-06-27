#### changing header 
### plotting results in different systems 

simplex.x <- function(x, omitted = 0){
  if(sum(x) == 0){return(.5)}
  return( (1 - omitted)*(x[2] + 0.5 * x[3]) / sum(x) + omitted/2)
}
simplex.y <- function(x, omitted = 0){
  if(sum(x) == 0){return(sqrt(.75)*(1/3))}  
  return( (1 - omitted)*(sqrt(0.75) *  x[3]) / sum(x) + omitted/2)
} 

simplex.x2 <- function(x){
  x[2] + 0.5*x[3]
}
simplex.y2 <- function(x){
  sqrt(0.75)*x[3]
} 

simplex.xy <- function(coords){
  c(simplex.x(coords), simplex.y(coords))
}



add.ternary.point = function(point, col = "black", pch = 19, cex = 1, x.offset = 0, y.offset = 0){
  points(x = simplex.x(point) + x.offset, y = simplex.y(point) + y.offset, pch = pch, col = col, cex = cex)
}

add.ternary.text = function(point, labels, col = "black", cex = 1, x.offset = 0, y.offset = 0, pos = NULL, offset = NULL){
  text(x = simplex.x(point) + x.offset, y = simplex.y(point) + y.offset, labels = labels, col = col, cex = cex, pos = pos, offset = offset)
}

add.ternary.lines = function(point.1, point.2, col = "black", lwd = 1, lty = 1, x.adj = c(0,0), y.adj = c(0, 0), lend = 0){
  lines(x = c(simplex.x(point.1) + x.adj[1], simplex.x(point.2) + x.adj[2]), y = c(simplex.y(point.1) + y.adj[1], simplex.y(point.2) + y.adj[2]), col = col, lwd = lwd, lty = lty, lend = lend)
}

add.ternary.lines2 = function(point.1, point.2, col = "black", lwd = 1, lty = 1, lend = 0, overhang = 0){
  xs = c(simplex.x2(point.1), simplex.x2(point.2))
  ys = c(simplex.y2(point.1), simplex.y2(point.2))
  if(overhang > 0){
    if(xs[1] == xs[2]){
      ys = sort(ys) + c(-overhang, overhang)
    }else if(ys[1] == ys[2]){
      xs = sort(xs) + c(-overhang, overhang)
    }else{
      delta.x = xs[2] - xs[1]
      delta.y = ys[2] - ys[1]
      delta.length = sqrt(delta.x^2 + delta.y^2)
      length.ratio = overhang/delta.length
      xs = xs + length.ratio*delta.x*c(-1,1)
      ys = ys + length.ratio*delta.y*c(-1,1)
    }
  }
  lines(x = xs, y = ys, col = col, lwd = lwd, lty = lty, lend = lend)
}


### Ok now produce basic figures with guidelines and no labels and for both polls. 

add.ternary.guidelines = function(maj_guidelines = T, plurality_guidelines = F, plurality_tie_lines = T, lwd = 1, lty = 3, col = rgb(.2, .2, .2, alpha = .8), overhang = 0){
  if(maj_guidelines){
    add.ternary.lines2(c(1/2, 1/2, 0), c(0, 1/2, 1/2), col = col, lwd = lwd, lty = lty, overhang = overhang)
    add.ternary.lines2(c(0, 1/2, 1/2), c(1/2, 0, 1/2), col = col, lwd = lwd, lty = lty, overhang = overhang)
    add.ternary.lines2(c(1/2, 0, 1/2), c(1/2, 1/2, 0), col = col, lwd = lwd, lty = lty, overhang = overhang)
  }
  center = rep(1/3, 3)
  if(plurality_guidelines){
    add.ternary.lines2(c(1/2, 1/2, 0), center, col = col, lwd = lwd, lty = lty, overhang = overhang)
    add.ternary.lines2(c(0, 1/2, 1/2), center, col = col, lwd = lwd, lty = lty, overhang = overhang)
    add.ternary.lines2(c(1/2, 0, 1/2), center, col = col, lwd = lwd, lty = lty, overhang = overhang)
  }
  if(plurality_tie_lines){
    add.ternary.lines2(c(1/2, 1/2, 0), c(0, 0, 1), col = col, lwd = lwd, lty = lty, overhang = overhang)
    add.ternary.lines2(c(0, 1/2, 1/2), c(1, 0, 0), col = col, lwd = lwd, lty = lty, overhang = overhang)
    add.ternary.lines2(c(1/2, 0, 1/2), c(0, 1, 0), col = col, lwd = lwd, lty = lty, overhang = overhang)
  }
}

add.ternary.arrow = function(point.1, point.2, length = .075, angle = 30, col = NULL, lwd = 1, lty = 1){
  arrows(x0 = simplex.x(point.1), y0 = simplex.y(point.1), x1 = simplex.x(point.2), y1 = simplex.y(point.2), length = length, angle = angle, col = col, lwd = lwd, lty = lty)
}
  


add.ternary.polygon = function(point.mat, border = NULL, border.lwd = 1, col = NA){
  xs = apply(point.mat, 1, simplex.x)
  ys = apply(point.mat, 1, simplex.y)
  polygon(xs, ys, border = border, col = col, lwd = border.lwd)
}

add.ternary.boundary = function(k = 0, lwd = 1){
  bca.vertex = c(1 - k, 0, 0)
  cba.vertex = c(0, 1 - k, 0)
  bac.vertex = c(0, 0, 1-k)
  bac.v.x = simplex.x(bac.vertex, k); bac.v.y = simplex.y(bac.vertex, k)
  bca.v.x = simplex.x(bca.vertex, k); bca.v.y = simplex.y(bca.vertex, k)
  cba.v.x = simplex.x(cba.vertex, k); cba.v.y = simplex.y(cba.vertex, k) 
  
  lines(c(bac.v.x, cba.v.x), c(bac.v.y, cba.v.y), lwd = lwd)
  lines(c(bca.v.x, cba.v.x), c(bca.v.y, cba.v.y), lwd = lwd)
  lines(c(bac.v.x, bca.v.x), c(bac.v.y, bca.v.y), lwd = lwd)
}


library(colorspace)


plot.plurality.result = function(v.vec, from.v.vec = NULL, vertex.labels = c("A", "B", "C"), shading.cols = rainbow_hcl(3, alpha = .4), main = NULL, new = T, border = "black", fp.result.col = "black", fp.result.cex = 1, space = .1, add.fp.result = F){
  # the basic plot
  if(new){
    xs = c(0, 1) + c(-space, space); ys = c(0, sqrt(3/4)) + sqrt(3/4)*c(-space, space)
    plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main)
    add.ternary.boundary()
  }
  
  # the regions
  A.vertex = c(1,0,0)
  B.vertex = c(0,0,1)
  C.vertex = c(0,1,0)
  AB.tie.C.0 = c(1/2, 0, 1/2)
  AC.tie.B.0 = c(1/2, 1/2, 0)
  BC.tie.A.0 = c(0, 1/2, 1/2)
  midpoint = c(1/3, 1/3, 1/3)
  A.point.mat = rbind(A.vertex, 
                    AB.tie.C.0,
                    midpoint,
                    AC.tie.B.0,
                    A.vertex
  )		
  B.point.mat = rbind(B.vertex, 
                      AB.tie.C.0,
                      midpoint,
                      BC.tie.A.0,
                      B.vertex
  )		
  C.point.mat = rbind(C.vertex, 
                      AC.tie.B.0,
                      midpoint,
                      BC.tie.A.0,
                      C.vertex
  )		
  add.ternary.polygon(A.point.mat, col = shading.cols[1], border = border)
  add.ternary.polygon(B.point.mat, col = shading.cols[2], border = border)
  add.ternary.polygon(C.point.mat, col = shading.cols[3], border = border)
  
  # the result
  fp.vec = c(sum(v.vec[c(1,2,7)]), sum(v.vec[c(3,4,8)]), sum(v.vec[c(5,6,9)]))
  
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
  
}

# was called plot.v.vec in original version
plot.av.result = function(v.vec, from.v.vec = NULL, secondary.line.col = "gray", secondary.line.lwd = 2, vertex.labels = c("A", "B", "C"), shading.cols = rainbow_hcl(3, alpha = .4), main = NULL, new = T, border = "black", border.lwd = 1, fp.result.col = "black", fp.result.cex = 1, space = .1, clipped = F, add.fp.result = F, identify.elimination.regions = F, identify.majority.thresholds = F){
  # v.vec is in AB, AC, BA, BC, CA, CB, AX, BX, CX order 
  if(length(v.vec == 6)){
    v.vec = c(v.vec, 0, 0, 0)
  }
  # B is the top vertex
  
  # the basic plot
  if(new){
    xs = c(0, 1) + c(-space, space); ys = c(0, sqrt(3/4)) + sqrt(3/4)*c(-space, space)
    if(clipped){
      xs = c(1/4, 3/4) + c(-space/2, space/2)
      ys = c(0, sqrt(3/4)/2) + sqrt(3/4)*c(-space/2, space/2)
    }
    plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main)
    add.ternary.boundary()
    
    # majority thresholds
    if(identify.majority.thresholds){
      add.ternary.lines(c(1/2, 1/2, 0), c(0, 1/2, 1/2), col = secondary.line.col, lwd = secondary.line.lwd, lty = 3)
      add.ternary.lines(c(0, 1/2, 1/2), c(1/2, 0, 1/2), col = secondary.line.col, lwd = secondary.line.lwd, lty = 3)
      add.ternary.lines(c(1/2, 0, 1/2), c(1/2, 1/2, 0), col = secondary.line.col, lwd = secondary.line.lwd, lty = 3)
      add.ternary.lines(c(1/2, 1/4, 1/4), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(1/4, 1/2, 1/4), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(1/4, 1/4, 1/2), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)		
    }
    
    # first-round pivotal events
    if(identify.elimination.regions){
      add.ternary.lines(c(1, 0, 0), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(0, 1, 0), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(0, 0, 1), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)		
    }
    
  }
  
  
  # the first preference shares
  fp.vec = c(sum(v.vec[c(1,2,7)]), sum(v.vec[c(3,4,8)]), sum(v.vec[c(5,6,9)]))
  
  if(!is.null(from.v.vec)){
    from.fp.vec = c(sum(from.v.vec[c(1,2,7)]), sum(from.v.vec[c(3,4,8)]), sum(from.v.vec[c(5,6,9)]))
    plot.av.result(v.vec = from.v.vec, new = F, shading.cols = c(NULL, NULL, NULL), border = "gray", fp.result.col = "black", fp.result.cex = .5)
    add.ternary.lines(fp.vec[c(1,3,2)], from.fp.vec[c(1,3,2)], col = "black")		
  }
  
  
  # second-round pivotal events 
  mAB = (v.vec[1] - v.vec[2])/fp.vec[1]
  mBA = (v.vec[3] - v.vec[4])/fp.vec[2]
  mCA = (v.vec[5] - v.vec[6])/fp.vec[3]
  
  # Determining key points
  AB.tie.C.0 = c(1/2, 0, 1/2)
  AC.tie.B.0 = c(1/2, 1/2, 0)
  BC.tie.A.0 = c(0, 1/2, 1/2)
  
  if(mAB > 0){
    denom.pos = 3 + mAB
    BC.tie.A.max = c(1/denom.pos, 1 - 2/denom.pos, 1/denom.pos)
  }else{
    denom.neg = 3 - mAB
    BC.tie.A.max = c(1/denom.neg, 1/denom.neg, 1 - 2/denom.neg)	
  }
  
  if(mBA > 0){
    denom.pos = 3 + mBA
    AC.tie.B.max = c(1/denom.pos, 1 - 2/denom.pos, 1/denom.pos)
  }else{
    denom.neg = 3 - mBA
    AC.tie.B.max = c(1 - 2/denom.neg, 1/denom.neg, 1/denom.neg)	
  }
  
  if(mCA > 0){
    denom.pos = 3 + mCA
    AB.tie.C.max = c(1/denom.pos, 1/denom.pos, 1 - 2/denom.pos)
  }else{
    denom.neg = 3 - mCA
    AB.tie.C.max = c(1 - 2/denom.neg, 1/denom.neg, 1/denom.neg)	
  }
  
  # now to draw this. 
  
  # A winning area 
  point.mat = rbind(
    AC.tie.B.0,
    c(1,0,0), 
    AB.tie.C.0,
    AB.tie.C.max) # this will always be the order
  if(mCA > 0 | mBA > 0){
    point.mat = rbind(point.mat, c(1/3, 1/3, 1/3))
  }
  point.mat = rbind(point.mat, 
                    AC.tie.B.max,
                    AC.tie.B.0
  )		
  add.ternary.polygon(point.mat, col = shading.cols[1], border = border, border.lwd = border.lwd)
  
  # B winning area 
  point.mat = rbind(
    AB.tie.C.0,
    c(0,0,1), 
    BC.tie.A.0,
    BC.tie.A.max) # this will always be the order
  if(mAB > 0 | mCA < 0){
    point.mat = rbind(point.mat, c(1/3, 1/3, 1/3))
  }
  point.mat = rbind(point.mat, 
                    AB.tie.C.max,
                    AB.tie.C.0
  )		
  add.ternary.polygon(point.mat, col = shading.cols[2], border = border, border.lwd = border.lwd)
  
  # C winning area 
  point.mat = rbind(
    BC.tie.A.0,
    c(0,1,0), 
    AC.tie.B.0,
    AC.tie.B.max) # this will always be the order
  if(mBA < 0 | mAB < 0){
    point.mat = rbind(point.mat, c(1/3, 1/3, 1/3))
  }
  point.mat = rbind(point.mat, 
                    BC.tie.A.max,
                    BC.tie.A.0
  )		
  add.ternary.polygon(point.mat, col = shading.cols[3], border = border, border.lwd = border.lwd)
  
  # FP result
  if(add.fp.result){
    add.ternary.point(fp.vec[c(1,3,2)], pch = 19, cex = fp.result.cex, col = fp.result.col)
  }
  
  if(new){
    label.offset = .05
    if(clipped){
      add.ternary.text(c(3/4,1/4,0), vertex.labels[1], x.offset = -label.offset, y.offset = -sqrt(3/4)*label.offset)
      # masking box
      y.inc = label.offset/2
      polygon(c(-100, 100, 100, -100), sqrt(3/4)*c(1/2 + y.inc, 1/2 + y.inc, 100, 100), col = "white", border = F)
      add.ternary.text(c(1/4, 1/4,1/2), vertex.labels[2], x.offset = 0, y.offset = sqrt(3/4)*label.offset)
      add.ternary.text(c(1/4,3/4,0), vertex.labels[3], x.offset = label.offset, y.offset = -sqrt(3/4)*label.offset)
    }else{
      add.ternary.text(c(1,0,0), vertex.labels[1], x.offset = -label.offset, y.offset = -sqrt(3/4)*label.offset)
      add.ternary.text(c(0, 0,1), vertex.labels[2], x.offset = 0, y.offset = sqrt(3/4)*label.offset)
      add.ternary.text(c(0,1,0), vertex.labels[3], x.offset = label.offset, y.offset = -sqrt(3/4)*label.offset)
    }
    
  }
  
  list("AB.tie.C.max" = AB.tie.C.max, "AC.tie.B.max" = AC.tie.B.max, "BC.tie.A.max" = BC.tie.A.max)
  
}

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
#    Z = first_edge_mat
#    first_edge_mat = second_edge_mat
#    second_edge_mat = Z
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
  # old approach 
  # add.ternary.polygon(rbind(A.vertex, AC.tie.B.0, AB.AC.int, AB.tie.C.0, A.vertex), col = shading.cols[1], border = border)
  # add.ternary.polygon(rbind(B.vertex, AB.tie.C.0, AB.BC.int, BC.tie.A.0, B.vertex), col = shading.cols[2], border = border)
  # add.ternary.polygon(rbind(C.vertex, AC.tie.B.0, AC.BC.int, BC.tie.A.0, C.vertex), col = shading.cols[3], border = border)
  
  # if(!is.null(fill.for.cycle)){
  #   add.ternary.polygon(rbind(AC.BC.int, AB.AC.int, AB.BC.int, AC.BC.int), col = fill.for.cycle, border = F)
  # }
  
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


# positional system 

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
  out = list()
  ab_point_mat_xyz = point_mat_in_xyz_form(p_vec, s)
  ab_point_mat = ab_point_mat_xyz[,c(1,3,2)]
  ac_point_mat_xyz = point_mat_in_xyz_form(c(1 - p_vec[1], p_vec[3], p_vec[2]), s)
  ac_point_mat = ac_point_mat_xyz
  bc_point_mat_xyz = point_mat_in_xyz_form(c(1 - p_vec[2], 1 - p_vec[3], p_vec[1]), s)
  bc_point_mat = bc_point_mat_xyz[, c(3, 2, 1)]
  list(ab_point_mat, ac_point_mat, bc_point_mat)
}

# this is incomplete -- just handling easy special cases for paper
plot.bucklin.result = function(v.vec, secondary.line.col = "gray", secondary.line.lwd = 2, vertex.labels = c("A", "B", "C"), shading.cols = rainbow_hcl(3, alpha = .4), main = NULL, new = T, border = "black", fp.result.col = "black", fp.result.cex = 1, space = .1, clipped = F, add.fp.result = F){
  
  out.list = plot.positional.result(v.vec, positional.s = 1, plot = F)
  tpms = out.list[["tpms"]]
  intersection.point = out.list[["intersection.point"]]
  
  xs = c(0, 1) + c(-space, space); ys = c(0, sqrt(3/4)) + sqrt(3/4)*c(-space, space)
  plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main)
  add.ternary.boundary()
  
  # vertices
  A.vertex = c(1,0,0)
  B.vertex = c(0,0,1)
  C.vertex = c(0,1,0)
  
  # winning areas 
  if(min(intersection.point) >= 0 & max(intersection.point) < 1/2){
    # then it's just a normal antiplurality result 
    add.ternary.polygon(rbind(A.vertex, tpms[[2]][1,], intersection.point, tpms[[1]][1,], A.vertex), col = shading.cols[1], border = border)
    add.ternary.polygon(rbind(B.vertex, tpms[[1]][1,], intersection.point, tpms[[3]][1,], B.vertex), col = shading.cols[2], border = border)
    add.ternary.polygon(rbind(C.vertex, tpms[[2]][1,], intersection.point, tpms[[3]][1,], C.vertex), col = shading.cols[3], border = border)
  }else if(intersection.point[3] < 0 & min(tpms[[1]][,1]) > 1/2 & min(tpms[[3]][,2]) > 1/2){
    # B wins whenever no one else wins a majorty     
    add.ternary.polygon(rbind(A.vertex, c(1/2, 1/2, 0), c(1/2, 0, 1/2), A.vertex), col = shading.cols[1], border = border)
    add.ternary.polygon(rbind(B.vertex, c(1/2, 0, 1/2), c(1/2, 1/2, 0), c(0, 1/2, 1/2), B.vertex), col = shading.cols[2], border = border)
    add.ternary.polygon(rbind(C.vertex, c(1/2, 1/2, 0), c(0, 1/2, 1/2), C.vertex), col = shading.cols[3], border = border)
  }
  
  if(add.fp.result){
    fp.vec = c(sum(v.vec[c(1,2,7)]), sum(v.vec[c(3,4,8)]), sum(v.vec[c(5,6,9)]))
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

plot.positional.result = function(v.vec, positional.s = .5, secondary.line.col = "gray", secondary.line.lwd = 2, vertex.labels = c("A", "B", "C"), shading.cols = rainbow_hcl(3, alpha = .4), main = NULL, new = T, border = "black", fp.result.col = "black", fp.result.cex = 1, space = .1, clipped = F, add.fp.result = F, plot.pairwise.lines = F, highlight.positional.tie.lines = F, highlight.color = rgb(1,0,0,alpha = .5), highlight.lwd = 4, highlight.lend = 0, plot = T){
  
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


# side-by-side comparison 
#basic.v.vec = c(20, 6, 13,10, 4, 21, 2,4,3)
#v.vec = basic.v.vec/sum(basic.v.vec)
#compare_three_systems(v.vec)

compare_three_systems = function(v.vec){
  par(mfrow = c(1,3))
  plot.plurality.result(v.vec, main = "plurality")
  plot.av.result(v.vec, main = "AV")
  plot.condorcet.result(v.vec, main = "Condorcet")
}


## Next steps: 
# shiny app (for these three or more -- text entries for counts, or arrows?)
# Borda count (or positional methods in general)
# overlays to show where they disagree

# writeup: 
