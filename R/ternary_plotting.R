### ternary points lines etc 

add.ternary.point = function(point, col = "black", pch = 19, cex = 1, x.offset = 0, y.offset = 0){
  points(x = simplex.x(point) + x.offset, y = simplex.y(point) + y.offset, pch = pch, col = col, cex = cex)
}

add.ternary.text = function(point, labels, col = "black", cex = 1, x.offset = 0, y.offset = 0, pos = NULL, offset = NULL){
  text(x = simplex.x(point) + x.offset, y = simplex.y(point) + y.offset, labels = labels, col = col, cex = cex, pos = pos, offset = offset)
}

add.ternary.lines = function(point.1, point.2, col = "black", lwd = 1, lty = 1, x.adj = c(0,0), y.adj = c(0, 0), lend = 0){
  lines(x = c(simplex.x(point.1) + x.adj[1], simplex.x(point.2) + x.adj[2]), y = c(simplex.y(point.1) + y.adj[1], simplex.y(point.2) + y.adj[2]), col = col, lwd = lwd, lty = lty, lend = lend)
}

# this version allows for an overhang. useful for making diagrams in paper. could be cleaned up of course. 
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

add.ternary.arrow = function(point.1, point.2, length = .075, angle = 30, col = NULL, lwd = 1, lty = 1){
  arrows(x0 = simplex.x(point.1), y0 = simplex.y(point.1), x1 = simplex.x(point.2), y1 = simplex.y(point.2), length = length, angle = angle, col = col, lwd = lwd, lty = lty)
}

add.ternary.polygon = function(point.mat, border = NULL, border.lwd = 1, col = NA){
  xs = apply(point.mat, 1, simplex.x)
  ys = apply(point.mat, 1, simplex.y)
  polygon(xs, ys, border = border, col = col, lwd = border.lwd)
}



#### commands for making the blank canvas and guidelines on it. #### 
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

add.ternary.boundary = function(lwd = 1){
  bca.vertex = c(1, 0, 0)
  cba.vertex = c(0, 1, 0)
  bac.vertex = c(0, 0, 1)
  bac.v.x = simplex.x(bac.vertex); bac.v.y = simplex.y(bac.vertex)
  bca.v.x = simplex.x(bca.vertex); bca.v.y = simplex.y(bca.vertex)
  cba.v.x = simplex.x(cba.vertex); cba.v.y = simplex.y(cba.vertex) 
  
  lines(c(bac.v.x, cba.v.x), c(bac.v.y, cba.v.y), lwd = lwd)
  lines(c(bca.v.x, cba.v.x), c(bca.v.y, cba.v.y), lwd = lwd)
  lines(c(bac.v.x, bca.v.x), c(bac.v.y, bca.v.y), lwd = lwd)
}