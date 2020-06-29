#### RCV aka AV aka STV1 aka preferential voting

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