#' Plot result of RCV election
#' 
#' Given ballot counts, represent the result of a ranked-choice voting 
#' election (aka alternative vote, preferential voting, instant-runoff voting, STV with one winner) on ternary diagram.
#' 
#' The dot shows the proportion of top ranks each candidate 
#' received; the division of the triangle into win regions shows 
#' the proportion of top ranks each candidate would need to win the election
#' (given the pattern of lower rankings).
#' 
#' @param result The result of the election to be plotted. There are three ways to specify a result: 
#'    \itemize{
#'    \item As a vector of ballot counts/shares of length 6 or length 9. 
#'    If candidates are A, B, and C, then the components should be 
#'    listed as (length 6 case) \code{c(ab, ac, ba, bc, ca, cb)} or
#'     (length 9 case) \code{c(ab, ac, ax, ba, bc, bx, ca, cb, cx)} where
#'     \code{ab} indicates the count or share of ballots listing \code{a}
#'      first and \code{b} second (implicitly \code{c} last) and 
#'      \code{ax} indicates the count of share of ballots listing \code{a}
#'      first and no one second. 
#'      \item As a matrix with a row for each of the three valid first choices (e.g. A, B, C) and a column for each valid second choice (A, B, C, and X if incomplete ballots are permitted)
#'      \item As a data frame in "long" format with the first and second columns identifying valid combinations of first and second choices (respectively) and a column of counts/shares} See documentation for \code{convert_result_to_vector_of_vote_shares()} for more detail on acceptable \code{result} input.
#' @param vertex.labels Labels for the vertices, in order bottom-left, top, bottom-right.
#' @param add.fp.result Include a dot for the observed shares of first-preferences, i.e. top rankings? 
#' @param shading.cols Colors for sharing first-preference win regions, in order bottom-left, top, bottom-right.
#' @param secondary.line.col Color for guidelines.
#' @param secondary.line.lwd Line width of guidelines 
#' @param main Main title for the plot.
#' @param new Draw new plot? If F, adds to existing plot.
#' = T, 
#' @param border Color of border of ternary diagram.
#' @param border.lwd Linewidth of diagram border. 
#' @param fp.result.col Color for first preference result.
#' @param fp.result.cex Size of first preference result. 
#' @param space How far from vertices to put labels? Default .1, where 1 is distance between bottom left and bottom right vertices. 
#' @param xlim,ylim Allows to focus on one part of figure. These are in standard coordinates. 
#' @param draw.elimination.regions Draws lines to show first preference shares where each candidate would be eliminated.
#' @param draw.majority.thresholds Draws lines to show where each candidate would win a majority of first preference shares. 
#' @return Beyond making the plot, the funtion returns a list of three points (in standard coordinates), each of
#'     which is an interior corner of a first-preference win region 
#'     in the diagram, where a majority tie line intersects with a 
#'     plurality tie line. For example, \code{AB.tie.C.max} is the
#'     intersection of the AB majority tie line with either the BC
#'     plurality tie line or the AC majority tie line, depending 
#'     on the pattern of second preferences. These points can 
#'     be useful for augmenting the plot.
#' @examples
#' plot_rcv_result(c(20, 3, 3,6, 7, 19))
#' plot_rcv_result(c(20, 3, 3,6, 7, 19, 4,6,2))
#' @export
plot_rcv_result = function(result, vertex.labels = c("A", "B", "C"), add.fp.result = T, fp.result.col = "black", fp.result.cex = 1, shading.cols = c("#E495A566", "#86B87566", "#7DB0DD66"), secondary.line.col = "gray", secondary.line.lwd = 2, main = NULL, new = T, border = "black", border.lwd = 1,  space = .1, xlim = c(0, 1), ylim = c(0, sqrt(3/4)), draw.elimination.regions = F, draw.majority.thresholds = F){
  
  v.vec <- convert_result_to_vector_of_vote_shares(result)
  
  # the basic plot
  if(new){
    xs = xlim + c(-space, space); ys = ylim + sqrt(3/4)*c(-space, space)
    plot(xs, ys, type = "n", xlab = "", ylab = "", axes = F, main = main) # blank canvas in space defined by clipped params
    add.ternary.boundary()
    
    # majority thresholds
    if(draw.majority.thresholds){
      add.ternary.lines(c(1/2, 1/2, 0), c(0, 1/2, 1/2), col = secondary.line.col, lwd = secondary.line.lwd, lty = 3)
      add.ternary.lines(c(0, 1/2, 1/2), c(1/2, 0, 1/2), col = secondary.line.col, lwd = secondary.line.lwd, lty = 3)
      add.ternary.lines(c(1/2, 0, 1/2), c(1/2, 1/2, 0), col = secondary.line.col, lwd = secondary.line.lwd, lty = 3)
      add.ternary.lines(c(1/2, 1/4, 1/4), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(1/4, 1/2, 1/4), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(1/4, 1/4, 1/2), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)		
    }
    
    # first-round pivotal events
    if(draw.elimination.regions){
      add.ternary.lines(c(1, 0, 0), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(0, 1, 0), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)
      add.ternary.lines(c(0, 0, 1), c(1/3, 1/3, 1/3), col = secondary.line.col, lwd = secondary.line.lwd, lty = 2)		
    }
    
  }
  
  # the first preference shares
  fp.vec = c(sum(v.vec[c(1,2,7)]), sum(v.vec[c(3,4,8)]), sum(v.vec[c(5,6,9)]))
  
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
    add.ternary.text(c(1,0,0), vertex.labels[1], x.offset = -label.offset, y.offset = -sqrt(3/4)*label.offset)
    add.ternary.text(c(0, 0,1), vertex.labels[2], x.offset = 0, y.offset = sqrt(3/4)*label.offset)
    add.ternary.text(c(0,1,0), vertex.labels[3], x.offset = label.offset, y.offset = -sqrt(3/4)*label.offset)
  }
  
  list("AB.tie.C.max" = AB.tie.C.max, "AC.tie.B.max" = AC.tie.B.max, "BC.tie.A.max" = BC.tie.A.max)
  
}