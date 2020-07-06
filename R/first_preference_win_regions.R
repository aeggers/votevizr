#' Extract first-preference win regions for plotting 
#'
#' In an ordinal voting system, a candidate's "first-preference 
#' win region" identifies first-preference shares where that candidate wins the election, 
#' given the pattern of lower preferences. Each function below take counts/shares of ballot orderings and returns 
#' a data frame with the vertices of each candidate's 
#' first-preference win region for a given voting system. 
#' 
#' @param result The election result specified as a named list 
#' of counts or shares for each ballot type, where each name 
#' is a (possibly incomplete) ordering of candidates. 
#' Examples: \itemize{
#' \item \code{list("abc" = 20, "acb" = 4, "bac" = 7, "bca" = 10, "cab" = 8, "cba" = 4, "ab" = 4, "a" = 1)}
#' \item \code{list("Remain > Deal > No deal" = 20, "Remain > No deal > Deal" = 4, etc.)}
#' }
#' @param split The character by which to split elements of 
#' the named list to recover the candidate names.
#' @param method The election method: one of \code{"positional", "Condorcet", "RCV", "plurality", "Borda", "antiplurality"}.
#' @param s If \code{method = "positional"}, specifies the 
#' relative value of a second preference, where a first
#'  preference is worth 1 and a third preference is worth 0.
#'  Special cases include plurality (\code{s = 0}) and 
#'  Borda count (\code{s = .5}).
#'  @param if_cycle If \code{method = "Condorcet"},
#'   specifies how the winner is determined in the 
#'  event of a Condorcet cycle. The first-preference win regions 
#'  leave open the cyclic area unless \code{if_cycle} is: 
#'  \itemize{
#'  \item \code{"kemeny"}: Kemeny-Young method, i.e. the winner 
#'  is the candidate who loses by the smallest margin
#'  \item a candidate name (must match a candidate name in \code{result}): 
#'  choose a particular candidate in the event of a cycle
#'  } 

#' @name first_preference_win_regions
NULL

#### generic #### 
#' @rdname first_preference_win_regions
#' @export
first_preference_win_regions <- function(result, split = "", method = "plurality", if_cycle = "empty", s = .5){
  if(grepl("^plurality$", method, ignore.case = T)){
    positional_first_preference_win_regions(result, split, s = 0)
  }else if(grepl("^borda", method, ignore.case = T)){
    positional_first_preference_win_regions(result, split, s = .5)
  }else if(grepl("^anti[-]?plurality", method, ignore.case = T)){
    positional_first_preference_win_regions(result, split, s = 1)
  }else if(grepl("^positional$", method, ignore.case = T)){
    positional_first_preference_win_regions(result, split, s = s)
  }else if(grepl("Condorcet", method, ignore.case = T)){
    condorcet_first_preference_win_regions(result, split, if_cycle = if_cycle)
  }else if(grepl("^irv$", method, ignore.case = T) | grepl("^rcv$", method, ignore.case = T)){
    rcv_first_preference_win_regions(result, split)
  }else{
    stop("You specified a method I didn't expect: I can handle 'positional', 'plurality', 'borda', 'antiplurality', 'IRV'/'RCV', 'Condorcet'")
  }
}

#### positional ####
#' @rdname first_preference_win_regions
#' @export
positional_first_preference_win_regions <- function(result, split = "", s = .5){
  
  # transform input to appropriate format
  out <- extract_vector_of_vote_shares_and_candidate_names_from_result(result, split)
  v_vec <- out$vector_of_vote_shares
  candidate_names <- out$candidate_names

  ### compute points of intersection ###
  # FP vote shares
  fp_vec = c(sum(v_vec[c(1,2,7)]), sum(v_vec[c(3,4,8)]), sum(v_vec[c(5,6,9)]))
  
  # conditional proportions 
  pAB = v_vec[1]/fp_vec[1]
  pAC = v_vec[2]/fp_vec[1]
  pBA = v_vec[3]/fp_vec[2]
  pBC = v_vec[4]/fp_vec[2]
  pCA = v_vec[5]/fp_vec[3]
  pCB = v_vec[6]/fp_vec[3]
  
  # this yields a list of 3 matrices, one for each positional tie line
  tpms = ternary_point_mats_from_p_vec_and_s_v3(c(pAB, pAC, pBA, pBC, pCA, pCB), s)  
  
  # find the point intersection among those tie lines, where A, B, and C all get the same score
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
  intersection_point = c(v_a.star, v_b.star, 1 - v_b.star - v_a.star)
  
  # vertices
  A.vertex = c(1,0,0)
  B.vertex = c(0,1,0)
  C.vertex = c(0,0,1)
  
  # first preference win regions 
  if(min(intersection_point) >= 0){
    out <- rbind(
      data.frame("candidate" = candidate_names[1], 
                 rbind(A.vertex, tpms[[2]][1,], intersection_point, tpms[[1]][1,], A.vertex)),
      data.frame("candidate" = candidate_names[2],
                 rbind(B.vertex, tpms[[1]][1,], intersection_point, tpms[[3]][1,], B.vertex)),
      data.frame("candidate" = candidate_names[3],
                 rbind(C.vertex, tpms[[2]][1,], intersection_point, tpms[[3]][1,], C.vertex))
    )
  }else if(intersection_point[2] < 0){
    out <- rbind(
      data.frame("candidate" = candidate_names[1], 
                 rbind(A.vertex, tpms[[1]][1,], tpms[[1]][2,], A.vertex)),
      data.frame("candidate" = candidate_names[2],
                 rbind(B.vertex, tpms[[1]][1,], tpms[[1]][2,], tpms[[3]][2,], tpms[[3]][1,], B.vertex)),
      data.frame("candidate" = candidate_names[3],
                 rbind(C.vertex, tpms[[3]][1,], tpms[[3]][2,], C.vertex))
    )
  }else if(intersection_point[1] < 0){
    out <- rbind(
      data.frame("candidate" = candidate_names[1], 
                 rbind(A.vertex, tpms[[1]][1,], tpms[[1]][2,], tpms[[2]][2,],  tpms[[2]][1,], A.vertex)),
      data.frame("candidate" = candidate_names[2],
                 rbind(B.vertex, tpms[[1]][1,], tpms[[1]][2,], B.vertex)),
      data.frame("candidate" = candidate_names[3],
                 rbind(C.vertex, tpms[[2]][1,], tpms[[2]][2,], C.vertex))
    )
  }else if(intersection_point[3] < 0){
    out <- rbind(
      data.frame("candidate" = candidate_names[1], 
                 rbind(A.vertex, tpms[[2]][1,], tpms[[2]][2,], A.vertex)),
      data.frame("candidate" = candidate_names[2],
                 rbind(B.vertex, tpms[[3]][1,], tpms[[3]][2,], B.vertex)),
      data.frame("candidate" = candidate_names[3],
                 rbind(C.vertex, tpms[[2]][1,], tpms[[2]][2,], tpms[[3]][2,], tpms[[3]][1,], C.vertex))
    )
  }
  rownames(out) = NULL
  colnames(out) = c("candidate", candidate_names)    
  out

}


#### RCV ####
#' @rdname first_preference_win_regions
#' @export
rcv_first_preference_win_regions <- function(result, split = ""){
  
  # transform input to appropriate format
  out <- extract_vector_of_vote_shares_and_candidate_names_from_result(result, split)
  v_vec <- out$vector_of_vote_shares
  candidate_names <- out$candidate_names
  
  ### compute points of intersection ###
  # FP vote shares
  fp_vec = c(sum(v_vec[c(1,2,7)]), sum(v_vec[c(3,4,8)]), sum(v_vec[c(5,6,9)]))
  
  # second-round pivotal events 
  mAB = (v_vec[1] - v_vec[2])/fp_vec[1]
  mBA = (v_vec[3] - v_vec[4])/fp_vec[2]
  mCA = (v_vec[5] - v_vec[6])/fp_vec[3]
  
  # Determining key points
  AB.tie.C.0 = c(1/2, 1/2, 0)
  AC.tie.B.0 = c(1/2, 0, 1/2)
  BC.tie.A.0 = c(0, 1/2, 1/2)
  
  if(mAB > 0){
    denom.pos = 3 + mAB
    BC.tie.A.max = c(1/denom.pos, 1/denom.pos, 1 - 2/denom.pos)
  }else{
    denom.neg = 3 - mAB
    BC.tie.A.max = c(1/denom.neg, 1 - 2/denom.neg, 1/denom.neg)	
  }
  
  if(mBA > 0){
    denom.pos = 3 + mBA
    AC.tie.B.max = c(1/denom.pos, 1/denom.pos, 1 - 2/denom.pos)
  }else{
    denom.neg = 3 - mBA
    AC.tie.B.max = c(1 - 2/denom.neg, 1/denom.neg, 1/denom.neg)	
  }
  
  if(mCA > 0){
    denom.pos = 3 + mCA
    AB.tie.C.max = c(1/denom.pos, 1 - 2/denom.pos, 1/denom.pos)
  }else{
    denom.neg = 3 - mCA
    AB.tie.C.max = c(1 - 2/denom.neg, 1/denom.neg, 1/denom.neg)	
  }
  
  A_fp_mat <- rbind(
      AC.tie.B.0,
      c(1,0,0), 
      AB.tie.C.0,
      AB.tie.C.max,
      rep(1, 3)/3, # conditionally
      AC.tie.B.max,
      AC.tie.B.0
      ) # this will always be the order
  if(mCA < 0 & mBA < 0){
      A_fp_mat <- A_fp_mat[-5,]
  }
  
  B_fp_mat <- rbind(
    AB.tie.C.0,
    c(0,1,0), 
    BC.tie.A.0,
    BC.tie.A.max,
    c(1/3, 1/3, 1/3),
    AB.tie.C.max,
    AB.tie.C.0
    )
  if(mAB < 0 & mCA > 0){
    B_fp_mat <- B_fp_mat[-5,]
  }
  
  C_fp_mat <- rbind(
    BC.tie.A.0,
    c(0,0,1), 
    AC.tie.B.0,
    AC.tie.B.max,
    c(1/3, 1/3, 1/3),
    BC.tie.A.max,
    BC.tie.A.0
  )
  if(mBA > 0 & mAB > 0){
    C_fp_mat <- C_fp_mat[-5,]
  }
  
  out <- rbind(data.frame(candidate = candidate_names[1], A_fp_mat),
        data.frame(candidate = candidate_names[2], B_fp_mat),
        data.frame(candidate = candidate_names[3], C_fp_mat)
        )
  
  rownames(out) <- NULL
  colnames(out) <- c("candidate", candidate_names)
  out
  
}

#### Condorcet #### 

#' @rdname first_preference_win_regions
#' @export
condorcet_first_preference_win_regions <- function(result, split = "", if_cycle = "empty"){
  
  # transform input to appropriate format
  out <- extract_vector_of_vote_shares_and_candidate_names_from_result(result, split)
  v_vec <- out$vector_of_vote_shares
  candidate_names <- out$candidate_names
  
  ### compute points of intersection ###
  # FP vote shares
  fp_vec = c(sum(v_vec[c(1,2,7)]), sum(v_vec[c(3,4,8)]), sum(v_vec[c(5,6,9)]))
  
  # margins 
  mBC = (v_vec[1] - v_vec[2])/fp_vec[1]  # to what extent does A's second pref favor B over C?
  mAC = (v_vec[3] - v_vec[4])/fp_vec[2]  # to what extent does B's second pref favor A over C?
  mAB = (v_vec[5] - v_vec[6])/fp_vec[3]  # to what extent does C's second pref favor A over B?
  
  # majority two-way tie points
  AB.tie.C.0 = c(1/2, 1/2, 0)
  AC.tie.B.0 = c(1/2, 0, 1/2)
  BC.tie.A.0 = c(0, 1/2, 1/2)
  
  # vertices
  A.vertex = c(1,0,0)
  B.vertex = c(0,1,0)
  C.vertex = c(0,0,1)
  
  # points of intersection 
  # the point of intersection between the AB majority tie line 
  # and the AC majority tie line. 
  AB.AC.int.B = ((1 + mAB)/2)/(1 + mAB + (1+mAC)*(1-mAB)/2)
  if(mAB == 1){mAB = .999} # this resolves a divide-by-zero situation
  AB.AC.int.A = (AB.AC.int.B*(1 + mAB) - mAB)/(1 - mAB)
  AB.AC.int = c(AB.AC.int.A, AB.AC.int.B, 1 - AB.AC.int.A - AB.AC.int.B)
  
  AB.BC.int.A = (1 - (2*mAB/(1 + mAB)))/(2*(1-mAB)/(1 + mAB) + 1 + mBC)
  AB.BC.int.B = 1/2*(1 - AB.BC.int.A*(1 + mBC))
  AB.BC.int = c(AB.BC.int.A, AB.BC.int.B, 1 - AB.BC.int.A - AB.BC.int.B)
  
  AC.BC.int.A = ((1 - mAC)/2)/(2 - (1 + mBC)*(1 + mAC)/2)
  AC.BC.int.B = (1/2)*(1 - AC.BC.int.A*(1 + mBC))
  AC.BC.int = c(AC.BC.int.A, AC.BC.int.B, 1 - AC.BC.int.A - AC.BC.int.B)
  
  # define cycle mat
  cycle_mat = rbind(AC.BC.int, AB.AC.int, AB.BC.int)
  
  # assemble points for the polygons we want to draw 
  kp = get_kemeny_point(v_vec)
  forward.cycle = a.beats.b.at(v_vec, kp) # logical.
  vertex_mat = rbind(A.vertex, B.vertex, C.vertex)
  first_edge_mat = rbind(AB.tie.C.0, AB.tie.C.0, AC.tie.B.0)
  second_edge_mat = rbind(AC.tie.B.0, BC.tie.A.0, BC.tie.A.0)
  first_inner_intersection_mat = rbind(AB.AC.int, AB.AC.int, AC.BC.int)
  second_inner_intersection_mat = rbind(AC.BC.int, AB.BC.int, AB.BC.int)
  
  if(!forward.cycle){
    first_inner_intersection_mat = rbind(AB.BC.int, AB.BC.int, AB.AC.int)
    second_inner_intersection_mat = rbind(AB.AC.int, AC.BC.int, AC.BC.int)
  }
  
  # now what to do if a cycle?
  if(if_cycle == "kemeny"){
    k_mat = rbind(kp, kp, kp)
  }else if((if_cycle == candidate_names[1] & forward.cycle) | (if_cycle == candidate_names[3] & !forward.cycle)){
    k_mat = rbind(AB.BC.int, AB.BC.int, AB.BC.int)
  }else if((if_cycle == candidate_names[3] & forward.cycle) | (if_cycle == candidate_names[2] & !forward.cycle)){
    k_mat = rbind(AB.AC.int, AB.AC.int, AB.AC.int)
  }else if((if_cycle == candidate_names[2] & forward.cycle) | (if_cycle == candidate_names[1] & !forward.cycle)){
    k_mat = rbind(AC.BC.int, AC.BC.int, AC.BC.int)
  }else{
    # this leaves it empty. 
    k_mat = first_inner_intersection_mat
  } 
  
  j = 1
  A_df <- data.frame("candidate" = candidate_names[1], rbind(vertex_mat[j,], first_edge_mat[j,], first_inner_intersection_mat[j, ], k_mat[j,], second_inner_intersection_mat[j,], second_edge_mat[j,], vertex_mat[j,]))
  j = 2
  B_df <- data.frame("candidate" = candidate_names[2], rbind(vertex_mat[j,], first_edge_mat[j,], first_inner_intersection_mat[j, ], k_mat[j,], second_inner_intersection_mat[j,], second_edge_mat[j,], vertex_mat[j,]))
  j = 3
  C_df <- data.frame("candidate" = candidate_names[3], rbind(vertex_mat[j,], first_edge_mat[j,], first_inner_intersection_mat[j, ], k_mat[j,], second_inner_intersection_mat[j,], second_edge_mat[j,], vertex_mat[j,]))

  out <- rbind(A_df, B_df, C_df)
  colnames(out) <- c("candidate", candidate_names)
  
  out
}
  


