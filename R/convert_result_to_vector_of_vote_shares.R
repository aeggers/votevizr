#' Extract vote share vector from election result 
#'
#' Used by \code{votevizr} plotting functions to process the election result passed by the user. The user may pass the election result either as vote counts or vote shares, and these counts/shares may be organized as a vector, a matrix, or a data frame. 
#' @details
#' If a numeric vector is passed, it should be in the form \code{c(ab, ac, ax, ba, bc, bx, ca, cb, cx)} where e.g. \code{ab} is the count or share of ballots ranking A first and B second (implicitly, C last) and e.g. \code{ax} is the count or share of ballots ranking A first and not ranking any candidate second. If there are no incomplete ballots (AX, BX, CX), these may be omitted. (The function adds 0s in the appropriate places.)
#' 
#' If a matrix is passed, it should be in the form
#' 
#' | | A | B | C | X |
#' | -- | -- | -- | -- | -- |
#' |A| 0 | ab | ac | ax | 
#' |B| ba | 0 | bc | bx |
#' |C| ca | cb | 0 | cx |
#' 
#' If there are no incomplete ballots, the final column may also be omitted.
#' 
#' If a data.frame is passed, it should be in the form
#'  
#'  | first_choice | second_choice | count_or_share | 
#'  |--|--|--|
#'  | A | B | 475 |
#'  | A | C | 284 |
#'  | A | X | 6 |
#'  | B | A | 621 |
#'  | ... |...  | ... |
#'  
#'  If there are no incomplete ballots those rows may be omitted. The function sorts on the first and second columns and assumes the resulting third column is \code{c(ab, ac, ax, ba, ...)}. This means that, if you use symbols others than A, B, C, X, you should make sure that your "X" replacement comes last in a sort. It could be NA. 
#' @param result An election result: a vector, matrix, or data.frame of vote shares/counts. See details. 
#' @examples
#' convert_result_to_vector_of_vote_shares(c(20, 5, 8, 8, 10, 20)) # no incomplete ballots 
#' convert_result_to_vector_of_vote_shares(c(20, 5, 0, 8, 8, 0, 10, 20, 0)) # also no incomplete ballots
#' convert_result_to_vector_of_vote_shares(c(20, 5, 1, 8, 8, 2, 10, 20, 1)) # some incomplete ballots
#' result <- matrix(c(0, 20, 5, 1, 8, 0, 8, 2, 10, 20, 0, 1), nrow = 3, ncol = 4, byrow = T)
#' convert_result_to_vector_of_vote_shares(result)
#' cands <- c("A", "B", "C", "X")
#' cand_combos <- expand.grid(cands, cands)
#' colnames(cand_combos) <- c("first", "second")
#' cand_combos <- cand_combos[cand_combos$first != "X" & cand_combos$first != cand_combos$second,]
#' cand_combos$count <- sample(1:100, size = nrow(cand_combos))
#' convert_result_to_vector_of_vote_shares(cand_combos)  
#' @return A vector of vote shares (normalized to sum to one) with the order \code{c(ab, ac, ba, bc, ca, cb, ax, bx, cx)}. Note that the incomplete ballots are put at the end: this is slightly annoying but this is how the code developed and it would be annoying to fix it. 
#' @export
convert_result_to_vector_of_vote_shares <- function(result){
  
  if(class(result) == "data.frame"){
    if(nrow(result) > 9){stop("Expecting election result data frame to have no more than 9 rows.")}
    if(!class(result[[1]]) %in% c("factor", "character")){stop("Expecting first column of election result data frame to be a factor or character naming a candidate.")}
    if(!class(result[[2]]) %in% c("factor", "character")){stop("Expecting second column of election result data frame to be a factor or character naming a candidate.")}
    if(!class(result[[3]]) %in% c("integer", "numeric")){stop("Expecting third column of election result data frame to be a numeric vector of counts.")}
    # if passes checks, we extract the counts from the sorted dataset 
    result <- result[order(result[,1], result[,2]), 3]
  }else if(class(result) == "matrix"){
    if(nrow(result) > 3){stop("Expecting at most three rows of the result matrix.")}
    if(ncol(result) > 4){stop("Expecting at most four columns of the result matrix.")}
    if(sum(c(result[1,1], result[2,2], result[3,3]), na.rm = T) > 0){stop("Expecting the diagonal elements of the result matrix to be NA or 0.")}
    result[1,1] = result[2,2] = result[3,3] = NA
    result <- as.vector(t(result[,]))
    result <- result[!is.na(result)]
    if(!length(result) %in% c(6,9)){stop(cat("After extracting result from matrix, I have ", length(result), " non-NA elements, and I expected 6 or 9."))}
  }
  if(!class(result) %in% c("integer", "numeric")){stop("Election result should be passed as a data.frame, matrix, or numeric vector.")}
  if(sum(is.na(result)) > 0){stop("No NA elements in election result permitted.")}
  # reorder because this slightly unintuitive order is what the rest of the code expects
  if(length(result) == 9){
    result <- result[c(1,2,4,5,7,8,3,6,9)]
  }else if(length(result) == 6){
    result <- c(result, 0, 0, 0)
  }else{
    stop("If passing election result as a vector, must be length 6 or 9.\n")
  }
  result/sum(result)
}