#' Extract first-preference shares from an ordinal election result
#'
#' Given counts/shares of ballot orderings in various 
#' formats, returns a vector with the proportion of 
#' ballots ranking each candidate first.  
#' 
#' @param result The election result specified as a vector, matrix, or dataframe/tibble of counts for each ordering of the candidates.
#' @export
first_preference_shares <- function(result, candidate_labels = c("A", "B", "C")){
  
  # transform input to appropriate format
  v_vec <- convert_result_to_vector_of_vote_shares(result)
  ### compute points of intersection ###
  # FP vote shares
  mat <- matrix(c(sum(v_vec[c(1,2,7)]), sum(v_vec[c(3,4,8)]), sum(v_vec[c(5,6,9)])), nrow = 1)
  colnames(mat) <- candidate_labels
  as.data.frame(mat)
}
