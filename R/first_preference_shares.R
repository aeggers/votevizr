#' Extract first-preference shares from an ordinal election result
#'
#' Given counts/shares of ballot orderings in various 
#' formats, returns a vector with the proportion of 
#' ballots ranking each candidate first.  
#' 
#' @param result The election result specified as a named list, 
#' with each name being candidate names in order, e.g. 
#' "Breed_Leno_Kim" or "ABC" or "Remain > Deal > No deal"
#' @param split The character on which to split the orderings 
#' to recover candidate names.
#' @export
first_preference_shares <- function(result, split = ""){
  
  out <- extract_vector_of_vote_shares_and_candidate_names_from_result(result, split)
  
  v_vec <- out$vector_of_vote_shares
  # FP vote shares
  mat <- matrix(c(sum(v_vec[c(1,2,7)]), sum(v_vec[c(3,4,8)]), sum(v_vec[c(5,6,9)])), nrow = 1)
  colnames(mat) <- out$candidate_names
  as.data.frame(mat)
}
