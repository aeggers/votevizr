#' Extract vote share vector from election result 
#'
#' Used by \code{votevizr} plotting functions to process the election result passed by the user as a named list. 
#' 
#' @param result A named list.
#' @param split The character to be used to split the names of \code{result}
#' into a vector of candidate names.   
#' @return A list: \itemize{ 
#' \item \code{candidate_names} is a vector indicating the candidate names extracted from \code{result}.
#' \item \code{vector_of_vote_shares} is a vector of ballot shares; if
#'  \code{candidate_names} is \code{c("a", "b", "c")}, then the vector
#'   yields \code{c(v_ab, v_ac, v_ba, v_bc, v_ca, v_cb, v_ax, v_bx, v_cx)}, 
#'   where e.g. \code{v_ab} is the share of ballots listing \code{a} first and \code{b} second (implicitly \code{c} third), and \code{v_ax} indicates the share of ballots listing candidate \code{a} only.
#'  } 
#'  
#' @export
extract_vector_of_vote_shares_and_candidate_names_from_result <- function(result, split = ""){
  
  if(class(result) == "list"){
    df <- data.frame(ballot = names(result), value = unlist(result))
    candidate_names <- unique(unlist(strsplit(as.character(df$ballot), split = split))) 
    
    if(length(candidate_names) > 3){
      stop(paste0("I expected 3 candidate names but I detected ", length(candidate_names), ": ", paste(candidate_names, collapse = ", "), ".\nMore than three candidates, or incorrect 'split' argument?"))
    }
    v_vec <- sums_for_patterns(df, candidate_names, split)
  }else{stop("Expecting results to be provided in a named list.")}
  
  list(vector_of_vote_shares = v_vec, candidate_names = candidate_names)
  
}

sum_for_pattern <- function(df, pattern){
  sum(df$value[grepl(pattern, df$ballot)])
}

sums_for_patterns <- function(df, candidates, split = ""){
  c(sum_for_pattern(df, paste0("^", candidates[1], split, candidates[2])),
    sum_for_pattern(df, paste0("^", candidates[1], split, candidates[3])),
    sum_for_pattern(df, paste0("^", candidates[2], split, candidates[1])),
    sum_for_pattern(df, paste0("^", candidates[2], split, candidates[3])),
    sum_for_pattern(df, paste0("^", candidates[3], split, candidates[1])),
    sum_for_pattern(df, paste0("^", candidates[3], split, candidates[2])),
    sum_for_pattern(df, paste0("^", candidates[1], "$")),
    sum_for_pattern(df, paste0("^", candidates[2], "$")),
    sum_for_pattern(df, paste0("^", candidates[3], "$"))
  )
}
