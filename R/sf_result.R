#' Ballots ranking the top three SF mayoral candidates, 2018
#'
#' The proportion of ballots with each ranking over the three 
#' top candidates in the San Francsico mayoral election of 2018.
#'
#' @format A named list with 9 elements, one for each 
#' (incomplete) ordering of the three alternatives. Names 
#' (e.g. "Breed_Leno", "Leno_Kim", "Kim") specify the ordering;
#' because a ballot ranking Breed over Leno over Kim is
#' effectively the same as a ballot ranking Breed over Leno 
#' (and not ranking Kim), the third candidate (if there is one)
#' is dropped.    
#' @source http://www.sfelections.org/results/20180605/data/20180627/20180627_ballotimage.txt
"sf_result"