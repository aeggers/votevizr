## code to prepare sf_result dataset goes here

library(tidyverse)
library(stringr)

library(curl)
t <- curl("http://www.sfelections.org/results/20180605/data/20180627/20180627_ballotimage.txt")
file <- readLines(t) # takes a while 


extract_from_to = function(s, from, to){
  paste0(str_split(s, "")[[1]][from:to], collapse = "")
} 

convert_line_to_data = function(s){
  c(as.integer(extract_from_to(s, 8, 16)), as.integer(extract_from_to(s, 27, 33)), as.integer(extract_from_to(s, 34, 36)), as.integer(extract_from_to(s, 37, 43)), as.integer(extract_from_to(s, 44, 44)), as.integer(extract_from_to(s, 45, 45)))
}

stopifnot(all.equal(convert_line_to_data("000002000001288600000010020000009001000018100"), c(12886, 9, 1, 181, 0, 0)))
stopifnot(all.equal(convert_line_to_data("000002000001288700000010020000009003000018200"), c(12887, 9, 3, 182, 0, 0)))

file = file[grepl("^0000020", file)]  # choosing only the mayor votes. 

D = t(sapply(file, convert_line_to_data))  # this takes a while too. 
rownames(D) = NULL
D = as.data.frame(D)
colnames(D) = c("voter_id", "precinct_id", "vote_rank", "candidate_id", "over_vote", "under_vote")

D %>% filter(over_vote == 0 & under_vote == 0 & candidate_id %in% c(179, 180, 181)) %>% 
  arrange(voter_id, vote_rank) %>% 
  group_by(voter_id) %>% 
  distinct(candidate_id) %>%  # can't vote for same person more than once 
  mutate(ballot = paste(candidate_id, collapse = "_")) %>% 
  ungroup() %>% 
  distinct(voter_id, ballot) -> DD

DD %>% 
  mutate(ballot = str_replace(ballot, "(?<=\\d{3}_\\d{3})_\\d+", "")) %>% 
  count(ballot) %>%
  mutate(ballot = str_replace(ballot, "179", "Breed"),
         ballot = str_replace(ballot, "180", "Leno"),
         ballot = str_replace(ballot, "181", "Kim")) -> ballot_counts

ballot_counts %>% pull(n) -> counts
proportions <- counts/sum(counts)
names(proportions) <- ballot_counts %>% pull(ballot)
sf_result <- proportions %>% as.list()
usethis::use_data(sf_result, overwrite = TRUE)
