## plotting examples 
# how much can I do without ggtern? 
source("R/first_preference_win_regions.R")
source("R/convert_result_to_vector_of_vote_shares.R")
source("R/condorcet_method_utilities.R")
source("R/first_preference_shares.R")
source("R/positional_method_utilities.R")


sf_result <- list(
  "Breed_Leno" = .149,
  "Breed_Kim" = .108,
  "Breed" = .176,
  "Leno_Breed" = .083,
  "Leno_Kim" = .147,
  "Leno" = .059,
  "Kim_Breed" = .056,
  "Kim_Leno" = .188,
  "Kim" = .034
)

sf_fpwr_kemeny <- first_preference_win_regions(sf_result, split = "_", method = "Condorcet", if_cycle = "kemeny")

sf_fps <- first_preference_shares(sf_result, split = "_")

# standard coordinates 
library(tidyverse)
ggplot(sf_fpwr_kemeny, aes(x = Breed, y = Leno)) + 
  geom_polygon(aes(group = candidate, fill = candidate), show.legend = T, col = "black") + 
  scale_fill_brewer() + 
  labs(x = "Breed first-preference share", y = "Leno first-preference share", fill = "Winning\ncandidate") + 
  geom_point(data = sf_fps, aes(x = Breed, y = Leno))
  
# quasi ternary 
padding <- .05
first_preference_win_regions(sf_result, split = "_", method = "IRV", s = 1, if_cycle = "empty") %>% 
  mutate(Breed = Breed + .5*Leno,
         Leno = sqrt(3/4)*Leno) %>%
ggplot(aes(x = Breed, y = Leno)) + 
  geom_polygon(aes(group = candidate, fill = candidate), show.legend = F, col = "black") + 
  theme_void() + 
  coord_fixed() +
  scale_fill_brewer() + 
  expand_limits(x = c(-.1, 1.1), y = sqrt(3/4)*c(-.1, 1.1)) + 
  annotate(geom = "text", x = c(1,.5,0), y = c(0,sqrt(3/4),0) + padding*c(-1,1,-1), label = c("Breed", "Leno", "Kim")) + 
  geom_point(data = sf_fps %>% mutate(Breed = Breed + .5*Leno,
                                      Leno = sqrt(3/4)*Leno), aes(x = Breed, y = Leno))


library(ggtern)

fpwr <- positional_first_preference_win_regions(brexit_poll, s = .75)
fpvs <- first_preference_shares(brexit_poll)

ggtern(fpwr, aes(x = A, y = B, z = C)) + 
  geom_polygon_closed(aes(group = candidate, fill = candidate), show.legend = F, col = "black") + 
  scale_fill_brewer() + 
  geom_point(data = fpvs, aes(x = A, y = B))
detach("package:ggtern", unload = T)

# can I show the same thing in standard coordinates? 
# yes I can, though stuff is getting masked by ggtern
library(ggplot2)
ggplot(fpwr, aes(x = C, y = B)) + 
  geom_polygon(aes(group = candidate, fill = candidate), show.legend = F, col = "black") + 
  scale_fill_brewer() + 
  geom_point(data = fpvs, aes(x = C, y = B)) + 
  ggplot2::theme_light()

library(ggtern)
fpwr_rcv <- rcv_first_preference_win_regions(brexit_poll, candidate_labels = c("R", "D", "N"))
fpvs <- first_preference_shares(brexit_poll, candidate_labels = c("R", "D", "N"))
ggtern(fpwr_rcv, aes(x = R, y = D, z = N)) + 
  geom_polygon_closed(aes(group = candidate, fill = candidate), show.legend = F, col = "black") + 
  scale_fill_brewer() + 
  geom_point(data = fpvs, aes(x = R, y = D))

fpwr_condorcet <- condorcet_first_preference_win_regions(brexit_poll, candidate_labels = c("R", "D", "N"), if_cycle = "kemeny")
ggtern(fpwr_condorcet, aes(x = R, y = D, z = N)) + 
  geom_polygon_closed(aes(group = candidate, fill = candidate), show.legend = F, col = "black") + 
  scale_fill_brewer() + 
  geom_point(data = fpvs, aes(x = R, y = D))


