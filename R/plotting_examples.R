## plotting examples 

library(ggtern)

fpwr <- positional_first_preference_win_regions(brexit_poll, s = .75)
fpvs <- first_preference_shares(brexit_poll)

ggtern(fpwr, aes(x = A, y = B, z = C)) + 
  geom_polygon_closed(aes(group = candidate, fill = candidate), show.legend = F, col = "black") + 
  scale_fill_brewer() + 
  geom_point(data = fpvs, aes(x = A, y = B))
detach("package:ggtern", unload = T)

# can I show the same thing in standard coordinates? 
# yes I can, thought stuff is getting masked by ggtern
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


