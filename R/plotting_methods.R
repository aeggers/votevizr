## TODO: make the passing of names work. 

votevizr_qplot <- function(result, x_name, y_name, z_name, split = "", method = "plurality", s = 0, if_cycle = "kemeny", ternary = T, expand_amt = .1){
  fpwr <- result %>% 
    first_preference_win_regions(split = split, method = method, if_cycle = if_cycle)
  fps <- result %>% first_preference_shares(split = split)
  
  if(ternary){
    #TODO: names quoting etc
    fpwr <- fpwr %>% mutate(x_name = x_name + .5*y_name, 
                            y_name = sqrt(3/4)*y_name)
    fps <- fps %>% mutate(x_name = x_name + .5*y_name, 
                            y_name = sqrt(3/4)*y_name)
  }
  
  expanded_coords <- c(-expand_amt, 1 + expand_amt)
  
  fpwr %>%
    #TODO: names quoting etc
    ggplot(aes(x = x_name, y = y_name)) +
    geom_polygon(aes(group = candidate, fill = candidate), show.legend = F) +
    coord_fixed() + 
    scale_fill_brewer() + 
    theme_void() + 
    expand_limits(x = expanded_coords, y = sqrt(3/4)*expanded_coords) + 
    #TODO: names quoting etc
    annotate(geom = "text", x = c(1,.5,0), y = c(0,sqrt(3/4),0) + .05*c(-1,1,-1), label = c(x_name, y_name, z_name)) + 
    #TODO: names quoting etc
    geom_point(data = brexit_fps, aes(x = x_name, y = y_name))
}