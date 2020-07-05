#' Draw gridlines on a ternary diagram
#'
#' Adds a layer to a ggplot object drawn in ternary coordinates. 
#'
#' @param at A vector of the points at which the gridlines should be drawn. 
#' @param line_overhang By how much should the gridlines overhang the edge of the ternary triangle?
#' @param text_overhang How far from the edge of the ternary triangle should the text labels be? 
#' @param alpha Opacity of gridlines.
#' @param col Color of gridlines.
#' @param size Size of text labels.
#' @export
geom_ternary_gridlines <- function(at = (1:3)/4, line_overhang = .05, text_overhang = .1, alpha = .75, col = "darkgray", size = 2){
  
  line_df <- gridlines_df(vals = at, overhang = line_overhang) %>% mutate(x = x + .5*y, y = sqrt(3/4)*y)
  
  text_df <- gridlines_df(vals = at, overhang = text_overhang) %>% 
    # ternary transformation
    mutate(x = x + .5*y, y = sqrt(3/4)*y) %>% 
    # label only half of the endpoints
    distinct(vertex, value, .keep_all = T) 
  
  list(
    geom_line(data = line_df,
              aes(x = x, y = y,
                  linetype = vertex, #TODO: make optional
                  group = interaction(vertex, value)),
              show.legend = F, 
              alpha = alpha, 
              col = col),
    geom_text(data = text_df,
              aes(x = x, y = y, label = value), 
              size = size)
  )
  
}


one_line <- function(val, overhang = .05){
  out <- data.frame(
    rbind(c(val, 1 - val + overhang, -overhang),
          c(val, -overhang, 1 - val + overhang)))
  colnames(out) <- c("x", "y", "z")
  out
}

one_df <- function(val, overhang = .05){
  data <- tibble(value = val,
                 one_line(val, overhang))
  data
}  

gridlines_df_for_one <- function(vals, overhang = .05){
  tibble(val = vals) %>% 
    purrr::pmap(one_df, overhang = overhang) %>% 
    bind_rows()
}

gridlines_df <- function(vals = c(.25, .5, .75), overhang = .05){
  g1 <- gridlines_df_for_one(vals, overhang = overhang)
  bind_rows(
    g1 %>% mutate(vertex = "x"),
    g1 %>% mutate(vertex = "y") %>% 
      select(vertex, value, x = y, y = x, z),
    g1 %>% mutate(vertex = "z") %>% 
      select(vertex, value, x = z, y, z = x)
  ) %>% select(vertex, everything())
}