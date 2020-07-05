#' Visualize ordinal election result on ternary diagram
#'
#' Given an ordinal election/poll result as a named list, produces 
#'  a ternary diagram showing first-preference win regions and 
#'  the observed first-preference result.  
#'
#' @inheritParams first_preference_win_regions
#' @param vertex_varnames A vector of candidate names (as characters) in 
#' the order in which they are assigned to vertices of the ternary diagram:
#' lower-right, top, lower-left. They must match the candidate names 
#' in \code{result} exactly. If \code{NULL}, they are taken from 
#' \code{result}.
#' @param padding Amount of padding around diagram. Make bigger to use 
#' long \code{vertex_labels}.
#' @param vertex_labels Vertex labels for plotting. If \code{NULL}, taken from \code{vertex_varnames}.
#' @param label_offset How far above/below the
#' vertices should we place the vertex labels?
#' @param show_fp_result If \code{TRUE}, put a dot
#' showing the first-preference shares in \code{result}.
#' @param show_gridlines Add gridlines to the ternary diagram?
#' @param fp_cex \code{cex} parameter for plotting the first preference 
#' result.
#' @export
qplot_votevizr <- function(result, split = "", method = "positional", if_cycle = "empty", s = .5, vertex_varnames = NULL, vertex_labels = NULL, label_offset = .05, padding = .1, show_gridlines = F, show_fp_result = T, fp_cex = 1){
  
  # get the polygons for first-preference win regions
  fpwr <- result %>% 
    first_preference_win_regions(split = split, method = method, if_cycle = if_cycle, s = s)
  
  if(is.null(vertex_varnames)){
    vertex_varnames <- unique(as.character(fpwr$candidate))
  }
  
  if(is.null(vertex_labels)){
    vertex_labels <- vertex_varnames
  }
  
  fpwr %>%
  # we do the transformation by passing 
  mutate(x = get(vertex_varnames[1]) + .5*get(vertex_varnames[2]), y = sqrt(3/4)*get(vertex_varnames[2])) %>%
  ggplot(aes(x = x, y = y)) +
    geom_polygon(aes(group = candidate, fill = candidate), show.legend = F) +
    coord_fixed() + 
    scale_fill_brewer() + 
    theme_void() + 
    expand_limits(x = c(-padding, 1 + padding), y = sqrt(3/4)*c(-padding, 1 + padding)) + 
    annotate(geom = "text", x = c(1,.5,0), y = c(0,sqrt(3/4),0) + label_offset*c(-1,1,-1), label = vertex_labels) -> p
  
  if(show_fp_result){
    result %>% 
      first_preference_shares(split = split) %>% 
      mutate(x = get(vertex_varnames[1]) + .5*get(vertex_varnames[2]), y = sqrt(3/4)*get(vertex_varnames[2])) -> fp_result
    p <- p + geom_point(data = fp_result, aes(x = x, y = y), cex = fp_cex)  
  }
  
  if(show_gridlines){
    p <- p + geom_ternary_gridlines()
  }
  
  p
}
