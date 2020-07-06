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
qplot_votevizr <- function(result, split = "", method = "positional", if_cycle = "empty", s = .5, vertex_varnames = NULL, vertex_labels = NULL, label_offset = .05, padding = .1, show_gridlines = T, show_fp_result = T, fp_cex = 1){
  
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("Package \"ggplot\" needed for qplot_votevizr(). Please install it.",
         call. = FALSE)
  }
  
  if(!requireNamespace("dplyr", quietly = TRUE)){
    stop("Package \"dplyr\" needed for qplot_votevizr(). Please install it.",
         call. = FALSE)
  }
  
  # get the polygons for first-preference win regions
  fpwr_main <- result %>% 
    first_preference_win_regions(split = split, method = method, if_cycle = if_cycle, s = s) 
  
  if(is.null(vertex_varnames)){
    vertex_varnames <- unique(as.character(fpwr_main$candidate))
  }
  
  fpwr_main <- fpwr_main %>%
    dplyr::mutate(x = get(vertex_varnames[1]) + .5*get(vertex_varnames[2]), y = sqrt(3/4)*get(vertex_varnames[2])) # note illicit use of get(): https://stackoverflow.com/questions/50913673/use-of-get-in-dplyr
  
  if(is.null(vertex_labels)){
    vertex_labels <- vertex_varnames
  }
  
  # if have specified a Condorcet completion method that is not handled by
  # first_preference_win_regions(), then we plot 
  # that method first and overlay the Condorcet FP win regions 
  if(grepl("condorcet", method, ignore.case = T) & (grepl("^irv$", if_cycle, ignore.case = T) | grepl("^rcv$", if_cycle, ignore.case = T) | grepl("^positional$", if_cycle, ignore.case = T) | grepl("^borda", if_cycle, ignore.case = T))){
    
    s <- case_when(
      grepl("^plurality", if_cycle, ignore.case = T) ~ 0,
      grepl("^borda", if_cycle, ignore.case = T) ~ .5,
      grepl("^anti[-]?plurality", if_cycle, ignore.case = T) ~ 1,
      TRUE ~ s
    )
    
    fpwr_cycle <- result %>% 
      first_preference_win_regions(split = split, method = if_cycle, s = s) %>% 
      dplyr::mutate(x = get(vertex_varnames[1]) + .5*get(vertex_varnames[2]), y = sqrt(3/4)*get(vertex_varnames[2]))
    fpwr_to_plot <- fpwr_cycle
    overlay_condorcet <- TRUE
  }else{
    fpwr_to_plot <- fpwr_main
    overlay_condorcet <- FALSE
  }
  
  fpwr_to_plot %>% 
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_polygon(ggplot2::aes(group = candidate, fill = candidate), show.legend = F) +
    ggplot2::coord_fixed() + 
    ggplot2::scale_fill_brewer() + 
    ggplot2::theme_void() + 
    ggplot2::expand_limits(x = c(-padding, 1 + padding), y = sqrt(3/4)*c(-padding, 1 + padding)) + 
    ggplot2::annotate(geom = "text", x = c(1,.5,0), y = c(0,sqrt(3/4),0) + label_offset*c(-1,1,-1), label = vertex_labels) -> p
  
  if(overlay_condorcet){
    p <- p + ggplot2::geom_polygon(data = fpwr_main, ggplot2::aes(group = candidate, fill = candidate), show.legend = F)
  }
  
  if(show_fp_result){
    result %>% 
      first_preference_shares(split = split) %>% 
      dplyr::mutate(x = get(vertex_varnames[1]) + .5*get(vertex_varnames[2]), y = sqrt(3/4)*get(vertex_varnames[2])) -> fp_result
    p <- p + ggplot2::geom_point(data = fp_result, ggplot2::aes(x = x, y = y), cex = fp_cex)  
  }
  
  if(show_gridlines){
    p <- p + geom_ternary_gridlines()
  }
  
  p
}
