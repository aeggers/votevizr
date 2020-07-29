#' Draw boundary for a ternary diagram
#'
#' Adds a triangular boundary to a ggplot object drawn in ternary coordinates. Useful when you don't want to plot the first-preference win regions.
#'
#' @param alpha Opacity of boundary lines.
#' @param col Color of boundary lines.
#' @export
geom_ternary_boundary <- function(alpha = 1, col = "black"){
  
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("Package \"ggplot\" needed for geom_ternary_gridlines(). Please install it.",
         call. = FALSE)
  }
  
  if(!requireNamespace("dplyr", quietly = TRUE)){
    stop("Package \"dplyr\" needed for geom_ternary_gridlines(). Please install it.",
         call. = FALSE)
  }
  
  boundary_df <- data.frame(x = c(1, 0, 0), y = c(0, 1, 0), z = c(0, 0, 1)) %>% dplyr::mutate(x = x + .5*y, y = sqrt(3/4)*y)
  
  ggplot2::geom_polygon(data = boundary_df,
                       ggplot2::aes(x = x, y = y),
                                    show.legend = F, 
                                    alpha = alpha,
                                    col = col,
                       fill = "white")
}
