#' Color palette
#'
#' This function creates the base color palette used in BatchQC
#' @param n numeric object representing number of colors to be created
#' @param first_hue numeric object to set the first hue value
#' @param last_hue numeric object to set the final hue value
#' @import tibble
#' @import dplyr
#' @return color_list list of colors generated
#' @examples
#' library(scran)
#' n <- 100
#' color_list <- color_palette(n)
#' color_list
#'
#' @export
color_palette <- function(n, first_hue = 25, last_hue = 360) {
    hues <- seq(first_hue, last_hue, length = n + 1)
    color_list <- grDevices::hcl(h = hues, l = c(25, 75),
                                c = 100)[seq_len(n)]
    return(color_list)
}
