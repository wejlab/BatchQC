#' Dendrogram alpha or numeric checker
#'
#' This function checks if there is any numeric or strings for plotting legend
#' @param dendro_var column from dendrogram object representing category
#' @import tibble
#' @import ggdendro
#' @import dplyr
#' @return geom_label label for the legend of category variable
#' @examples
#' library(scran)
#' se <- mockSCE()
#' dendro_alpha_numeric_check <- dendrogram_alpha_numeric_check(
#'                                         dendro_var = "Treatment")
#' dendro_alpha_numeric_check
#'
#' @export
#'
dendrogram_alpha_numeric_check <- function(dendro_var) {
    numeric_or_alpha <- !is.na(suppressWarnings(as.numeric((levels(
        factor(dendro_var))))))
    all_numeric <- TRUE

    for (n in numeric_or_alpha) {
        if (n == FALSE) {
            all_numeric <- FALSE
            break
        }
    }

    if (all_numeric) {
        geom_label <- as.character(sort(as.numeric(levels(factor(
            dendro_var)))))

    } else {
        # Get unique 'dendro_var' values, convert to dataframe
        unique_strings <- levels(factor(dendro_var)) %>%
            as.data.frame() %>% rownames_to_column("unique_string_index")

        # Concatenate values from 'unique_strings' with "_"
        label_var_strings <- paste(unlist(unique_strings[1]),
                                        unlist(unique_strings[2]),
                                        sep = " - ") %>%
            as.data.frame() %>% dplyr::rename("label_var" = ".")

        geom_label <- label_var_strings[, "label_var"]
    }

    return(geom_label)
}
