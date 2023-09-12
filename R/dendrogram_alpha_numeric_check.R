#' Dendrogram alpha or numeric checker
#'
#' This function checks if there is any numeric or strings for plotting legend
#' @param dendro_category column from dendrogram object representing category
#' @import tibble
#' @import ggdendro
#' @import dplyr
#' @return geom_label label for the legend of category variable
#' @examples
#' library(scran)
#' se <- mockSCE()
#' dendro_alpha_numeric_check <- dendrogram_alpha_numeric_check(
#'                                         dendro_category = "Treatment")
#' dendro_alpha_numeric_check
#'
#' @export
#'
dendrogram_alpha_numeric_check <- function(dendro_category) {
    numeric_or_alpha <- !is.na(suppressWarnings(as.numeric((levels(
        factor(dendro_category))))))
    all_numeric <- TRUE

    for (n in numeric_or_alpha) {
        if (n == FALSE) {
            all_numeric <- FALSE
            break
        }
    }

    if (all_numeric) {
        geom_label <- as.character(sort(as.numeric(levels(factor(
            dendro_category)))))

    } else {
        # Get unique 'category_var' values, convert to dataframe
        unique_strings <- levels(factor(dendro_category)) %>%
            as.data.frame() %>% rownames_to_column("unique_string_index")

        # Concatenate values from 'unique_strings' with "_"
        label_category_strings <- paste(unlist(unique_strings[1]),
                                        unlist(unique_strings[2]),
                                        sep = " - ") %>%
            as.data.frame() %>% dplyr::rename("category_val" = ".")

        geom_label <- label_category_strings[, "category_val"]
    }

    return(geom_label)
}
