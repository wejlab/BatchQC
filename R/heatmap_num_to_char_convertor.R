#' Heatmap numeric to character converter
#'
#' This function converts any found numerics to characters
#' @param ann_col column data of heatmap
#' @import tibble
#' @import dplyr
#' @return ann_col modified column data of heatmap
#' @examples
#' library(scran)
#' se <- mockSCE()
#' col_info <- colData(se)
#' ann_col <- heatmap_num_to_char_converter(ann_col = col_info)
#' ann_col
#'
#' @export
#'
heatmap_num_to_char_converter <- function(ann_col) {

    # Loop through each column in ann_col
    for (col_name in names(ann_col)) {

        # Get unique values for the column
        unique_val <- unique(ann_col[[col_name]])

        # Loop through each value in unique_values
        for (i in seq_along(unique_val)) {
            val <- unique_val[i]

            # Check if the value is numeric
            if (is.numeric(as.numeric(val)) && !is.na(as.numeric(val))) {

                # Convert the numeric value to character and add an "_"
                new_val <- paste0(as.character(val))

                # Replace the original value with new_val to that column
                ann_col[ann_col[[col_name]] == val,
                            col_name] <- as.character(new_val)

            } else {
                break
            }
        }
    }
    return(ann_col)
}
