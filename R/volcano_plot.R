#' Volcano plot
#'
#' This function allows you to plot DE analysis results as a volcano plot
#' @param volcano_data A dataframe of expression change and p-value data
#' @param pslider Magnitude of significance value threshold
#' @param fcslider Magnitude of expression change value threshold
#' @return A volcano plot of expression change and significane value data
#' @import ggplot2
#' @import scran
#' @example R/examples/volcano_plot.R
#'
#' @export
volcano_plot <- function(volcano_data, pslider, fcslider) {
    volcano_data <- as.data.frame(volcano_data)
    pslider_factor <- pslider

    pslider_cond <- case_when(volcano_data[, 2] < pslider_factor ~ "TRUE",
        volcano_data[, 2] >= pslider_factor ~ "FALSE",
        TRUE ~ 'NA')
    fcslider_factor <- fcslider
    fcslider_cond <- case_when(abs(volcano_data[, 1]) <
            fcslider_factor ~ "FALSE",
        abs(volcano_data[, 1]) >=
            fcslider_factor ~ "TRUE",
        TRUE ~ 'NA')
    filters <- cbind(pslider_cond, fcslider_cond)
    cond <- apply(filters, 1, function(x)(length(which(x == TRUE)) == 2))
    Features <- NULL
    volcano_data <- volcano_data %>% mutate(Features = cond)

    pval <- round(volcano_data[, 1], digits = 2)
    log2fc <- round(-log10(volcano_data[, 2]), digits = 2)
    feature <- volcano_data[, 3]

    p <- ggplot2::ggplot(data = volcano_data,
        aes(x = pval, y = log2fc, text = feature, color = Features)) +
        geom_point() +
        scale_color_manual(values = c('FALSE' = 'blue',
            'TRUE' = 'red',
            'NA' = 'black'),
            labels = c('Threshold failed',
                'All Thresholds passed',
                'NA')) +
        xlab("Change in Expression (log2 fold change)") +
        ylab("Signifigance Value (-log10 p-value)") +
        theme(legend.position = "bottom")

    vol_plot <- p +
        geom_hline(yintercept = -log10(pslider_factor), linetype = "dashed") +
        geom_vline(xintercept = c(-fcslider_factor, fcslider_factor),
            linetype = "dashed")

    vol_plot <- plotly::ggplotly(vol_plot, tooltip = c('x', 'y', 'text'))

    return(vol_plot)
}
