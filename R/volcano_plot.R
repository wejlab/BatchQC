#' Volcano plot
#'
#' This function allows you to plot DE analysis results as a volcano plot
#' @param DE_results a dataframes with the results of one of the DE Analysis;
#' must include "log2Foldchange" and "pvalue" columns
#' @param pslider Magnitude of significance value threshold
#' @param fcslider Magnitude of expression change value threshold
#' @return A volcano plot of expression change and significance value data
#' @import ggplot2
#' @import scran
#'
#' @export
volcano_plot <- function(DE_results, pslider, fcslider) {
    DE_results <- as.data.frame(DE_results)
    pslider_factor <- pslider

    pslider_cond <- case_when(DE_results[, 2] < pslider_factor ~ "TRUE",
        DE_results[, 2] >= pslider_factor ~ "FALSE",
        TRUE ~ 'NA')
    fcslider_factor <- fcslider
    fcslider_cond <- case_when(abs(DE_results[, 1]) <
            fcslider_factor ~ "FALSE",
        abs(DE_results[, 1]) >=
            fcslider_factor ~ "TRUE",
        TRUE ~ 'NA')
    filters <- cbind(pslider_cond, fcslider_cond)
    cond <- apply(filters, 1, function(x)(length(which(x == TRUE)) == 2))
    Features <- NULL
    DE_results <- DE_results %>% mutate(Features = cond)

    pval <- round(DE_results[, 1], digits = 2)
    log2fc <- round(-log10(DE_results[, 2]), digits = 2)
    feature <- DE_results[, 3]

    p <- ggplot2::ggplot(data = DE_results,
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
