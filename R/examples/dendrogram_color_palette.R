library(scran)

se <- mockSCE()

process_dendro <- BatchQC::process_dendrogram(se, "counts")

dendrogram_ends <- process_dendro$dendrogram_ends
col <- process_dendro$condition_var

dendro_colors <- dendrogram_color_palette(col = "Treatment",
                                        dendrogram_info = dendrogram_ends)

dendro_colors
