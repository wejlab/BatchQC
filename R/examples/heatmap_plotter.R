library(scran)

se <- mockSCE()

heatmaps <- BatchQC::heatmap_plotter(se,
                                    assay = "counts",
                                    nfeature = 15,
                                    annotation_column = c("Mutation_Status",
                                                        "Treatment"))
correlation_heatmap <- heatmaps$correlation_heatmap
correlation_heatmap

heatmap <- heatmaps$topn_heatmap
heatmap
