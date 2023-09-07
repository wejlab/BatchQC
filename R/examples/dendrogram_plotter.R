library(scran)

se <- mockSCE()

dendrogram_plot <- BatchQC::dendrogram_plotter(se,
                                               "counts",
                                               "Mutation_Status",
                                               "Treatment")

dendrogram_plot$dendrogram
dendrogram_plot$circular_dendrogram
