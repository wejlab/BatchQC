library(scran)

se <- mockSCE()

DE_res <- BatchQC::DE_analyze(se,
                                "DESeq2",
                                "Mutation_Status",
                                "Treatment",
                                "counts")

volcano <- cbind(DESeq2::results(DE_res$dds)$log2FoldChange,
                    DESeq2::results(DE_res$dds)$pvalue)

plotly::ggplotly(volcano_plot(volcano, -150, 2))
