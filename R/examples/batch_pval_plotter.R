library(scran)

se <- mockSCE()

DE_res <- BatchQC::DE_analyze(se,
                              "DESeq2",
                              "Mutation_Status",
                              "Treatment",
                              "counts")

batch_pval_plotter <- BatchQC::batch_pval_plotter(DE_res = DE_res)

batch_pval_plotter
