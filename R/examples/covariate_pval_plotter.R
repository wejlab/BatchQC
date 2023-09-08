library(scran)

se <- mockSCE()

DE_res <- BatchQC::DE_analyze(se,
                              "DESeq2",
                              "Mutation_Status",
                              "Treatment",
                              "counts")

covariate_pval_plotter <- BatchQC::covariate_pval_plotter(DE_res = DE_res)

covariate_pval_plotter
