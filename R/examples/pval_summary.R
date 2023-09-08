library(scran)

se <- mockSCE()

DE_res <- BatchQC::DE_analyze(se,
                              "DESeq2",
                              "Mutation_Status",
                              "Treatment",
                              "counts")

pval_summary <- BatchQC::pval_summary(DE_res = DE_res)

pval_summary
