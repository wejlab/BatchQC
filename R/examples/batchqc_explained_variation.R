library(scran)

se <- mockSCE()

batchqc_explained_variation <- BatchQC::batchqc_explained_variation (se,
                                batch = "Mutation_Status",
                                condition = "Treatment",
                                assay_name = "counts")
batchqc_explained_variation
