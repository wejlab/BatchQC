library(scran)

se <- mockSCE()

batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
                                    covariate = "Treatment")

pearson_cor_result <- BatchQC::std_pearson_corr_coef(batch_design_tibble)

pearson_cor_result
