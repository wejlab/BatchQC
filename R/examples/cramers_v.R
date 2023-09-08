library(scran)

se <- mockSCE()

batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
                                    covariate = "Treatment")

cramers_v_result <- BatchQC::cramers_v(batch_design_tibble)
cramers_v_result
