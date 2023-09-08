library(scran)

se <- mockSCE()

batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
                            covariate = "Treatment")

batch_design_tibble
