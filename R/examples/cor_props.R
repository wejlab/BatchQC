library(scran)

se <- mockSCE()

batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
                                    covariate = "Treatment")

correlation_property <- BatchQC::cor_props(batch_design_tibble)

correlation_property
