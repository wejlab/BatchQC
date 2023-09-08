library(scran)

se <- mockSCE()

covariates_not_confounded <- BatchQC::covariates_not_confounded(se,
                                                    batch = "Mutation_Status")
covariates_not_confounded
