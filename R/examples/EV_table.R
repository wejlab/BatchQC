library(scran)

se <- mockSCE()

EV_table <- BatchQC::EV_table(se, batch = "Mutation_Status",
                                condition = "Treatment",
                                assay_name = "counts")
EV_table
