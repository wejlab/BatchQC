library(scran)

se <- mockSCE()

confound_table <- BatchQC::confound_metrics(se, batch = "Mutation_Status")
confound_table
