library(scran)

se <- mockSCE()

EV_boxplot <- BatchQC::EV_plotter(se, batch = "Mutation_Status",
                                condition = "Treatment", assay_name = "counts")
EV_boxplot
