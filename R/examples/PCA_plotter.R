library(scran)

se <- mockSCE()

se_object_ComBat_Seq <- BatchQC::batch_correct(se, method = "ComBat-Seq",
                                               assay_to_normalize = "counts",
                                               batch = "Mutation_Status",
                                               covar = "Treatment",
                                               output_assay_name =
                                                   "ComBat_Seq_Corrected")

pca_plot <- BatchQC::PCA_plotter(se = se_object_ComBat_Seq,
                                 nfeature = 2, color = "Mutation_Status",
                                 shape = "Treatment",
                                 assays = c("counts", "ComBat_Seq_Corrected"),
                                 xaxisPC = 1, yaxisPC = 2)
pca_plot$plot
pca_plot$var_explained
