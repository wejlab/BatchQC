library(scran)

se <- mockSCE()

se_object_ComBat_Seq <- BatchQC::batch_correct(se, method = "ComBat-Seq",
                                        assay_to_normalize = "counts",
                                        batch = "Mutation_Status",
                                        covar = "Treatment",
                                        output_assay_name =
                                            "ComBat_Seq_Corrected")

se_object_ComBat <- BatchQC::batch_correct(se, method = "Combat",
                                                assay_to_normalize = "counts",
                                                batch = "Mutation_Status",
                                                covar = "Treatment",
                                                output_assay_name =
                                                    "Combat_Corrected")

se_object_ComBat_Seq
se_object_ComBat
