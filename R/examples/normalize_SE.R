library(scran)

se <- mockSCE()

se_CPM_normalized <- BatchQC::normalize_SE(se, method = "CPM",
                                    assay_to_normalize = "counts",
                                    output_assay_name =
                                        "CPM_normalized_counts")

se_DESeq_normalized <- BatchQC::normalize_SE(se, method = "DESeq",
                                           assay_to_normalize = "counts",
                                           output_assay_name =
                                               "DESeq_normalized_counts")
se_CPM_normalized
se_DESeq_normalized
