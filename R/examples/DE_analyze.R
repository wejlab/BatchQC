library(scran)
se <- mockSCE()

# Normalized counts are required for analysis methods other than DESeq2
DE_res <- BatchQC::DE_analyze(se,"DESeq2","Treatment","counts")

DE_res$res
