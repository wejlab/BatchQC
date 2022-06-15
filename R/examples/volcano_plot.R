library(scran)
se <- mockSCE()

DE_res <- BatchQC::DE_analyze(se,"DESeq2","Treatment","counts")

volcano_plot(DE_res$volcano, -150)