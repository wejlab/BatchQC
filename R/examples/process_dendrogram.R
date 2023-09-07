library(scran)

se <- mockSCE()

process_dendro <- BatchQC::process_dendrogram(se, "counts")

process_dendro
