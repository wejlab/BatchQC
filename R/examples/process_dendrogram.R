library(scran)
se <- mockSCE(20)

dends <- BatchQC::process_dendrogram(se,"counts","Treatment")

dends$dendrogram_segments

dends$dendrogram_ends
