library(scran)
se <- mockSCE(20)

dendrograms <- BatchQC::dendrogram_plotter(se,"counts","Treatment")

dendrograms$dendrogram

dendrograms$circular_dendrogram
