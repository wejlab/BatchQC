library(scran)

se <- mockSCE()

dendro_alpha_numeric_check <- dendrogram_alpha_numeric_check(
    dendro_category = "Treatment")

dendro_alpha_numeric_check
