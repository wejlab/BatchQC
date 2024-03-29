% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_statistics.R
\name{std_pearson_corr_coef}
\alias{std_pearson_corr_coef}
\title{Calculate a standardized Pearson correlation coefficient}
\usage{
std_pearson_corr_coef(bd)
}
\arguments{
\item{bd}{batch design}
}
\value{
standardized Pearson correlation coefficient
}
\description{
Calculate a standardized Pearson correlation coefficient
}
\examples{
library(scran)
se <- mockSCE()
batch_design_tibble <- batch_design(se, batch = "Mutation_Status",
                                                covariate = "Treatment")
pearson_cor_result <- BatchQC::std_pearson_corr_coef(batch_design_tibble)
pearson_cor_result

}
