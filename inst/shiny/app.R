# library(shiny)
# library(shinyjs)
# library(shinythemes)
# require(SummarizedExperiment)
# require(pheatmap)
# require(ggplot2)
# require(plotly)
# require(EBSeq)
# require(data.table)
# require(reader)
# library(dplyr)
# library(tidyr)

source(file.path("utils", "helpers.R"),  local = TRUE)

ui <- navbarPage(

  # title = paste("BatchQC v", packageVersion("BatchQC"), sep = ""),

  title = "BatchQC",
  id="BatchQC",
  fluid=TRUE,
  theme = shinytheme("yeti"),
  # theme = "bootstrap.min.css",
  source(file.path("ui", "ui_01_upload.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_02_experimental_design.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_03_variation.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_04_batch_effect_correction.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_05_median_correlations.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_06_heatmaps.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_07_circular_dendogram.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_08_pca.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_09_shape.R"),  local = TRUE)$value
)

server <- function(input, output, session) {
  source(file.path("server/", "server.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
