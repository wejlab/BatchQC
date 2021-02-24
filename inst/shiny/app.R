library(shiny)
library(shinyjs)
library(shinythemes)
require(SummarizedExperiment)
require(pheatmap)
require(ggplot2)
require(plotly)
require(EBSeq)
require(data.table)
require(reader)

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
  source(file.path("ui", "ui_04_median_correlations.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_05_heatmaps.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_06_circular_dendogram.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_07_pca.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_08_shape.R"),  local = TRUE)$value
)

server <- function(input, output, session) {
  source(file.path("server/", "server.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
