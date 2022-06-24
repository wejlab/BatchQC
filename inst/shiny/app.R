options(shiny.maxRequestSize=600*1024^2)
library(shiny)
library(shinyjs)
library(shinythemes)
library(sva)
library(SummarizedExperiment)
library(data.table)
library(reader)
library(abind)
library(DT)
library(dendextend)
library(circlize)

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
  # source(file.path("ui", "ui_04_median_correlations.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_05_heatmaps.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_06_dendrogram.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_07_pca.R"),  local = TRUE)$value,
  # source(file.path("ui", "ui_08_shape.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_09_differential_expression_analysis.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_10_data_download.R"),  local = TRUE)$value
)

server <- function(input, output, session) {
    reactivevalue <- reactiveValues(counts = NULL,
                                   counts_location = NULL,
                                   metadata = '',
                                   metadata_location = NULL,
                                   se_location = NULL,
                                   se = NULL)
    output$confounding_table <- NULL
    output$metadata <- NULL
    #source(file.path("server/", "server.R"),  local = TRUE)$value
    source(file.path("server/", "server_01_upload.R"), local = TRUE)$value
    source(file.path("server/", "server_02_experimentalDesign.R"), local = TRUE)$value
    source(file.path("server/", "server_03_variationAnalysis.R"), local = TRUE)$value
    source(file.path("server/", "server_04_heatmap.R"), local = TRUE)$value
    source(file.path("server/", "server_05_dendrogram.R"), local = TRUE)$value
    source(file.path("server/", "server_06_pca.R"), local = TRUE)$value
    source(file.path("server/", "server_07_differentialExpression.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
