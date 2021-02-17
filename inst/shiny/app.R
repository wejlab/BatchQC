library(shiny)
library(shinyjs)
library(shinythemes)

source(file.path("utils", "helpers.R"),  local = TRUE)

ui <- navbarPage(

  # title = paste("BatchQC v", packageVersion("BatchQC"), sep = ""),

  title = "BatchQC",
  id="BatchQC",
  fluid=TRUE,
  theme = shinytheme("yeti"),
  # theme = "bootstrap.min.css",
  source(file.path("ui", "ui_01_upload.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_02_variation.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_03_diff_exp.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_04_median_correlations.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_05_heatmaps.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_06_circular_dendogram.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_07_pca.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_08_shape.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_09_combat.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_10_sva.R"),  local = TRUE)$value
)

server <- function(input, output, session) {
  source(file.path("server", "server_01.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
