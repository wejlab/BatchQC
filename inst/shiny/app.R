library(shiny)
library(shinyjs)

source(file.path("utils", "helpers.R"),  local = TRUE)

ui <- navbarPage(
  #title = paste("BatchQC v", packageVersion("BatchQC"), sep = ""),
  title = "BatchQC",
  id="BatchQC",
  fluid=TRUE,
  theme = "bootstrap.min.css",
  source(file.path("ui", "ui_01.R"),  local = TRUE)$value,
  source(file.path("ui", "ui_02.R"),  local = TRUE)$value
)

server <- function(input, output, session) {
  source(file.path("server", "server_01.R"),  local = TRUE)$value
  source(file.path("server", "server_02.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
