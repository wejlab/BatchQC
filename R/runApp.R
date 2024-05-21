#' Run BatchQC shiny app
#'
#' @return The shiny app will open
#' @rawNamespace import(shinyjs, except=show)
#' @import shinythemes
#' @param dev Run the application in developer mode
#'
#' @examples
#' if(interactive()){
#' BatchQC()
#' }
#' @export

BatchQC <- function(dev = FALSE) {
    appDir <- system.file("shiny", package = "BatchQC")
    if (appDir == "") {
        stop("Could not find BatchQC. Try re-installing `BatchQC`.",
                call. = FALSE)
    }
    if (dev) {
        options(shiny.autoreload = TRUE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
