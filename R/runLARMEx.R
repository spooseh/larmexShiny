#' Run the Shiny App
#'
#' @export
#'
#' @examples
runLARMEx <- function(){
  shiny::shinyApp(ui = app_ui, server = app_server)
}
