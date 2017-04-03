#' Run the Shiny app allowing for interaction with the bracket
#' production given user-entered criteria
#'
#' @author eshayer
#' @export
#' @importFrom shiny runApp

run.app = function() {
  shiny::runApp(system.file('shinyApp', package = 'mRchmadness'))
}
