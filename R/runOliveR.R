#' @title Launches the web Shiny app "OliveR"
#'
#' @description Best package in the world
#'
#' @param host optional parameter where to execute the application, default is 127.0.0.1
#'
#' @param ui R script containing the code of the user interface of the shiny app
#'
#' @param server R script containing the code of the server of the shiny app
#'
#' @import shiny
#'
#' @return The function loads the web application OliveR in your default browser
#' through the \code{\link{shinyApp}} function, which requires a script with the
#' User Interface of the app and a script of the Server.
#'
#' @export runOliveR
#'
#' @examples
#' \dontrun{
#' runOliver()
#' }

runOliveR <- function() {

  shinyApp(ui = OliveR_UI, server = OliveR_SERVER)

}
