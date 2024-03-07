#' ediTable
#'
#' @param id id in shiny app
#' @param ... other rHandsontableOutput parametters (try ?rHandsontableOutput)
#' @import rhandsontable
#' @return ui output
#' @export
#'
#' @examples "non"

ediTable <- function(id, ...) {
  ns <- NS(id)
  rHandsontableOutput(outputId = ns("SampleTable"), ...)

}







#' ediTable_server
#'
#' @param id id in shiny app
#' @param rd endred tble
#' @param allowRowEdit default T
#' @param allowColumnEdit default F
#' @param manualRowMove default T
#' @param ... other rhandsontable parametters (try ?rhandsontable)
#' @import rhandsontable
#' @return server outputs
#' @export
#'
#' @examples "non"

ediTable_server <-
  function(id,
           rd,
           allowRowEdit = TRUE,
           allowColumnEdit = T,
           manualRowMove = TRUE,
           ...) {
    moduleServer(id,
                 function(input, output, session) {

                   output$SampleTable <- renderRHandsontable({
                     tmp <- isolate(rd())#Gotta isolate it or it'll cause infinite loop
                     #Necessary to avoid the issue described [here](https://github.com/jrowen/rhandsontable/issues/166)
                     rownames(tmp) <- NULL
                     rhandsontable(
                       tmp,
                       allowRowEdit = allowRowEdit,
                       allowColumnEdit = allowColumnEdit,
                       manualRowMove = manualRowMove,
                       rowHeaders = as.character((1:nrow(rd()))),
                       ...
                     )

                   })

                   #Update the reactive values for this user-manipulated data to pass back to main environment
                   observeEvent(input$SampleTable, {

                     tmp <- rhandsontable::hot_to_r(input$SampleTable)

                     rd(tmp)


                   })

                 })
  }

