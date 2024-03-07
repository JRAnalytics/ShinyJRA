#' Chose Sampels files UI user interface part for Loading data widget
#'
#' @param id ID to use in the UI
#' @param column.size column size
#' @param label description
#' @import shiny
#' @import shinyFiles
#' @import data.table
#' @return a module of loading data
#' @export
#'
#' @examples \dontrun{"non"}
#'
SamplesFileUI = function(id, column.size=12, label)   {

  requireNamespace("shiny")

  ns <-  NS(id)

  fluidPage(
    fluidRow(
      column(column.size,
             shinyFilesButton(id = ns("SampleFile"), label = label,
                              title = "Choose the Samples File '.txt'",
                              multiple = T)# shinyFileButton

      ) #column Input.txt
    ), #fluidrow 1
    hr()
  )#fluidpage


} #LoadingDataUI

