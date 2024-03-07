#' LoadingDataUI user interface part for Loading data widget
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
LoadingDataUI = function(id, column.size=12, label)   {

  requireNamespace("shiny")

ns <-  NS(id)

fluidPage(
  fluidRow(
    column(column.size,
           shinyDirButton(id = ns("datadir"), label = label,title = "Choose directory"),# shinyDirButton
           verbatimTextOutput(ns("dir"), placeholder = TRUE)

    ) #column Input.txt
  ), #fluidrow 1
  hr()
)#fluidpage


} #LoadingDataUI


#' LoadingData server part
#'
#' @param input input
#' @param output output
#' @param session session
#' @import shiny
#' @import shinyFiles
#' @return nothing
#' @export
#'
#' @examples \dontrun{"non"}
#'
LoadingData = function(input, output, session) {
  requireNamespace("shiny")

      ns <- session$ns
      volumes <- c(getVolumes()())

      shinyDirChoose(input, "datadir" ,roots = volumes, session = session, allowDirCreate = F)

      dir <- reactive(input$datadir)

      global <- reactiveValues(datapath = volumes[1])


      observeEvent(ignoreNULL = TRUE,
                   eventExpr = {
                     input$datadir
                   },
                   handlerExpr = {
                     if (!"path" %in% names(dir())) return()

                     global$datapath <- parseDirPath(roots = volumes, input$datadir)


                   })

      output$dir <- renderPrint({global$datapath})

     return(list(
        DirValue = reactive({  global$datapath})
        ))





} #LoadingData
