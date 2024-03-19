#' BoxplotUI user interface part for Loading data widget
#'
#' @param id ID to use in the UI
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#' @return a module of loading data
#' @export
#'
#' @examples \dontrun{"non"}
#'
FC_HeatmapUI = function(id){

  requireNamespace("shiny")
  ns <-  NS(id)

  fluidPage(h2("Heatmap building tool"),
            br(),
            fluidRow(
              column(4,box(
                title="Select a compareason result.",
                pickerInput(
                  inputId = ns("FCresults"),
                  label = "Select one.",
                  choices = list.files("data/FC"),selected = list.files("data/FC")[1]
                ),
                br(),

                tags$h3("Graphic parameters"),
                uiOutput(ns("FCthresold")),
                uiOutput(ns("FCpval")),
                uiOutput(ns("HMAsel")),
                h4("Samples sub selection."),
                awesomeCheckbox(ns("Wrap.FC"),"Samples sub selection ?"),

                uiOutput(ns("Wrap.Gpe.Sub")),

                uiOutput(ns("group.wrap.FC" )),
                br(),
                tags$h4("HM decoration"),
                awesomeCheckbox(ns("HM.Colnames"), "Show samples names?"),
                awesomeCheckbox(ns("HM.Rownames"), "Show genes names?"),
                awesomeCheckbox(ns("HM.Gras"), "Bold text?"),
                awesomeCheckbox(ns("HM.split_column"), "Split HM by samples?"),
                uiOutput(ns("HM.split_column_selUI")),
                br(),
                tags$h4("HM dimensions"),
                textAreaInput(ns("HM.hauteure"), h5("Height (cm)"),"20"),
                textAreaInput(ns("HM.largeure"), h5("Width (cm)"),"20"),


                downloadButton(ns("downloadFC"), "Export Plots"),


                status = "primary",solidHeader = T,
              )),
              column(5,
                     plotOutput(ns("HMplot"))),
              column(2)

              ))






} #BoxplotUI


#' BoxplotServer
#'
#'
#' @param project project
#' @param id id
#' @param DF DF
#' @param pheno description
#' @param FC.geneCol name of colnames correspondant au geneID du DF
#' @import shiny
#' @import tidyr
#' @import ranger
#' @import shinydashboard
#' @import shinyjs
#' @import colourpicker
#' @import gridExtra
#' @import ggrepel
#' @import dplyr
#' @import ggfortify
#' @import cowplot
#' @import rvg
#' @import RColorBrewer
#' @import officer
#' @import knitr
#' @import ComplexHeatmap
#' @import circlize
#' @import stringr
#' @return nothing
#' @export
#'
#'
#' @examples \dontrun{"non"}
#'

#'
FC_HeatmapServer = function(id, project, DF, Pheno, FC.geneCol) {

  moduleServer(id , function(input, output, session)
  {

    requireNamespace("shiny")

    ns <- session$ns



    FC.df <- reactive({
          FC.file <- as.character(input$FCresults)

          FC.df <- readRDS(file = paste0("data/FC/",FC.file))
          return(FC.df)
          }) # reactive FC input file


    output$FCthresold <-   renderUI({
      FC.df <- FC.df()
      return(sliderInput(ns("FCT"),label="Log2 Fold change threshold :",
                         min = 0,
                         max = round(max(FC.df[,"log2FoldChange"]), digits = 2),
                         value = 2))
})


    output$FCpval<-   renderUI({
      FC.df <- FC.df()
      return(sliderInput(ns("FCP"),label="Pvalue threshold : ",
                         min = 0,
                         max = 0.1,step = 0.001,
                         value = 0.05))
    })




    output$HMAsel <- renderUI({

      pickerInput(
        inputId = ns("hma"),
        label = "HM legend selection:",
        choices =  colnames(Pheno),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE, selected = colnames(Pheno)[1]
      )
      })



    HM.split_column <- reactive({input$HM.split_column})





    output$Wrap.Gpe.Sub <- renderUI({

      if(input$Wrap.FC==T){

        pickerInput(
          inputId = ns("Wrap.Gpe.FC"),
          label = "which groups to show ?",
          choices =  colnames(Pheno),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE, selected = colnames(Pheno)[1]
        )}
    })

    Wrap.Gpe.FC <- reactive({input$Wrap.Gpe.FC})


    output$group.wrap.FC <- renderUI({

      if(input$Wrap.FC==T){

        Wrap.Gpe.FC <-   Wrap.Gpe.FC()
        if(is.null(Wrap.Gpe.FC)){
          Wrap.Gpe.FC <-  colnames(Pheno)[1]
          choices <- unique(Pheno[,Wrap.Gpe.FC])
        } else {

          wrap <- which(colnames(Pheno)%in%Wrap.Gpe.FC)

          choices <- unique(tidyr::unite(Pheno, Gpe, wrap)[,"Gpe"])

        }

        pickerInput(
          inputId = ns("Sub.FC"),
          label = "which groups to show ?",
          choices = choices,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE, selected = choices[1]
        )
      }
    })

    Sub.samples.FC <- reactive({

      if(input$Wrap.FC==T) {

        Wrap.Gpe.FC <- Wrap.Gpe.FC()
        if(is.null(Wrap.Gpe.FC)){Wrap.Gpe.FC <- colnames(Pheno)[1]}

        wrap <- which(colnames(Pheno)%in%Wrap.Gpe.FC)
        print("ok0")
        print(wrap)

        Pheno <- tidyr::unite(Pheno, Gpe, wrap)
        print("ok1")
        print(head(Pheno))

        Sub.FC <- as.character(input$Sub.FC)

        print("ok3")
        print(Sub.FC)

        if(is.null(Sub.FC)){ Sub.FC <-  unique(colnames(Pheno)[1])[1]}


        Sub..samples.FC <- rownames(Pheno)[which(Pheno$Gpe%in%Sub.FC)]


        return(as.character(Sub..samples.FC))}})



    HM.annot <- reactive({

      hm.anot <- input$hma

      if(is.null(hm.anot)){ hm.anot <- colnames(Pheno)[1]}

      return(hm.anot)

    })


    pval = reactive({input$FCP})
    FC.thresold = reactive({input$FCT})

    HM.Colnames <- reactive({input$HM.Colnames})
    HM.Rownames <- reactive({input$HM.Rownames})
    HM.Gras <- reactive({input$HM.Gras})
    HM.hauteure <- reactive({input$HM.hauteure})
    HM.largeure <- reactive({input$HM.largeure})


    #####







  annot = reactive({

    req(input$FCresults)
    req(input$FCT)
    req(input$FCP)

    FC.df = FC.df()
    pval = pval()
    FC.thresold = FC.thresold()

    FC = subset(FC.df, abs(log2FoldChange) > FC.thresold & pvalue <pval )
    FC <- FC[which(FC$gene_type=="protein_coding"),]

        HM.annot <- HM.annot()


    if(input$Wrap.FC==T){
    if(length(Sub.samples.FC())==0){} else {DF <- DF[,Sub.samples.FC()]}}

        Pheno <- Pheno[colnames(DF),]


      annot <- data.frame("t" = rep("a", length(rownames(Pheno[,]))),    row.names = rownames(Pheno[,]))
              for (k in HM.annot){
                annot <- cbind(annot,Pheno[rownames(annot),k])

              }
              annot$t <- NULL
              colnames(annot) <- HM.annot



              return(annot)

              })



  output$HM.split_column_selUI <- renderUI({

    req(annot())
    annot = annot()

    HM.split_column <- HM.split_column()

    if(HM.split_column==TRUE) {


     selectInput(width = "auto",
                 inputId = ns("HM.split_column_sel"),
                 label = "HM split by:",
                 choices = colnames(annot))

    }


  })


  plot = reactive({


    FC.df = FC.df()
    pval = pval()
    FC.thresold = FC.thresold()

    annot = annot()

    width = as.numeric(HM.largeure())*37.795275591
    height = as.numeric(HM.hauteure())*37.795275591

    FC = subset(FC.df, abs(log2FoldChange) > FC.thresold & pvalue <pval )

    genes = FC[,FC.geneCol]



    HM.annot <- HM.annot()


    if(input$Wrap.FC==T){
      if(length(Sub.samples.FC())==0){ DF <- DF


      } else {

        DF <- DF[,Sub.samples.FC()]}}


    Pheno <- Pheno[colnames(DF),]


    conti <- colnames(annot)[sapply(annot, class)=="numeric" |sapply(annot, class)== "integer"]
    charac <-  colnames(annot)[!c(sapply(annot, class)=="numeric" | sapply(annot, class)== "integer")]
    if(is.null(charac)){ charac <-  colnames(annot[,sapply(annot, class)=="character"])           }

    Coulours <- c("red" , "green3" , "blue" , "orange"   ,  "magenta", "yellow" ,"cyan", "gray", "black" )


    if(length(conti)==0) {  annot <- annot  } else {

      if (length(charac)==0){ annot <- annot  } else {  annot <- annot[,c(charac,conti)]   }}



    cols <- list()
    if(length(charac)>0) {

      for (i in charac){

        z = unique(annot[,i])


        count = 1
        c <- as.character()

        for (j in z){

          c <- c(c,Coulours[count])

          names(c) <- c(names(c)[names(c)!=""],j)

          count = count+1

        }
        ll <- list(c)
        names(ll) <- i

        cols <- c(cols,ll)

      }}

    if(length(conti)>0) {

      if(length(charac)==0){cols <- list() }

      for (i in conti) {

        if(length(which(is.na(annot[,i])))>0){
          colfun = circlize::colorRamp2(c(min(na.omit(annot[,i])), median(na.omit(annot[,i])),max(na.omit(annot[,i])) ), c("blue", "white", "red"))
        }else {  colfun = circlize::colorRamp2(c(min((annot[,i])), median((annot[,i])),max((annot[,i])) ), c("blue", "white", "red"))   }

        c <- list(colfun)
        names(c) <- i

        cols <- c(cols, c)
      }}




    ha <- HeatmapAnnotation(df = annot, col= cols)


    mat = t(scale(t(na.omit(DF[genes,])), center = T))



    if(input$HM.Rownames==T){   HM.hauteure <- as.numeric(10)*length(genes)/15 } else { HM.hauteure <- as.numeric(HM.hauteure()) }

    if( HM.Gras()==T) { ht_opt(heatmap_column_names_gp = gpar(fontface = "bold"),
                               heatmap_row_names_gp = gpar(fontface = "bold"),
                               legend_labels_gp = gpar(fontface = "bold"),
                               legend_title_gp = gpar(fontface = "bold"),
                               heatmap_column_title_gp = gpar(fontface = "bold"))} else { ht_opt(RESET = T)}


    if(HM.split_column()==T){Split = annot[,input$HM.split_column_sel] }else{Split = NULL}


    plot <-  Heatmap(mat,
                     show_column_names = HM.Colnames(),
                     show_row_names = HM.Rownames(),
                     column_title = paste(as.character(input$FCresults), ": Heatmap, pval <",pval ,"|log2FC| >",FC.thresold,"."),
                     top_annotation  = ha,
                     column_split = Split,
                     heatmap_legend_param = list(title_position = "lefttop-rot"),
                     cluster_columns = T,
                     width = width,
                     height = height,
                     name = "Z score")



    return(plot)




  })



  observe({
    req(input$FCresults)
    req(input$FCT)
    req(input$FCP)

    width = as.numeric(HM.largeure())*37.795275591
    height = as.numeric(HM.hauteure())*37.795275591


    plot = plot()

    output$HMplot = renderPlot({return(plot)},
                               width = as.numeric(width),
                               height = as.numeric(height))


  })


    output$downloadFC <- downloadHandler(

      filename = function() {

        results = gsub(" ","-",input$FCresults)
        if(stringr::str_detect(results, ".rds")){      results = gsub(".rds","",results) }

        paste0(paste0(project,results,"-HM-",Sys.Date(),".pptx"))
      },
      content = function(file) {

        ppt <- read_pptx() %>%
          add_slide(layout="Title Slide", master="Office Theme") %>%
          ph_with(value = "JRA Heatmap export", location = ph_location_type(type = "ctrTitle"))


        plot = plot()

        ppt <- ppt %>%
        add_slide(layout = "Blank", master = "Office Theme") %>%
          ph_with(dml({ print(plot) }), ph_location(ph_location_label(ph_label = "Content Placeholder 2"),left=0,top=0,
                                                      width=(as.numeric(HM.largeure())*0.393701),
                                                      height=(as.numeric(HM.hauteure())*0.393701)))



        print(ppt, file)


      }#
    )# downloader FC plot




})
}
