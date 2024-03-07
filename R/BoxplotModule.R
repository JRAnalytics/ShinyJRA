#' BoxplotUI user interface part for Loading data widget
#'
#' @param id ID to use in the UI
#' @param pheno pheno
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#' @return a module of loading data
#' @export
#'
#' @examples \dontrun{"non"}
#'
BoxplotUI = function(id, Pheno){

  requireNamespace("shiny")
  ns <-  NS(id)

  fluidPage(
  fluidRow(
        column(7,box(title = "Target genes : ",solidHeader = T,status ="warning" ,
                     textAreaInput(ns("cible"), label = NULL,value = "Enter genes symbol (FABP4 or fabp4)...", width = "250"))),
        column(5,
               box(title = "Groupe à analyser",status = "primary",solidHeader = T,footer = "Valeurs catégorielles",
                   selectInput(ns("Gpe"),"",choices = names(Pheno)),
                   h4("Samples sub selection."),

                   awesomeCheckbox(ns("Wrap")," Samples sub selection ?"),

                   uiOutput(ns("Wrap.Gpe")),

                   uiOutput( ns("group.wrap" ))
               ))
      ),
  fluidRow(
    column(8,
           box(title = "You have selected those genes: ",
               verbatimTextOutput(ns("targets")),
               h4("Unretrieved genes"),
               verbatimTextOutput(ns("unretrieved"))))),

  fluidRow(column(5),actionButton(ns("goButton"),"launch analyses", class = "btn-danger")),
  br(),
  fluidRow(class = "myRow2",tags$head(tags$style(".myRow2{height:10px;background-color: grey;}"))),

  fluidRow( #fluid row marche!!
        br(),
        column(1),
        column(5,
               dropdownButton(

                 column(10,offset = 1,

                        tags$h3("Graphic parameters"),
                        tags$h4("Dimmension des plot"),
                        textAreaInput(ns("height"), h5("height (cm)"),"10"),
                        textAreaInput(ns("width"), h5("width (cm)"),"15"),
                        tags$h4("Adjust text size"),
                        fluidRow(actionBttn(ns("c.up.txt"), "",  icon = icon("thumbs-up"))),
                        br(),
                        fluidRow(actionBttn(ns("c.down.txt"), "" , icon = icon("thumbs-down"))),
                        br(),
                        fluidRow(awesomeCheckbox(ns("mul.stat"),"Anova or multiple compareasons (Tukey)?"),
                                 uiOutput(outputId = ns("selectColorBP")))
                 ),
                 circle = TRUE,
                 status = "danger",
                 icon = icon("gear"),
                 width = "400px",
                 tooltip = tooltipOptions(title = "Click to see parameters !")
               )),
        column(7,
               uiOutput(outputId = ns("Boxplots"))),

        column(4,box(downloadButton(ns('downloadData'), 'Download data'), #marche
                     br(),br(),
                     downloadButton(ns('downloadPlot'), "Export Plots"),#marche
                     title = "Download Graphics or Data",status = "success", solidHeader = T))#marche


  ))




} #BoxplotUI


#' BoxplotServer
#'
#' @param input input
#' @param project project
#' @param id id
#' @param DF DF
#' @param pheno description
#' @import shiny
#' @import ggplot2
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
#' @import magrittr
#' @return nothing
#' @export
#'
#'
#' @examples \dontrun{"non"}
#'

#'
BoxplotServer = function(id, project, DF, Pheno, theme) {

  moduleServer(id , function(input, output, session)
    {

  requireNamespace("shiny")

  ns <- session$ns


  InputCible <- eventReactive(input$goButton,{input$cible})


  targets <- reactive({
    x <- unlist(strsplit(InputCible(),"\n"))
    return(toupper(x))
  })


  output$targets <- reactive({return(as.character(targets()))}) #render print targets



  output$Wrap.Gpe <- renderUI({

    if(input$Wrap==T){

      pickerInput(
        inputId = ns("Wrap.Gpe"),
        label = "Which group(s) to study?",
        choices =  colnames(Pheno),
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE, selected = colnames(Pheno)[1]
      )}
  })

  Wrap.Gpe <- reactive({input$Wrap.Gpe})


  observe({print(names(input))})

  output$group.wrap <- renderUI({

    if(input$Wrap==T){


      Wrap.Gpe <-   Wrap.Gpe()
      if(is.null(Wrap.Gpe)){

        Sub.BP <-  "Type"
        choices <- unique(Pheno[,Sub.BP])}  else {

          wrap <- which(colnames(Pheno)%in%Wrap.Gpe)

          choices <- unique(unite(Pheno, Gpe, wrap)[,"Gpe"])
        }


      pickerInput(
        inputId = ns("Sub"),
        label = "Wich samples ?",
        choices =  choices,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE, selected = choices[1]
      )
    }
  })


  Sub.samples <- reactive({

    Wrap.Gpe <- Wrap.Gpe()
    if(is.null(Wrap.Gpe)){Wrap.Gpe <- "Type" }

    Sub <- as.character(input$Sub)
    if(is.null(Sub)){Sub <- "LSEC" }

    wrap <- which(colnames(Pheno)%in%Wrap.Gpe)
    Pheno <- unite(Pheno, Gpe, wrap)

    Sub.samples <- rownames(Pheno[which(Pheno$Gpe%in%Sub),])



    return(as.character(Sub.samples))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(project,"-genesExpression-", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(as.data.frame(MetaData()),row.names = F, file, sep="\t", dec="." )}
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(project,"-plot-", Sys.Date(), ".pptx")
    },
    content = function(file) {



      targets <- as.character(targets())
      targets <- intersect(targets,rownames(DF))

      MetaData <- as.data.frame(MetaData())

      ppt <- read_pptx() %>%
        add_slide(layout="Title Slide", master="Office Theme") %>%
        ph_with(value = "JRA plot export", location = ph_location_type(type = "ctrTitle"))

      Colsel <- Colsel()
      InputWidth <- as.numeric(InputWidth())
      plotBP <- list()

      for (i in targets){

        toplot <- data.frame("gene" = as.numeric(MetaData[,i]), "Gpe" = as.factor(MetaData$Gpe))


        if (length(summary(toplot$Gpe)) == 2) {




          p <- ggplot(toplot, aes(y = gene, x = Gpe, col= Gpe))+
            geom_boxplot()+theme+
            ylab(label= paste0(i, "\n normalized  expression"))+
            xlab(label = "")+ylim(NA,max(toplot$gene)+3)+
            stat_compare_means(method = "t.test" ,label.x =1 , label.y = max(toplot$gene)+0.5)+theme(text = element_text(size = 15+Police.up.BP()*3-Police.down.BP()*3))



          if (!is.null(Colsel[[1]])| !is.null(Colsel[[2]] )) {plotBP[[i]] <-  p + scale_color_manual(breaks = c(levels(MetaData$Gpe)[1], levels(MetaData$Gpe)[2]),
                                                                                                     values=c(Colsel[[1]],Colsel[[2]])) } else { plotBP[[i]] <- p }


        } else {


          p <- ggplot(toplot, aes(y = gene, x = Gpe, col= Gpe))+
            geom_boxplot()+theme+
            ylab(label= paste0(i, "\n normalized expression"))+
            xlab(label = "")+ylim(NA,max(toplot$gene)+1)+
            stat_compare_means(method = "anova", aes(label=..p.adj..), label.x =1.5 , label.y = max(toplot$gene)+0.5)+theme(text = element_text(size = 15+Police.up.BP()*3-Police.down.BP()*3))



          if (is.null(Colsel[[1]])) { plotBP[[i]] <- p }

          else {

            b <- as.character()
            cc <- as.character()
            for (j in 1:length(levels(toplot$Gpe))) { b <- c(b,levels(toplot$Gpe)[j]) }
            for (j in 1:length(Colsel)) { cc <- c(cc, Colsel[[j]])}

            plotBP[[i]] <- p+scale_color_manual(breaks = b,values=cc)

          }
        }
      }
      for (i in targets){
        ppt <- ppt %>%
          add_slide(layout = "Blank", master = "Office Theme") %>%
          ph_with(dml({ print(plotBP[[i]]) }), ph_location(left=0,top=1.5,
                                                           width=(as.numeric(InputWidth())*0.393701),
                                                           height=(as.numeric(InputHeight())*0.393701)
          )
          )

      }

      print(ppt, file)


    }#
  )# doanwloader




  output$unretrieved <- renderPrint({
    ttt <- as.character(targets())
    targets <- as.list(ttt)

    if (length(intersect(rownames(DF), ttt))!=length(targets))
    {
      inter <- data.frame(inter = intersect(rownames(DF), ttt))
      targ <- data.frame(tar = ttt)
      unretrieved <- targ[!targ$tar%in%inter$inter,1]
      print(paste("Not retrieve genes : ", combine_words(unretrieved), ". Running Analysis with only retrieved genes."))
    } else {
      print("All genes were found in the database")}

  })# renderprint unretrived



  output$Boxplots <- renderUI({
    fluidRow(column(10,{
      targets <- as.character(targets())
      targets <- intersect(targets,rownames(DF))

      plot_output_list <- lapply(targets, function(i) {
        plot_BP <- paste(ns("Boxplot"), i, sep = "")
        plotOutput(plot_BP, height = as.numeric(InputHeight())*37.795275591, width = as.numeric(InputWidth())*37.795275591)
      })#plot list

      do.call(tagList, plot_output_list)
    }
    )#coloumn
    )#fluidRow
  })# render UI output boxplot


  MetaData <- reactive({

    targets <-  as.character(targets())
    tDF <- t(DF[targets,])
    MetaData <- cbind("Samples"= rownames(tDF), "Gpe" = Pheno[rownames((tDF)),InputGpe()], tDF)

    if(input$Wrap==T){ MetaData <- MetaData[Sub.samples(),]}


    return(MetaData)
  })# reactive metadata





  InputHeight <- reactive({as.numeric(input$height)})
  InputWidth <- reactive({as.numeric(input$width)})
  InputGpe <-  reactive({input$Gpe})
  mul.stat <- reactive({input$mul.stat})

  Police.up.BP <- reactive({input$c.up.txt})
  Police.down.BP <- reactive({input$c.down.txt})


  ColBP <- reactive({
    MetaData <- as.data.frame(MetaData())
    MetaData$Gpe <- as.factor(MetaData$Gpe)

    x <- lapply(unique(factor(MetaData$Gpe)) ,function(n) {paste0("Color", str_replace_all(n," ", "-"))})
    return(x)
  })

  Colsel <- reactive({
    ColBP <- ColBP()

    x <- lapply(ColBP, function(n) {input[[n]]})
    return(x)
  })


  output$selectColorBP <- renderUI({
    MetaData <- as.data.frame(MetaData())
    MetaData$Gpe <- as.factor(MetaData$Gpe)
    sel <- lapply(unique(factor(MetaData$Gpe)) ,function(n) {

      colSel <- paste0(ns("Color"), str_replace_all(n," ", "-"))
      namecol <- paste(ns("Select color"),n)

      colourInput(colSel, namecol, "green",
                  returnName = TRUE,
                  closeOnClick = TRUE)
    }

    )

    do.call(tagList, sel)

  })


  # render plot BP
  observe({


    targets <- as.character(targets())
    targets <- intersect(targets,rownames(DF))
    MetaData <- as.data.frame(MetaData())
    Gpe <- levels(as.factor(MetaData$Gpe))#selection input



    for (i in targets) {
      local({
        gene <- i
        plot_BP <- paste("Boxplot", i, sep = "")

        toplot <- data.frame("gene" = as.numeric(MetaData[,gene]), "Gpe" = as.factor(MetaData$Gpe))


        Colsel <- Colsel()
        InputWidth <- as.numeric(InputWidth())

        output[[plot_BP]] <- renderPlot({


          if (length(summary(toplot$Gpe)) == 2) {


            G1 <- mean(toplot[toplot$Gpe==Gpe[1],"gene"])
            G2 <- mean(toplot[toplot$Gpe==Gpe[2],"gene"])
            foldchange <- round(G1 - G2,digits = 2)

            p <- ggplot(toplot, aes(y = gene, x = Gpe, col= Gpe))+
              geom_boxplot()+theme+
              ylab(label= paste0(gene, "\n normalized  expression"))+
              xlab(label = "")+ylim(NA,max(toplot$gene)+3)+
              stat_compare_means(method = "t.test" ,label.x =1 , label.y = max(toplot$gene)+0.5)+theme(text = element_text(size = 15+Police.up.BP()*3-Police.down.BP()*3))


            p <- p+
              annotate(geom="text", x=1, y=max(toplot$gene)+2, label=paste(Gpe[1], "vs" ,Gpe[2],"FC =", foldchange),
                       color="red")

            if (!is.null(Colsel[[1]])| !is.null(Colsel[[2]] )) { p + scale_color_manual(breaks = c(levels(MetaData$Gpe)[1], levels(MetaData$Gpe)[2]),
                                                                                        values=c(Colsel[[1]],Colsel[[2]])) }
            else { return(p) }

          } else {
            p <- ggplot(toplot, aes(y = gene, x = Gpe, col= Gpe))+
              geom_boxplot()+theme+
              ylab(label= paste0(gene, "\n normalized expression"))+
              xlab(label = "")

            if(mul.stat()==F){p <- p+ylim(NA,max(toplot$gene)+4)+ stat_compare_means(method = "anova" ,label.x =1 , label.y = max(toplot$gene)+0.5)+
              theme(text = element_text(size = 15+Police.up.BP()*3-Police.down.BP()*3))} else {


              stat.test <- aov(gene ~ Gpe, data = toplot) %>%
                tukey_hsd()



              p <- p+
                ylim(NA,max(toplot$gene)+nlevels(as.factor(toplot$Gpe))*2)+
                stat_pvalue_manual(stat.test, label = "p.adj", y.position = seq_along(levels(as.factor(toplot$Gpe)))*1.5+max(toplot$gene))

            }



            if (is.null(Colsel[[1]])) {

              return(p)

            }
            else {

              b <- as.character()
              cc <- as.character()
              for (i in 1:length(levels(toplot$Gpe))) { b <- c(b,levels(toplot$Gpe)[i])  }
              for (i in 1:length(Colsel)) { cc <- c(cc, Colsel[[i]]) }

              p+scale_color_manual(breaks = b,values=cc)

            }
          }

        })#renderPlot Plot_BP

      })#Local
    } # for i in targets
  })# Observe


  })



} #BoxplotServer
