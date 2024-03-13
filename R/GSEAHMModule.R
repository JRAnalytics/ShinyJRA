#' BoxplotUI user interface part for Loading data widget
#'
#' @param id ID to use in the UI
#' @param Height default 10cm
#' @param Width default 10cm
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinyjs
#' @return a module of loading data
#' @export
#'
#' @examples \dontrun{"non"}
#'
GSEA_HeatmapUI = function(id, Height = 10, Width = 10){

  requireNamespace("shiny")
  ns <-  NS(id)

  fluidPage(h2("GSEA Heatmaps building tool"),
            br(),
            fluidRow(
              column(4,box(
                title="Select a GSEA result.",
                pickerInput(
                  inputId = ns("GSEAresults"),
                  label = "Select one.",
                  choices = list.files("data/GSEA"),selected = list.files("data/GSEA")[1]
                ),
                br(),
                uiOutput(ns("TabGSEA")),
                uiOutput(ns("PathwaySel")),
                tags$h3("Graphic parameters"),
                uiOutput(ns("HMAsel")),
                awesomeCheckbox(ns("HM.Colnames"), "Show samples names?"),
                awesomeCheckbox(ns("HM.Rownames"), "Show genes names?"),
                awesomeCheckbox(ns("HM.Gras"), "Bold text?"),
                awesomeCheckbox(ns("HM.split_column"), "Split HM by samples?"),
                uiOutput(ns("HM.split_column_selUI")),
                br(),
                tags$h4("HM dimensions"),
                textAreaInput(ns("HM.hauteure"), h5("Height (cm)"),Height),
                textAreaInput(ns("HM.largeure"), h5("Width (cm)"),Width),

                h4("Samples sub selection."),
                awesomeCheckbox(ns("Wrap.GSEA"),"Samples sub selection ?"),

                uiOutput(ns("Wrap.Gpe.Sub")),

                uiOutput(ns("group.wrap.GSEA" )),
                downloadButton(ns("downloadGSEA"), "Export Plots"),


                status = "primary",solidHeader = T,
              )),
              column(5,
                     uiOutput(ns("HM"))),
              column(2)

            ))






} #BoxplotUI


#' BoxplotServer
#'
#'
#' @param project project
#' @param id id
#' @param DF DF
#' @param Pheno description
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
GSEA_HeatmapServer = function(id, project, DF, Pheno) {

  moduleServer(id , function(input, output, session)
  {

    requireNamespace("shiny")

    ns <- session$ns



    GSEA.df <- reactive({
      GSEA.file <- as.character(input$GSEAresults)

      GSEA.df <- readRDS(file = paste0("data/GSEA/",GSEA.file))
      return(GSEA.df)
    }) # reactive GSEA input file


    output$TabGSEA <- renderUI({

      req(GSEA.df())

      tab.names = names(GSEA.df())

      pickerInput(
        inputId = ns("TabSel"),
        label = "GSEA results geneSets selection:",
        choices =  tab.names,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ),
        multiple = F, selected = tab.names[1]
      )


    })



    output$PathwaySel <- renderUI({

      req(input$TabSel)
      req(GSEA.df())
      tab = input$TabSel
      GSEA.df =  GSEA.df()[[tab]]

      pickerInput(
        inputId = ns("pathways"),
        label = "pathways selection:",
        choices =  rownames(GSEA.df),
        options = pickerOptions(dropupAuto = F,
                                liveSearch = T),
        multiple = T, selected =  rownames(GSEA.df)[1]
        )

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

      if(input$Wrap.GSEA==T){

        pickerInput(
          inputId = ns("Wrap.Gpe.GSEA"),
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

    Wrap.Gpe.GSEA <- reactive({input$Wrap.Gpe.GSEA})


    output$group.wrap.GSEA <- renderUI({

      if(input$Wrap.GSEA==T){

        Wrap.Gpe.GSEA <-   Wrap.Gpe.GSEA()
        if(is.null(Wrap.Gpe.GSEA)){
          Wrap.Gpe.GSEA <-  colnames(Pheno)[1]
          choices <- unique(Pheno[,Wrap.Gpe.GSEA])
        } else {

          wrap <- which(colnames(Pheno)%in%Wrap.Gpe.GSEA)

          choices <- unique(tidyr::unite(Pheno, Gpe, wrap)[,"Gpe"])

        }

        pickerInput(
          inputId = ns("Sub.GSEA"),
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

    Sub.samples.GSEA <- reactive({

      if(input$Wrap.GSEA==T) {

        Wrap.Gpe.GSEA <- Wrap.Gpe.GSEA()
        if(is.null(Wrap.Gpe.GSEA)){Wrap.Gpe.GSEA <- colnames(Pheno)[1]}

        wrap <- which(colnames(Pheno)%in%Wrap.Gpe.GSEA)
        Pheno <- tidyr::unite(Pheno, Gpe, wrap)

        Sub.GSEA <- as.character(input$Sub.GSEA)


        if(is.null(Sub.GSEA)){ Sub.GSEA <-  unique(colnames(Pheno)[1])[1]}

        Sub..samples.GSEA <- rownames(Pheno[which(Pheno$Gpe%in%Sub.GSEA),])


        return(as.character(Sub..samples.GSEA))}})



    HM.annot <- reactive({

      hm.anot <- input$hma

      if(is.null(hm.anot)){ hm.anot <- colnames(Pheno)[1]}

      return(hm.anot)

    })


    HM.Colnames <- reactive({input$HM.Colnames})
    HM.Rownames <- reactive({input$HM.Rownames})
    HM.Gras <- reactive({input$HM.Gras})
    HM.hauteure <- reactive({if(is.na(input$HM.hauteure)|input$HM.hauteure==""){return(10)} else {return(input$HM.hauteure)}})
    HM.largeure <- reactive({if(is.na(input$HM.largeure)|input$HM.largeure==""){return(10)} else {return(input$HM.largeure)}})


    #####







    annot = reactive({

      req(input$GSEAresults)

      HM.annot <- HM.annot()


      if(input$Wrap.GSEA==T){
        if(length(Sub.samples.GSEA())==0){} else {DF <- DF[,Sub.samples.GSEA()]}}

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

    H.HM = reactiveValues()




##### ici : faire le plot list avec l'app original

    output$HM <-  renderUI({

      fluidRow(column(5,{

        req(input$TabSel)
        req(HM.hauteure())
        req(HM.largeure())
        pathways = input$pathways
        TabSel = input$TabSel

        gsea.file <- as.character(input$GSEAresult)
        GSEA.df <- GSEA.df()
        x <-  as.data.frame(GSEA.df[[TabSel]])
        rownames(x) <- x$pathway
        gene.list <- x[pathways,"leadingEdge"]
        names(gene.list) <- pathways


        plot_output_list <- lapply(pathways, function(i) {

          if(input$HM.Rownames==T){
            ll = length(gene.list[[i]])
            if(ll<5) { ll = 5}
            HM.hauteure <- as.numeric(10)*ll/15


            } else { HM.hauteure <- as.numeric(HM.hauteure()) }
          plot_HM <- paste0("HM.", i)

          plotOutput(ns(plot_HM),
                     height = paste0(as.numeric(HM.hauteure)*37.795275591,"px"),
                     width = paste0(as.numeric(HM.largeure())*37.795275591,"px"))}


        )#plot list



        for(i in pathways){

          if(input$HM.Rownames==T){
            ll = length(gene.list[[i]])
            if(ll<5) { ll = 5}
            HM.hauteure <- as.numeric(10)*ll/15


          } else { HM.hauteure <- as.numeric(HM.hauteure()) }


          H.HM[[paste0("HM.", i)]] = HM.hauteure
        }

        do.call(tagList, plot_output_list)
      }
      )#coloumn
      )#fluidRow
    })# render UI output HM


    plotHM =   reactiveValues()


observe({


      pathways = input$pathways
      annot = annot()

      width = as.numeric(HM.largeure())*37.795275591
      height = as.numeric(HM.hauteure())*37.795275591



      list.plot = list()

      for (pathways in pathways) {
      local({

          gsea.file <- as.character(input$GSEAresult)
          TabSel = input$TabSel
          GSEA.df = GSEA.df()
          x <-  as.data.frame(GSEA.df[[TabSel]])
          rownames(x) <- x$pathway

          gene.list <- x[pathways,"leadingEdge"]
          names(gene.list) <- pathways




      if(input$Wrap.GSEA==T){
        if(length(Sub.samples.GSEA())==0){ DF <- DF


        } else {

          DF <- DF[,Sub.samples.GSEA()]}}


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



      mat = t(scale(t(na.omit(DF[unlist(gene.list),rownames(annot)])), center = T))

      ha <- HeatmapAnnotation(df = annot, col= cols)


      if(input$HM.Rownames==T){   HM.hauteure <- as.numeric(10)*length(unlist(gene.list))/15 } else { HM.hauteure <- as.numeric(HM.hauteure()) }

      if( HM.Gras()==T) { ht_opt(heatmap_column_names_gp = gpar(fontface = "bold"),
                                 heatmap_row_names_gp = gpar(fontface = "bold"),
                                 legend_labels_gp = gpar(fontface = "bold"),
                                 legend_title_gp = gpar(fontface = "bold"),
                                 heatmap_column_title_gp = gpar(fontface = "bold"))} else { ht_opt(RESET = T)}


      if(HM.split_column()==T){Split = annot[,input$HM.split_column_sel] }else{Split = NULL}



     title = gsub("_", " ", pathways)
     title = gsub("%", " ", title)
     title =  paste(strwrap(title,width = width/10),collapse = "\n")


      p <-  Heatmap(mat,
                       show_column_names = HM.Colnames(),
                       show_row_names = HM.Rownames(),
                       column_title = title,
                       top_annotation  = ha,
                       column_split = Split,
                       heatmap_legend_param = list(title_position = "lefttop-rot"),
                       cluster_columns = T,
                       width = width,
                       height = height,
                       name = "Z score")

      p2 = list(p)
      names(p2) = paste0("HM.", pathways)
      list.plot = append(list.plot,p2)


      output[[paste0("HM.", pathways)]] <- renderPlot({return(p2)})


      print(paste("names in local",names(list.plot) ))

      plotHM[[paste0("HM.", pathways)]] = p


})#local
      }# for i in pathways
})




    output$GSEA.courbes <- renderUI({

      fluidRow(column(3,{
        pathway <-  input$pathways
        plot_output_list <- lapply(pathway, function(i) {

          plot_GSEA_courbes <- paste0("GSEA.courbe", i)

          plotOutput(ns(plot_GSEA_courbes),  height = paste0(as.numeric(HM.hauteure())*37.795275591,"px"))

        })
        do.call(tagList, plot_output_list)}))

    })


        output$downloadGSEA <- downloadHandler(

      filename = function() {

        results = gsub(" ","-",input$GSEAresults)
        if(stringr::str_detect(results, ".rds")){      results = gsub(".rds","",results) }

        paste0(paste0(project,results,"-GSEA.HM-",Sys.Date(),".pptx"))
      },
      content = function(file) {

        ppt <- read_pptx() %>%
          add_slide(layout="Title Slide", master="Office Theme") %>%
          ph_with(value = "JRA GSEA Heatmap", location = ph_location_type(type = "ctrTitle"))







        for (i in names(plotHM)){


        ppt <- ppt %>%
          add_slide(layout = "Blank", master = "Office Theme") %>%
          ph_with(dml({ print(plotHM[[i]]) }), ph_location(ph_location_label(ph_label = "Content Placeholder 2"),left=0,top=0,
                                                    width=(as.numeric(HM.largeure())*0.393701),
                                                    height=(as.numeric(H.HM[[i]]*0.393701))))


        }
        print(ppt, file)


      }#
    )# downloader GSEA plot




  })
}
