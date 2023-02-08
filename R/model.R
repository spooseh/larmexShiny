#####################################################UI
modelUI <- function(id){
  ns <- shiny::NS(id)
  rmdF <- system.file("www", "rmdFit.Rmd", package="larmexShiny")
  opt <- list(names=TRUE)
  shiny::fluidRow(
    shinydashboard::box(
      title = 'Add\\Remove fixed or random components', width = 12,
      solidHeader=T, collapsible=T, collapsed=T, status = 'primary',
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width=12, shiny::h6('Network structure: fixed effects'),
          shinyMatrix::matrixInput(ns('feM'),class='numeric', cols=opt, rows=opt),
          shiny::hr(), shiny::h6('Network structure: random effects'),
          shinyMatrix::matrixInput(ns('reM'),class='numeric',  cols=opt, rows=opt),
        ),
        shiny::mainPanel()
      )
    ),
    shinydashboard::box(
      title = 'Formula', width = 12, solidHeader = TRUE, status = 'primary',
      shiny::sidebarLayout(
        shiny::sidebarPanel(width=12, shiny::verbatimTextOutput(ns('frm'))),
        shiny::mainPanel()
      )
    ),
    shinydashboard::box(
      title = 'Fit Data', width = 12, solidHeader = TRUE, status = 'primary',
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width=12,
          shiny::fluidRow(
            shiny::column(width=2, shinyFiles::shinyDirButton(ns('dir'), "Save to", "Upload")),
            shiny::column(width=10, shiny::verbatimTextOutput(ns('dirTxt'), placeholder=TRUE))
          ),
          shiny::fluidRow(
            shiny::column(width=5, selectMenu(ns('fitID'), 'IDs to Fit', mult=T)),
            shiny::column(
              width=4,
              shiny::checkboxInput(ns("cnt"), label="Center data", value=T)
            ),
            shiny::column(
              width=3,
              shiny::div(class='button', style = 'margin-top: 20px',
                         shiny::actionButton(ns('fit'),'FIT', class = "btn-fit")
              )
            )
          )

        ),
        shiny::mainPanel(
          width=12,height=400,
          shiny::div(shiny::verbatimTextOutput(ns('infoFit'))),#, placeholder=h4('Information here!'))),
          shiny::div(style = 'overflow-x: scroll;overflow-x: scroll;',
                     shinycssloaders::withSpinner(shiny::verbatimTextOutput(ns('sumFit')), proxy.height='100px')
          )
        )
      )
    ),
    shinydashboard::box(
      width=12, title='Help', solidHeader=T, collapsible=T, collapsed=T,
      shiny::includeMarkdown(rmdF)
    )
  )
}
#####################################################Server
modelServer <- function(id, objF, sjID, nDay, nBeep, nAbb, ar, ex){
  shiny::moduleServer(id, function(input,output,session){
    shiny::observeEvent(c(ar(), ex(), nAbb()),{
      if(is.null(ar()))
        objF$arList <- list()
      else
        objF$arList <- ar()
      if(is.null(ex()))
        objF$exList <- list()
      else
        objF$exList <- ex()
      objF$nAbb <- nAbb()
      doAbbreviate(objF)
      if(length(objF$arList) > 0){
        genNetMat('feM',objF, session)
        genNetMat('reM',objF, session)
      }
    })

    shiny::observeEvent(c(nDay(), nAbb(), input$feM, input$reM),{
      objF$feM <- input$feM
      objF$reM <- input$reM
      setFormulaM(objF)
      frm <- objF$frm
      n = unlist(gregexpr('[(]', frm))
      if(n > 0)
        output$frm <- shiny::renderText({
          paste0(substr(frm,1,n-1), '\n   ', substr(frm,n, nchar(frm)))
          })
      else
        output$frm <- shiny::renderText({frm})
    })

    shiny::observeEvent(sjID(),{
      if(sjID()!=''){
        x <- c('All',unique(objF$rawData[objF$sjID]))
        opt <- list(placeholder='Click to choose')
        shiny::updateSelectizeInput(session,'fitID',   choices=x, selected=character(), options=opt)
      }
    })

    shiny::observeEvent(input$fitID,{
      if(input$fitID[1] == 'All')
        objF$fitID <- unique(objF$rawData[, objF$sjID])
      else
        objF$fitID <- input$fitID
    })

    output$sumFit <- shiny::renderText({
      'Elapsed time will appear here.'
    })
    output$infoFit <- shiny::renderText({
      'If successful the ID of the last fitted subject will appear here.'
    })

    shinyFiles::shinyDirChoose(input,'dir',
      roots = c(home = '~'),
      filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    fName <- "[res_%d%m%y_%H%M]"
    hDir <- normalizePath("~")
    fSep <- .Platform$file.sep
    objF$savePath <- hDir
    output$dirTxt <- shiny::renderText({paste0(objF$savePath, fSep, fName)})

    shiny::observeEvent(input$dir, {
      if (!"path" %in% names(input$dir))
        path <- hDir
      else{
        cDir <- unlist(input$dir$path[-1])
        if(length(cDir) > 0)
          path <- file.path(hDir, paste(cDir, collapse=fSep))
        else
          path <- hDir
      }
      objF$savePath <- path
      output$dirTxt <- shiny::renderText({paste0(path, fSep, fName)})
    })

    shiny::observeEvent(input$fit,{
      if(!is_formula(objF$frm)){
        shiny::showModal(shiny::modalDialog(
          title = "",
          shiny::HTML("Please import data and set a formula to be fit!")
        ))
        return()
      }
      toDir = file.path(objF$savePath, format(Sys.time(), "res_%d%m%y_%H%M"))
      dir.create(toDir, recursive=T, showWarnings=F)
      objF$savePath <- toDir
      output$dirTxt <- shiny::renderText({toDir})
      t0 <- proc.time()
      for(sj in objF$fitID){
        output$infoFit <- shiny::renderPrint({
          print(sprintf("Subject %s done!", sj))
        })
        output$sumFit <- shiny::renderPrint({
          proc.time()-t0
        })
        fitLmer1(objF, sj, toDir)
        fName <- paste0('re_', sj, '.csv')
        shiny::validate(
          shiny::need(file.exists(file.path(toDir, fName)), 'Fitting failed!')
        )
      }
    })

    # observeEvent(input$feM,{
    #   objF$feM <- input$feM
    # })
    #
    # observeEvent(input$reM,{
    #   objF$reM <- input$reM
    # })

    shiny::observeEvent(input$cnt,{
      objF$doCenter <- input$cnt
    })
  })
}
