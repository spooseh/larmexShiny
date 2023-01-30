#####################################################UI
modelUI <- function(id){
  ns <- NS(id)
  opt <- list(names=TRUE)
  fluidRow(
    box(
      title = 'Add\\Remove fixed or random components', width = 12,
      solidHeader=T, collapsible=T, collapsed=T, status = 'primary',
      sidebarLayout(
        sidebarPanel(
          width=12, h6('Network structure: fixed effects'),
          matrixInput(ns('feM'),class='numeric', cols=opt, rows=opt),
          hr(), h6('Network structure: random effects'),
          matrixInput(ns('reM'),class='numeric',  cols=opt, rows=opt),
        ),
        mainPanel()
      )
    ),
    box(
      title = 'Formula', width = 12, solidHeader = TRUE, status = 'primary',
      sidebarLayout(
        sidebarPanel(width=12, verbatimTextOutput(ns('frm'))),
        mainPanel()
      )
    ),
    box(
      title = 'Fit Data', width = 12,solidHeader = TRUE, status = 'primary',
      sidebarLayout(
        sidebarPanel(
          width=12,
          fluidRow(
            column(width=2, shinyDirButton(ns('dir'), "Save to", "Upload")),
            column(width=10,verbatimTextOutput(ns('dirTxt'), placeholder=TRUE))
          ),
          fluidRow(
            column(width=5, selectMenu(ns('fitID'), 'IDs to Fit', mult=T)),
            column(
              width=4, 
              checkboxInput(ns("cnt"), label="Center data", value=T)
            ),
            column(
              width=3,
              div(class='button', style = 'margin-top: 20px',
                  actionButton(ns('fit'),'FIT', class = "btn-fit")
              )
            )
          )
           
        ),
        mainPanel(
          width=12,height=400,
          div(verbatimTextOutput(ns('infoFit'))),#, placeholder=h4('Information here!'))),
          div(style = 'overflow-x: scroll;overflow-x: scroll;', 
              withSpinner(verbatimTextOutput(ns('sumFit')),proxy.height='100px')
          )
        )
      )
    ),
    box(
      width=12,title='Help',solidHeader=T,collapsible=T,collapsed=T,
      includeMarkdown('./www/rmdFit.Rmd')
    )
  )
}
#####################################################Server
modelServer <- function(id, objF, sjID, nDay, nBeep, nAbb, ar, ex){
  moduleServer(id, function(input,output,session){
    observeEvent(c(ar(), ex(), nAbb()),{
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
    
    observeEvent(c(nDay(), nAbb(), input$feM, input$reM),{
      objF$feM <- input$feM
      objF$reM <- input$reM
      setFormulaM(objF)
      frm <- objF$frm
      n = unlist(gregexpr('[(]', frm))
      if(n > 0)
        output$frm <- renderText({
          paste0(substr(frm,1,n-1), '\n   ', substr(frm,n, nchar(frm)))
          })
      else
        output$frm <- renderText({frm})
    })
    
    observeEvent(sjID(),{
      if(sjID()!=''){
        x <- c('All',unique(objF$rawData[objF$sjID]))
        opt <- list(placeholder='Click to choose')
        updateSelectizeInput(session,'fitID',   choices=x, selected=character(), options=opt)
      }
    })
    
    observeEvent(input$fitID,{
      if(input$fitID[1] == 'All')
        objF$fitID <- unique(objF$rawData[, objF$sjID])
      else
        objF$fitID <- input$fitID
    })
    
    output$sumFit <- renderText({
      'Elapsed time will appear here.'
    })
    output$infoFit <- renderText({
      'If successful the ID of the last fitted subject will appear here.'
    })
    shinyDirChoose(input,'dir',
      roots = c(home = '~'),
      filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    fName <- "[res_%d%m%y_%H%M]"
    hDir <- normalizePath("~")
    fSep <- .Platform$file.sep
    objF$savePath <- hDir
    output$dirTxt <- renderText({paste0(objF$savePath, fSep, fName)})
    
    observeEvent(input$dir, {
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
      output$dirTxt <- renderText({paste0(path, fSep, fName)})
    })
    
    observeEvent(input$fit,{
      if(!is_formula(objF$frm)){ 
        showModal(modalDialog(
          title = "",
          HTML("Please import data and set a formula to be fit!")
        ))
        return()
      }
      toDir = file.path(objF$savePath, format(Sys.time(), "res_%d%m%y_%H%M"))
      dir.create(toDir, recursive=T, showWarnings=F)
      objF$savePath <- toDir
      output$dirTxt <- renderText({toDir})
      t0 <- proc.time()
      for(sj in objF$fitID){
        output$infoFit <- renderPrint({
          print(sprintf("Subject %s done!", sj))
        })
        output$sumFit <- renderPrint({
          proc.time()-t0
        })
        fitLmer1(objF, sj, toDir)
        fName <- paste0('re_', sj, '.csv')
        validate(
          need(file.exists(file.path(toDir, fName)), 'Fitting failed!')
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
    
    observeEvent(input$cnt,{
      objF$doCenter <- input$cnt
    })
  })
}