#####################################################UI
importUI <- function(id){
  ns <- NS(id)
  fluidPage(
    box(
      title = 'Import Data', width = 12, solidHeader = TRUE, status = 'primary',
      sidebarLayout(
        sidebarPanel(
          width=12, 
          fileInput(ns('import'), "Choose CSV File", accept = ".csv")
        ),
        mainPanel(width=12,
          h6('First rows from the imported data:'),
          div(style = 'overflow-x: scroll', tableOutput(ns('impView')))
        )
      )
    ),
    box(
      width = 12, solidHeader = F,
      sidebarLayout(
        sidebarPanel(
          width=12,
          fluidRow(
            column(width=4, selectMenu(ns('sjID'),  'ID')),
            column(width=4, selectMenu(ns('nDay'),  'Day\\Week')),
            column(width=4, selectMenu(ns('nBeep'), 'Beep'))
          ),hr(),
          fluidRow(
            column(width=5, selectMenu(ns('arList'), 'Autoregressive', mult=T)),
            column(width=5, selectMenu(ns('exList'), 'Exogenous',      mult=T)),
            column(
              width=2,
              numericInput(
                ns('nAbb'), 
                label ='Abbreviation',
                value=3, min=0, step=1
              )
            )
          )
        ),
        mainPanel()
      )
    ),
    box(
      width=12,title='Help',solidHeader=T,collapsible=T,collapsed=T,
      includeMarkdown('./www/rmdImp.Rmd')
    )
  )
}
#####################################################Server
importServer <- function(id, objF){
  moduleServer(id, function(input,output,session) {
    observeEvent(input$import,{
      file <- input$import
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      objF$rawData <- read.csv(file$datapath)
      if(!is.data.frame(objF$rawData)){
        showModal(modalDialog(
          title = "",
          HTML("Please make sure that the uploaded file can be converted to an 
               R dataframe!")
        ))
        return()
      }
      x <- colnames(objF$rawData)
      updateMenus(session, c("sjID", "nDay", "nBeep", "arList", "exList"), choices=x)
      output$impView <- renderTable({
        head(objF$rawData,3)
      })
    })
    
    observeEvent(c(input$sjID, input$nDay, input$nBeep),{
      objF$sjID <- input$sjID
      objF$nDay <- input$nDay
      objF$nBeep <- input$nBeep
      if(input$sjID!='' & input$nDay!='' & input$nBeep!=''){
        x <- propperLevels(objF)
        id <- x[[1]]
        nD <- x[[2]]
        flag <- x[[3]]
        msg <- x[[4]]
        if(!all(flag)){
          showModal(modalDialog(
            title = "",
            HTML("Please make sure that for each subject every level of \"nDay\\nWeek\"
                 has multiple \"nBeep\" items!. The following is the first problematic
                 encounter in data.", "<br>",
                 "Possible cause:", "<br>",
                 "   Subject ID: ", sprintf("%s", id), "<br>",
                 "   Day\\Week number: ", sprintf("%s", nD), "<br>",
                 "   Beeps: ", msg, "<br>",
                 "There might be further such problems!")
          ))
          if(!flag[1])
            updateSelectizeInput(session,"nDay",  selected=list())
          if(!flag[2])
            updateSelectizeInput(session,"nBeep",  selected=list())
        }else{
          x <- setdiff(colnames(objF$rawData), c(objF$sjID, objF$nDay, objF$nBeep))
          updateMenus(session, c("arList","exList"), choices=x)
        }
      }
    })

    observeEvent(input$nAbb,{
      objF$nAbb <- input$nAbb
    })
  })
}
