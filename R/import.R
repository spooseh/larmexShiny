#####################################################UI
importUI <- function(id){
  ns <- shiny::NS(id)
  rmdI <- system.file("www", "rmdImp.Rmd", package="larmexShiny")
  shiny::fluidPage(
    shinydashboard::box(
      title = 'Import Data', width = 12, solidHeader = TRUE, status = 'primary',
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width=12,
          shiny::fileInput(ns('import'), "Choose CSV File", accept = ".csv")
        ),
        shiny::mainPanel(
          width=12,
          shiny::h6('First rows from the imported data:'),
          shiny::div(style = 'overflow-x: scroll', shiny::tableOutput(ns('impView')))
        )
      )
    ),
    shinydashboard::box(
      width = 12, solidHeader = F,
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width=12,
          shiny::fluidRow(
            shiny::column(width=4, selectMenu(ns('sjID'),  'ID')),
            shiny::column(width=4, selectMenu(ns('nDay'),  'Day\\Week')),
            shiny::column(width=4, selectMenu(ns('nBeep'), 'Beep'))
          ),shiny::hr(),
          shiny::fluidRow(
            shiny::column(width=5, selectMenu(ns('arList'), 'Autoregressive', mult=T)),
            shiny::column(width=5, selectMenu(ns('exList'), 'Exogenous',      mult=T)),
            shiny::column(
              width=2,
              shiny::numericInput(
                ns('nAbb'),
                label ='Abbreviation',
                value=3, min=0, step=1
              )
            )
          )
        ),
        shiny::mainPanel()
      )
    ),
    shinydashboard::box(
      width=12, title='Help', solidHeader=T, collapsible=T, collapsed=T,
      shiny::includeMarkdown(rmdI)
    )
  )
}
#####################################################Server
importServer <- function(id, objF){
  shiny::moduleServer(id, function(input,output,session) {
    shiny::observeEvent(input$import, {
      file <- input$import
      ext <- tools::file_ext(file$datapath)
      shiny::req(file)
      shiny::validate(shiny::need(ext == "csv", "Please upload a csv file"))
      objF$rawData <- utils::read.csv(file$datapath)
      if(!is.data.frame(objF$rawData)){
        shiny::showModal(shiny::modalDialog(
          title = "",
          shiny::HTML("Please make sure that the uploaded file can be converted to an
               R dataframe!")
        ))
        return()
      }
      x <- colnames(objF$rawData)
      updateMenus(session, c("sjID", "nDay", "nBeep", "arList", "exList"), choices=x)
      output$impView <- shiny::renderTable({
        utils::head(objF$rawData,3)
      })
    })

    shiny::observeEvent(c(input$sjID, input$nDay, input$nBeep),{
      objF$sjID <- input$sjID
      objF$nDay <- input$nDay
      objF$nBeep <- input$nBeep
      testLevels(input, session, objF)
    })

    shiny::observeEvent(input$nAbb,{
      objF$nAbb <- input$nAbb
    })
  })
}
