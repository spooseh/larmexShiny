app_ui <- function(){
  title <- 'Exogenous Linear Autoregressive Mixed Effects, LARMEx'
  header  <- shinydashboard::dashboardHeader(title = title, titleWidth = 650)
  sidebar <- shinydashboard::dashboardSidebar(
    width = 100, collapsed = F,
    shinydashboard::sidebarMenu(
      id = "tabs", style = "position: fixed; overflow: visible;",
      shinydashboard::menuItem(shiny::div(class = 'menu1', "Home"),  tabName = "HLP"),
      shinydashboard::menuItem(shiny::div(class = 'menu1', "Data"),  tabName = "IMP"),
      shinydashboard::menuItem(shiny::div(class = 'menu1', "Model"), tabName = "FIT")
    )
  )
  rmdH <- system.file("www", "rmdHome.Rmd", package="larmexShiny")
  rmdT <- system.file("www", "mathSum.html", package="larmexShiny")
  rmdA <- system.file("www", "rmdAbout.Rmd", package="larmexShiny")
  homeUI <- shiny::fluidPage(
    shiny::tabsetPanel(
      shiny::tabPanel('Start here!', shiny::includeMarkdown(rmdH)),
      shiny::tabPanel('Theory',      shiny::includeHTML(rmdT)),
      shiny::tabPanel('About',       shiny::includeMarkdown(rmdA))
    )
  )
  cssF <- fpath <- system.file("www", "style.css", package="larmexShiny")
  body <- shinydashboard::dashboardBody(
    shiny::tags$head(shiny::includeCSS(cssF)),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = 'HLP', homeUI),
      shinydashboard::tabItem(tabName = 'IMP', importUI('imp')),
      shinydashboard::tabItem(tabName = 'FIT', modelUI('fit'))
    )
  )

  shiny::div(class="container", shinydashboard::dashboardPage(header, sidebar, body))
}

app_server <- function(input, output, session) {

  nAbb <- shiny::reactive(input[['imp-nAbb']])
  sjID <- shiny::reactive(input[['imp-sjID']])
  nDay <- shiny::reactive(input[['imp-nDay']])
  nBeep <- shiny::reactive(input[['imp-nBeep']])
  arL  <- shiny::reactive(input[['imp-arList']])
  exL  <- shiny::reactive(input[['imp-exList']])

  objF <- LARMExFit$new()
  importServer('imp', objF)
  modelServer('fit', objF, sjID, nDay, nBeep, nAbb, arL, exL)

}
