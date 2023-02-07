app_ui <- function(){
  title <- 'Exogenous Linear Autoregressive Mixed Effects, LARMEx'
  header  <- dashboardHeader(title = title, titleWidth = 650)
  sidebar <- dashboardSidebar(
    width = 100, collapsed = F,
    sidebarMenu(
      id = "tabs", style = "position: fixed; overflow: visible;",
      menuItem(div(class = 'menu1', "Home"),  tabName = "HLP"),
      menuItem(div(class = 'menu1', "Data"),  tabName = "IMP"),
      menuItem(div(class = 'menu1', "Model"), tabName = "FIT")
    )
  )
  homeUI <- fluidPage(
    tabsetPanel(
      tabPanel('Start here!', includeMarkdown('./www/rmdHome.Rmd')),
      tabPanel('Theory',      includeHTML("./www/mathSum.html")),
      tabPanel('About',       includeMarkdown('./www/rmdAbout.Rmd'))
    )
  )
  body <- dashboardBody(
    tags$head(includeCSS("./www/style.css")),
    tabItems(
      tabItem(tabName = 'HLP', homeUI),
      tabItem(tabName = 'IMP', importUI('imp')),
      tabItem(tabName = 'FIT', modelUI('fit'))
    )
  )
  
  div(class="container", dashboardPage(header, sidebar, body))
}

app_server <- function(input, output, session) {
  
  nAbb <- reactive(input[['imp-nAbb']])
  sjID <- reactive(input[['imp-sjID']])
  nDay <- reactive(input[['imp-nDay']])
  nBeep <- reactive(input[['imp-nBeep']])
  arL  <- reactive(input[['imp-arList']])
  exL  <- reactive(input[['imp-exList']])
  
  objF <- LARMExFit$new()
  importServer('imp', objF)
  modelServer('fit', objF, sjID, nDay, nBeep, nAbb, arL, exL)
  
}