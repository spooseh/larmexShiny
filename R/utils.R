#####################################################Util
testLevels <- function(input, session, objF){
  if(input$sjID!='' & input$nDay!='' & input$nBeep!=''){
    x <- propperLevels(objF)
    nSj <- x[[1]]
    id <- x[[2]]
    msgD <- x[[3]]
    msgB <- x[[4]]
    if(msgD != '' | msgB != ''){
      shiny::showModal(shiny::modalDialog(
        title = "",
        shiny::HTML("The data does not have a proper multilevel structure for
        all subjects. Please ensure that each subject has multiple
        \"nDay/nWeek.\" in an ascending order. Each \"nDay/nWeek.\" must also
        have multiple \"nBeep\" items in an ascending order.", "<br>",
        "The following information might help to identify the problem.", "<br>",
        "   Number of subjects: ", sprintf("%s", nSj), "<br>",
        "First problematic instance:", "<br>",
        "   Subject ID: ", sprintf("%s", id), "<br>",
        "   Day\\Week: ", msgD, "<br>",
        "   Beeps: ", msgB, "<br>",
        "Please exclude the problematic part from the dataset and then upload it
        again!", "<br>",
        "There might be further such problems!")
      ))
      if(msgD != ''){
        shiny::updateSelectizeInput(session,"nDay",  selected=list())
        shiny::updateSelectizeInput(session,"nBeep",  selected=list())
      }
      if(msgB != '')
        shiny::updateSelectizeInput(session,"nBeep",  selected=list())
    }else{
      x <- setdiff(colnames(objF$rawData), c(objF$sjID, objF$nDay, objF$nBeep))
      updateMenus(session, c("arList","exList"), choices=x)
    }
  }
}
#####################################################Util
# ch=list()
selectMenu <- function(id, lab, choices=list(), mult=F,
                       opt=list(placeholder='Import data first')){
  shiny::selectizeInput(
    id,
    label = lab,
    choices = choices,
    multiple = mult,
    options = opt)
}
#####################################################Util
# ch=list()
updateMenus <- function(session, listID,   choices=list(), sel=list(),
                       opt=list(placeholder='Click to choose')){
  for(id in listID)
    shiny::updateSelectizeInput(session, id, choices=choices, selected=sel, options=opt)
}
#####################################################Util
genNetMat <- function(id, objF, session){
  n <- length(objF$arAbb)
  m <- length(objF$exAbb)
  M <- matrix(1, n, n+m)
  colM <- paste0(objF$arAbb,' =>')
  if(m > 0)
    colE <- paste0(objF$exAbb,' =>')
  else
    colE <- list()
  colC <- list()
  if(id=='reM'){
    colC <- "C =>"
    M <- matrix(1, n, n+m+1)
  }
  rownames(M) <- paste0('=> ', objF$arAbb)
  colnames(M) <- c(colM, colE, colC)
  shinyMatrix::updateMatrixInput(session, id, M)
}
#####################################################Util
genAbb <- function(orgList,n){
  if(length(orgList)>0 & n!=0 & any(lapply(orgList, nchar) > n))
    return(abbreviate(orgList, n))
  else
    return(orgList)
}
#####################################################Util
#https://stackoverflow.com/questions/36361158/how-to-test-if-an-object-is-a-formula-in-base-r
is_formula <- function(x){
  tryCatch(
    expr = {
      stats::as.formula(x)
      return(T)
    },
    error = function(e){
      return(F)
    },
    warning = function(w){
    },
    finally = {
    }
  )
}
