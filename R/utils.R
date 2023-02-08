#####################################################Util
testLevels <- function(input, session, objF){
  if(input$sjID!='' & input$nDay!='' & input$nBeep!=''){
    x <- propperLevels(objF)
    id <- x[[1]]
    nD <- x[[2]]
    flag <- x[[3]]
    msg <- x[[4]]
    if(!all(flag)){
      shiny::showModal(shiny::modalDialog(
        title = "",
        shiny::HTML("Please make sure that for each subject every level of \"nDay\\nWeek\"
                 has multiple \"nBeep\" items!. The following is the first problematic
                 encounter in data.", "<br>",
                    "Possible cause:", "<br>",
                    "   Subject ID: ", sprintf("%s", id), "<br>",
                    "   Day\\Week number: ", sprintf("%s", nD), "<br>",
                    "   Beeps: ", msg, "<br>",
                    "There might be further such problems!")
      ))
      if(!flag[1])
        shiny::updateSelectizeInput(session,"nDay",  selected=list())
      if(!flag[2])
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
