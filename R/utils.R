#####################################################Util
# ch=list()
selectMenu <- function(id, lab, choices=list(), mult=F, 
                       opt=list(placeholder='Import data first')){
  selectizeInput(
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
    updateSelectizeInput(session, id, choices=choices, selected=sel, options=opt)
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
  updateMatrixInput(session, id, M)
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
      as.formula(x)
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
