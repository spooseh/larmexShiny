library(lmerTest)
library(R6)
#######################################
#' Collection of data and settings 
#' 
#' An R6Class is used instead of a named list
#' because we needed a data structure to be passed to functions by reference 
#' instead of value. It is not implemented in a pure object oriented form for 
#' simplicity. However, it is kept similar by naming this object "self" 
#' and passing it as an argument to independent functions.
#' 
#' @field rawData data, single or multiple subjects, long dataframe
#' @field sjID    variable name, subject ID, character
#' @field nBeep   variable name, number of beeps, character
#' @field nDay    variable name, number of days, character
#' @field arList  list of interacting mood nodes, character
#' @field exList  list of external factors acting on moods, character
#' @field nAbb    abbreviate long names to this length, ignore if 0
#' @field arAbb   arList after abbreviation
#' @field exAbb   exlist after abbreviation
#' @field fitID   list of subject of IDs to be fitted
#' @field frm     "lmer" formula to be fit
#' @field fitRes  result of fitting 
#' @field feM     adjacency matrix of fixed effects
#' @field reM     adjacency matrix of random effects
#' @field savePath path to save results, default to HOME
#' @field doCenter center mood and external factor values per subject
#'
#' @examples
#' obj1 <-LARMExFit$new()
#' obj1$arList <- c("mood1", "mood2")
LARMExFit <- R6Class(
  'LARMExFit',
  public = list(
    rawData = NULL, # data, long, single or multiple subjects, dataframe
    sjID    = NULL, # variable name, subject ID, character
    nBeep   = NULL, # variable name, number of beeps, character
    nDay    = NULL, # variable name, number of days, character
    arList  = NULL, # variable names, AR nodes, character
    exList  = NULL, # variable names, EX nodes, character
    nAbb    = 3,    # abbreviate the long names, ignore if 0
    arAbb   = NULL, # AR list after abbreviation
    exAbb   = NULL, # EX list after abbreviation
    fitID   = NULL, # list of subject of IDs to be fitted
    frm     = NULL, # lmer formula to be fit
    fitRes  = NULL, # result of fitting 
    feM = matrix(0,1,1), # adjacency matrix of fixed effects
    reM = matrix(0,1,1), # adjacency matrix of random effects
    savePath = normalizePath("~"), # path to save results, default to HOME 
    doCenter = TRUE # center mood and external factor values per subject
  )
)
#######################################
#' Check if data hase aproper multilevel form for the Shiny app
#'
#' @param self LARMExFit. A setRefClass object holding data and settings
#' @param nD integer. Minimum number of days per subject
#' @param nB integer. Minimum number of beeps per day 
#' @examples
propperLevels <- function(self, nD=2, nB=2){
  id <- unique(self$rawData[ ,self$sjID])
  flag <- c(T, T)
  msg <- ''
  msgDay  <- c(paste0("Less than ", nD, " days\\weeks"), "Not ascending days\\weeks")
  msgBeep <- c(paste0("Less than ", nB, " beeps"), "Not ascending beeps")
  for(i in id){
    df <- self$rawData[self$rawData[ ,self$sjID]==i, c(self$nDay, self$nBeep)]
    day <- unique(df[ ,self$nDay])
    lenDay <- length(day) < nD
    ascDay <- all(diff(day) > 0)
    if(lenDay | !ascDay){
      flag <- c(F, F)
      msg <- msgDay[c(lenDay, !ascDay)]
      break
    }
    for(j in day){
      beeps <- df[df[ ,self$nDay]==j, self$nBeep]
      lenBeep <- length(beeps) < nD
      ascBeep <- all(diff(beeps) > 0)
      if(lenBeep | !ascBeep){
        msg <- msgBeep[c(lenBeep, !ascBeep)]
        flag[2] <- F
        x <- list(i, j, flag, msg)
        return(x)
      }
    }
  }
  x <- list(i, 0, flag, msg)
  return(x)
}
#######################################
#' Abbreviate long names according to self$nAbb
#'
#' @param self LARMExFit. A setRefClass object holding data and settings
#'
#' @return Updates $arAbb and $exAbb based on $arList, $exList and $nAbb.
#' If $nAbb is zero or greater than the lengths of all names, the original
#' names are retained.
#'
#' @examples
#' obj1 <-LARMExFit$new()
#' obj1$arList <- c("mood1", "mood2")
#' doAbbreviate(obj1)
doAbbreviate <- function(self){
  if(self$nAbb!=0 & any(lapply(self$arList, nchar) > self$nAbb))
    self$arAbb <- abbreviate(self$arList, self$nAbb)
  else
    self$arAbb <- self$arList
  if(self$nAbb!=0 & any(lapply(self$exList, nchar) > self$nAbb))
    self$exAbb <-  abbreviate(self$exList, self$nAbb)
  else
    self$exAbb <- self$exList
}
#######################################
#' Construct adjacency matrices for fixed and random effects
#'
#' @param self LARMExFit. A setRefClass object holding data and settings
#'
#' @return Updates $feM and $reM based on $arAbb and $exAbb of self
adjMats <- function(self){
  doAbbreviate(self)
  n <- length(self$arAbb)
  m <- length(self$exAbb)
  M <- matrix(1, n, n+m)
  colM <- paste0(objF$arAbb,' =>')
  if(m > 0)
    colE <- paste0(objF$exAbb,' =>')
  else
    colE <- list()
  rownames(M) <- paste0('=> ', objF$arAbb)
  colnames(M) <- c(colM, colE)
  self$feM <- M
  colC <- "C =>"
  M <- matrix(1, n, n+m+1)
  rownames(M) <- paste0('=> ', objF$arAbb)
  colnames(M) <- c(colM, colE, colC)
  self$reM <- M
}
#######################################
#' Generate part of formula based on adjacency matrix
#'
#' @param self  LARMExFit. A setRefClass object holding data and settings
#' @param x Network edge matrix with elements of "FROM_TO" form.
#'  FROM and TO are node names from $arAbb and $exAbb of self  
#' @param M Network adjacency matrix of fixed or random effects
#'
#' @return Part of an "lmer" formula
#'
#' @examples
#' obj1 <-LARMExFit$new()
#' obj1$nDay <- "Day"
#' obj1$arList <- c("mood1", "mood2")
#' obj1$exList <- "E"
#' adjMats(obj1)
#' doAbbreviate(obj1)
#' m <- length(obj1$exAbb)
#' vars <- t(outer(c(obj1$arAbb, obj1$exAbb), obj1$arAbb, FUN=paste, sep='_'))
#' FE <- frmPart(obj1, vars, obj1$feM)
#' RE <- frmPart(obj1, vars, obj1$reM[ ,1:(n+m)])
frmPart <- function(self, x, M){ 
  n <- length(self$arList)
  m <- length(self$exList)
  x[!M] <- NA
  ar <- as.vector(t(x[ , 1:n])) 
  ar <- ar[!is.na(ar)] 
  if(m > 0){
    ex <- as.vector(t(x[ , (n+1):(n+m), drop=F]))
    ex <- ex[!is.na(ex)]
    return(c(ar,ex))
  }else
    return(ar)
}
#######################################
#' Generate mixed-effects formula from adjacency matrices of fixed and 
#' random effects given that "self" has at least two variables in $arList
#'
#' @param self  LARMExFit. A setRefClass object holding data and settings 
#'
#' @return self$frm is updated
#'
#' @examples
#' obj1 <-LARMExFit$new()
#' obj1$nDay <- "Day"
#' obj1$arList <- c("M1", "M2")
#' obj1$exList <- "E"
#' adjMats(obj1)
#' setFormulaM(obj1)
setFormulaM <-  function(self){
  n <- length(self$arList)
  if(n < 2)
    frm <- "At least two AR variables are needed!"
  else if(self$nDay == '')
    frm <- "No level 2, Day or Week, term!"
  else{
    m <- length(self$exList)
    vars <- t(outer(c(self$arAbb, self$exAbb), self$arAbb, FUN=paste, sep='_'))
    FE <- frmPart(self, vars, self$feM)
    RE <- frmPart(self, vars, self$reM[ ,1:(n+m)])
    cTerm <- self$arAbb[self$reM[ ,ncol(self$reM)]==1]
    if(length(cTerm) > 0)
      RE <- c(RE, paste("C", cTerm, sep='_'))
    feTerm <- paste(FE, collapse = ' + ')
    reTerm <- paste(RE, collapse = ' + ')
    if(feTerm == '')
      frm <- "No fixed-effects term!"
    if(reTerm == '')
      frm <- "No random-effects term!"
    if(feTerm!='' & reTerm!='')
      frm <- paste0('M ~ 0 + ', feTerm, ' + (0 + ', reTerm, ' | ', self$nDay, ')')
  }
  self$frm <- frm
}
#######################################
#' Generate mixed-effects formula from a transformed data frame 
#' see help('prepData2Fit')
#'
#' @param self LARMExFit. A setRefClass object holding data and settings 
#'
#' @return self$frm is updated
#'
#' @examples
#' obj1 <- LARMExFit$new()
#' obj1$rawData <- read.csv('./data/simMood.csv')
#' obj1$nDay <- "Day"
#' obj1$nBeep <- "Beep"
#' obj1$arList <- c("M1", "M2")
#' obj1$exList <- "E"
#' selCols <- c(obj1$nDay, obj1$nBeep, obj1$arList, obj1$exList)
#' sj <- 1001
#' sjData <- obj1$rawData[obj1$rawData[obj1$sjID]==sj, selCols]
#' df2Fit <- prepData2Fit(obj1, sjData)
#' setFormulaDf(obj1, data2Fit)
setFormulaDf <-  function(self, data2Fit){
  RE <- colnames(data2Fit)[c(-1,-2)]
  FE <- RE[!RE %in% grep(paste0(c('C'), collapse = "|"), RE, value = T)]
  self$frm <- paste0('M ~ 0 + ',paste(FE,collapse = ' + '),
                     ' + (0 + ',paste(RE,collapse = ' + '),' | ',self$nDay,')')
}
#######################################
#' Transform raw data to a format suitable for fitting by LARMEx
#'
#' @param self LARMExFit. A setRefClass object holding data and settings 
#' @param sjData Raw data for single subject
#'
#' @return data.frame. Transformed data
#'
#' @examples
#' obj1 <- LARMExFit$new()
#' obj1$rawData <- read.csv('./data/simMood.csv')
#' obj1$nDay <- "Day"
#' obj1$nBeep <- "Beep"
#' obj1$arList <- c("M1", "M2")
#' obj1$exList <- "E"
#' selCols <- c(obj1$nDay, obj1$nBeep, obj1$arList, obj1$exList)
#' sj <- 1001
#' sjData <- obj1$rawData[obj1$rawData[obj1$sjID]==sj, selCols]
#' df2Fit <- prepData2Fit(obj1,sjData)
prepData2Fit <-  function(self, sjData){
  doAbbreviate(self)
  colnames(sjData) <- c(self$nDay, self$nBeep, self$arAbb, self$exAbb)
  nVar <- length(self$arList)
  nExg <- length(self$exList)
  colM <- paste(rep(self$arAbb,nVar),rep(self$arAbb,each=nVar),sep='_') 
  colE <- paste(rep(self$exAbb,nVar),rep(self$arAbb,each=nExg),sep='_') 
  colC <- paste('C', self$arAbb, sep='_')
  col  <-c(self$nDay, 'M', colM, colE, colC) 
  D <- matrix(ncol=length(col),nrow=0)
  for(nd in unique(sjData[[self$nDay]])){
    d1 <- sjData[sjData[self$nDay]==nd, ]
    beeps <- d1[ ,self$nBeep]
    maxB <- max(beeps)
    if(!all(diff(beeps==1))){
      d2 = data.frame(cbind(rep(nd, maxB), seq_len(maxB), matrix(nrow=maxB, ncol=nVar+nExg)))
      colnames(d2) = names(sjData)
      d2[beeps, ] <- d1
      d1 <- d2
    }
    nObs <- nrow(d1)
    C <- rep(1,nObs-1)
    dM  <- as.matrix(d1[2:nObs,self$arAbb])
    dim(dM) <- NULL
    dL <- kronecker(diag(nVar), as.matrix(d1[1:(nObs-1),self$arAbb]))
    dE <- kronecker(diag(nVar), as.matrix(d1[2:nObs,self$exAbb]))
    dC <- kronecker(diag(nVar), C)
    D <- rbind(D,cbind(rep(nd,nVar*(nObs-1)),dM,dL,dE,dC))
  }
  fitData <- data.frame(D[complete.cases(D), ])
  colnames(fitData) <- col
  return(fitData)
}
#######################################
#' Fit a LARMEx model to the data of a single subject
#'
#' @param self LARMExFit. A setRefClass object holding data and settings 
#' @param sj ID for a single subject
#' @param toDir Path to save results
#'
#' @export
#'
#' @examples
#' obj1 <- LARMExFit$new()
#' obj1$rawData <- read.csv('./data/simMood.csv')
#' obj1$nDay <- "Day"
#' obj1$nBeep <- "Beep"
#' obj1$arList <- c("M1", "M2")
#' obj1$exList <- "E"
#' obj1$savePath <- normalizePath("~")
#' selCols <- c(obj1$nDay, obj1$nBeep, obj1$arList, obj1$exList)
#' toDir = file.path(objF$savePath, "fitRes")
#' dir.create(toDir, recursive=T, showWarnings=F)
#' sj <- 1001
#' fitLmer1(obj1, sj, toDir)
fitLmer1 <-  function(self, sj, toDir){
  if(is.null(sj))
    return
  selCols <- c(self$nDay, self$nBeep, self$arList, self$exList)
  sjData <- self$rawData[self$rawData[self$sjID]==sj, selCols]
  if(self$doCenter)
    for(c in c(self$arList, self$exList)){
      avg <- mean(sjData[ , c])
      if(avg != 0)
        sjData[ , c] <- sjData[ , c] - avg
    }
  df <- prepData2Fit(self,sjData)
  ctrl <- lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
  res <- lmer(self$frm, df, REML=FALSE, control=ctrl)
  self$fitRes <- res
  sjObj <- self$clone()
  sjObj$rawData <- sjData
  sjObj$fitID   <- sj
  vName <- paste0("fit_", sj)
  assign(vName, sjObj)
  fName <- file.path(toDir,paste0('sbj_', sj, '.RData'))
  eval(parse(text=paste0("save(", vName, ", file=fName)")))
  fName <- file.path(toDir, paste0('summary_', sj, '.txt'))
  capture.output(summary(res), file=fName)
  fName <- file.path(toDir, paste0('fe_', sj, '.csv'))
  write.csv(coef(summary(res)), file=fName)
  fName <- file.path(toDir, paste0('re_', sj, '.csv'))
  write.csv(ranef(res)[[self$nDay]], file=fName)
}
#######################################