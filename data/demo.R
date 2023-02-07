
source('/R/LARMExFit.R') # mind the working directory please 
df <- read.csv('/data/simMood.csv') # mind the working directory please 
objF <- LARMExFit$new()
objF$rawData <- df
objF$sjID    <- 'sjID'
objF$nDay    <- 'nDay'
objF$nBeep   <- 'nBeep'
objF$arList  <- c('M1', 'M2')
objF$exList  <- c('E')
objF$nAbb    <- 3L
objF$arAbb   <- abbreviate(objF$arList, objF$nAbb)
objF$exAbb   <- abbreviate(objF$exList, objF$nAbb)
propperLevels(objF)
adjMats(objF)
setFormulaM(objF)
objF$fitID <- unique(objF$rawData[, objF$sjID])
objF$savePath <- normalizePath("~")
toDir = file.path(objF$savePath, format(Sys.time(), "res_%d%m%y_%H%M"))
dir.create(toDir, recursive=T, showWarnings=F)
objF$savePath <- toDir
for(sj in objF$fitID){
  print(sprintf("Subject %s done!", sj))
  fitLmer1(objF, sj, toDir)
}