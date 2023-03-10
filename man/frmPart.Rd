% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LARMExFit.R
\name{frmPart}
\alias{frmPart}
\title{Generate part of formula based on adjacency matrix}
\usage{
frmPart(self, x, M)
}
\arguments{
\item{self}{LARMExFit. A setRefClass object holding data and settings}

\item{x}{Network edge matrix with elements of "FROM_TO" form.
FROM and TO are node names from $arAbb and $exAbb of self}

\item{M}{Network adjacency matrix of fixed or random effects}
}
\value{
Part of an "lmer" formula
}
\description{
Generate part of formula based on adjacency matrix
}
\examples{
obj1 <-LARMExFit$new()
obj1$nDay <- "nDay"
obj1$arList <- c("mood1", "mood2")
obj1$exList <- "E"
adjMats(obj1)
doAbbreviate(obj1)
n <- length(obj1$arAbb)
m <- length(obj1$exAbb)
vars <- t(outer(c(obj1$arAbb, obj1$exAbb), obj1$arAbb, FUN=paste, sep='_'))
FE <- frmPart(obj1, vars, obj1$feM)
RE <- frmPart(obj1, vars, obj1$reM[ ,1:(n+m)])
}
