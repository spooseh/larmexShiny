# Intraindividual Dynamic Network of Affects

This repository hosts R code for a Shiny application developed as part of [DynaMORE](http://www.dynamore-project.eu) projct. It provides tools to fit an **Exogenous Linear Autoregressive Mixed Effects model**, LARMEx, to ecological momentary assessments (EMA).

## How to use the app?

-   install **R** from [r-project](https://www.r-project.org/)
-   install **RStudio** from [rstudio-desktop](https://posit.co/download/rstudio-desktop/)

### I. Install as an R package

-   to install packages from GitHub one needs the R package "remotes"
    -   **`install.packages("remotes")`**
-   install "larmexShiny""
    -   **`remotes::install_github(repo = "spooseh/larmexShiny", ref = "master", dependencies = TRUE)`**
-   load the package
    -   **`library(larmexShiny)`**
-   run in R command line
    -   **`runLARMEx()`**

### II. Download the source code and run locally

- two ways to get the codes:
    - i.  through terminal by **cloning** the repository
        -   **`git clone -b clone_and_run --single-branch https://github.com/spooseh/larmexShiny.git`**
    - ii. through a browser by **downloading** the repository 
        -   go to [github.com/spooseh/larmexShiny](https://github.com/spooseh/larmexShiny.git)
        -   select the branch "clone_and_run"
        -   download the code as a zip file ("larmexShiny-clone_and_run.zip")

-   navigate to the local directory, **`larmexShiny`**, in RStudio

-   optional: for a smooth work flow, make this folder, by `setwd()`, the working directory

-   open **`packInstaller.R`** and press **`Source`** in RStudio

    -   it runs the command `source("packInstaller.R")`
    -   one could install the missing packages manually
    -   see `sessionInfo.txt` for version information

-   open the **`runLARMEx.R`** file and click the **`Run App`** at the top of your editor's menu

## What you see

-   an Rstudio window opens and shows the user interface
-   you can click **`Open in Browser`** or enter the address next to it, **`http://127.0.0.1:6967`**, on a browser of your choice (6967 may be different in your case)

<img src="inst/www/img/RunApp3.png" alt="RunApp.png" width="600"/>

<br>

### Instructions

-   detailed instructions in [larmexShiny/www/Instructions.md](https://github.com/spooseh/larmexShiny/www/Instructions.md)

### Not interested in a GUI?
The following steps walk you through the workflow of fitting some simulated data
and hopefully demonsterate how to treat your own. Once "larmexShiny" is installed in R:
- access the simulated data included in the package
    * `simMood <- system.file("extdata", "simMood.csv", package="larmexShiny")`
- create an object holding the data and settings
    * `obj1 <- LARMExFit$new()`
    * `?LARMExFit` for more information
- assign raw data, long, single or multiple subjects, dataframe
    * `obj1$rawData <- read.csv(simMood)`
- variable name, subject ID, character 
    * `obj1$sjID <- "sjID"` 
- variable name, number of beeps, character
    * `obj1$nDay <- "nDay"`
- variable name, number of days, character 
    * `obj1$nBeep <- "nBeep"`
- variable names, interacting moods (AR nodes), character
    * `obj1$arList <- c("M1", "M2")`
- variable names, external factors acting on meeds (EX nodes), character
    * `obj1$exList <- "E"`
- where to save results, default to HOME
    * `obj1$savePath <- normalizePath("~")`
    * `toDir = file.path(obj1$savePath, "fitRes")`
    * `dir.create(toDir, recursive=TRUE, showWarnings=FALSE)`
- extract data for a single subject 
    * `sj <- 1001`
    * `selCols <- c(obj1$nDay, obj1$nBeep, obj1$arList, obj1$exList)`
    * `sjData <- obj1$rawData[obj1$rawData[obj1$sjID]==sj, selCols]`
- transform the data into a special format suitable for LARMEx
    * `df2Fit <- prepData2Fit(obj1, sjData)`
- set the formula using the transformed data (the column names are used) 
    * `setFormulaDf(obj1, df2Fit)`
- fit the data and save the results
    * `fitLmer1(obj1, sj, toDir)`


### To do ...

-   exception handling
-   more documentation

<hr>

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"> <img src="https://i.creativecommons.org/l/by/4.0/88x31.png" alt="Creative Commons Lizenzvertrag" style="right"/></img></a>
