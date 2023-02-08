# Intraindividual Dynamic Network of Affects
This repository hosts R code for a Shiny application developed as part of 
[DynaMORE](http://www.dynamore-project.eu) projct. It provides tools to fit an 
**Exogenous Linear Autoregressive Mixed Effects model**, LARMEx, to ecological 
momentary assessments (EMA). 

## How to use the app?
- install **R** from [r-project](https://www.r-project.org/)
    * install **RStudio** from [rstudio-desktop](https://posit.co/download/rstudio-desktop/)
- to install packages from GitHub one needs the R package "remotes"
    * **`install.packages("remotes")`**
- install **`larmexShiny`**
  * **`remotes::install_github(repo = "spooseh/larmexShiny", ref = "master", dependencies = TRUE)`**
### Run the app locally
- load the package, **`library(larmexShiny)`**
- run **`runLARMEx()`** in command line 
    * an Rstudio window opens and shows the user interface
    * you can click **`Open in Browser`** or enter the address next to it, **`http://127.0.0.1:6967`**, on a browser of your choice (6967 may be different in your case)

<img src="./www/img/RunApp3.png" alt="RunApp.png" width="600"/>

<br>

### Instructions

- detailed instructions in [larmexShiny/www/Instructions.md](https://github.com/spooseh/larmexShiny/www/Instructions.md)

### Not interested in a GUI?
- follow the R commands in [larmexShiny/www/demo.Rmd](larmexShiny/data/demo.R)

### To do ...
- exception handling
- more documentation
 
<hr>
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/">
<img style="right" alt="Creative Commons Lizenzvertrag" style="border-width:0" 
src="https://i.creativecommons.org/l/by/4.0/88x31.png"/></img></a>
