---
output: github_document
---
**Missing left menu**:<br>
Please adjust the window size if you don't see the sidebar menu to the left
with a black background. This might happen following certain settings of 
RStudio.
<hr>

### Import data
- click on **`Data`** menu item on the sideber and upload a **`.CSV`** data file. It is assumed that the CSV file contains column names for 
    * **`ID`**: subject ID to extract data for a single respondent
    * **`nDay`**: level 2 variable in a multilevel setting, e.g., number of days or weeks 
    * **`nBeep`**: level 1 variable in a multilevel setting, number of beeps for EMA data
    * **`Autoregressive`**: columns for interacting moods, e.g., `M1` and `M2` below
    * **`Exogenous`**: columns for external factors, e.g., E below
- sumulated data for two subject IDs is provided as **`/data/simMood.csv`**. Note that only the IDs different and the fitting results would be indentical for both IDs.

<img src="img/OpenCSV.png" alt="OpenCSV.png" width="600"/>

### Specify variable names
- select variable names from drop-down menus
    * if selected **`ID`**, **`nDay`** and **`nBeep`** don't have multilevel format, conflicting columns are cleard after an error window pops up with a message pointing to a potential cause.
    * you can specify if there should be an **`Abbreviation` on the names of moods and and external factors.  

<img src="img/VarNames.png" alt="VarNames.png" width="600"/>

### Specify the model and fit
- on the **`Model`** page a mixed effects formula appears under **`Formula`**. This is a full model assuming that 
every **`Autoregressive`** and **`Exogenous`** variable has fixed- and random- effects components.
    * you can modify this formula under **`Add\Remove fixed and random components`** explained below.
    * the results will be saved in your **`home`** directory in a new folder named **`res_DATE_TIME` where DATE 
    and TIME refer to your local values at the time of pressing the **`FIT`** button.
    * you can change the parent directory of the results folder by **`Save to`**.
    * under **`IDs to Fit`** you can choose **`All`**, single or multiple IDs to fit the whole data set of desired parts based of selectd IDs.
    * if **`Center data`** is checked, observations will be centered for individual respondents.

<img src="img/Fit.png" alt="Fit.png" width="600"/>

### Modify the model
- the model formula is constructed using adjacency matrices for fixed and radom effects.
<img src="img/Formula1.png" alt="Formula1.png" width="600"/>

- every term represents an edge in the network from a column to a row.
    * **`M2_M1`** is the connection from **`M2`** to **`M1`** and corresponds to the cell with column **`M2 =>`** and row **`=> M`**.
- the presence (absence) of each term is shown by **`1`** (**`0`**). 
- you can remove or add a term by changeing the matrix cell to zero and one respectively.
- fixed and random effects are accessed by the upper and lower matrices.
- to remove **`M2_M1`** from the fixed-effects and **`E_M2`** from the random-effects:
    * you might need to click outside the cell to have the effect.
<img src="img/Formula2.png" alt="Formula2.png" width="600"/>
