# FAIRe term review for MIxS/DwC eDNA checklist proposal
This repository contains scripts and documentation for the TDWG/GSC eDNA task group to conduct a term-by-term review of the FAIRe checklist. 

# Background
The TDWG/GSC eDNA Task Group is working to develop an eDNA checklist under the DwC and MIxS standards.
To support this effort, a term-by-term review of the FAIRe checklist is being conducted to:
- Track the origin of each FAIRe term.
- Document how terms have been modified from their original source, e.g., MIxS, DwC, the DNA extension, and DwC-DP.
- Determine whether each term should be included in the proposed DwC/MIxS eDNA checklist.
To help visualise the decision-making process, an R function was developed to generate decision trees that capture and display the review outcomes for each term.

# General procedure for each FAIRe term
1. Complete the decision tree as much as possible (by filling Column AA- AY of the Google Sheet [FAIRe_checklist_v.1.0.2_decision_tree](https://docs.google.com/spreadsheets/d/1WxiiFNDMOjaucp5WXk-fKSXC3dH2e7jzsAiPnSR8pdU/edit?usp=sharing))
2. Generate decision trees using the [tree R functions](https://github.com/FAIR-eDNA/FAIRe_term_review_decision_tree/blob/main/FAIRetree.R) 
3. Discuss with the eDNA TG and GSC/TDWG members
4. Make decisions
5. Review SKOS mapping (Re-visit points raised in [Issue #14](https://github.com/FAIR-eDNA/FAIRe_checklist/issues/14))
6. Enter 1 in the Column X "tree_completed" [FAIRe_checklist_v.1.0.2_decision_tree](https://docs.google.com/spreadsheets/d/1WxiiFNDMOjaucp5WXk-fKSXC3dH2e7jzsAiPnSR8pdU/edit?usp=sharing)



# About the [tree R functions](https://github.com/FAIR-eDNA/FAIRe_term_review_decision_tree/blob/main/FAIRetree.R) 
Follow the below steps to run generate a decision tree for a FAIRe term using R functions (tree_fun or half_tree_fun).

**STEP 1.** Install packages. 

```
packages <- c("readxl", "openxlsx", "RColorBrewer", "dplyr", "here", 
              "yaml", "xml2" ,"jsonlite", "purrr", "googledrive", "googlesheets4",
              "DiagrammeR", "DiagrammeRsvg", "rsvg", "glue", "magick") 
for (i in packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  }
}
```

**STEP 2.** Source the `FAIRetree.R` script to load the function into your R environment.

```
source("https://raw.githubusercontent.com/FAIR-eDNA/FAIRe_term_review_decision_tree/main/FAIRetree.R")

```

**STEP 3.** Run the function. For example:
```
tree_fun(term = 'project_name', map_to = 'DwC')
half_tree_fun(term = 'project_name', map_to = 'MIxS', plot_save = T, plot_save_path = 'ROutput')
```

About the functions: 
`FAIRetree.R` contains two functions; 
- `tree_fun` produces the complete decision tree.
- `half_tree_fun` produces a condensed version that displays only relevant branches (determined by whether the term exists in the target standard (map_to)) to increase font readability.

About the input: 
- Information to generate the tree are read from the Google sheet FAIRe_checklist_v1.0.2_decision_tree.
- Avoid using special characters '\"{}[]<>:; in the note columns e.g., A1a_name_mod_reason. These characters will be replaced to __ in the tree.

Both `tree_fun` and `half_tree_fun` requires the following parameters:
- `term` A FAIRe term
- `map_to` Data standard to compare the term to. Select from "MIxS", "DwC", "DwC-DP", "DNA-extension"

The two optional parameters are:
- `plot_save` T or F to save the tree plot. Default is F.
- `plot_save_path` A path where a tree plot is saved. Do not include / at the end. Default is a current working directly. 

Expected output:
A decision tree will be generated and displayed in the Viewer pane in RStudio.
If you choose to save the plot, a `tree_plot` or `half_tree_plot` folder will be created within the specified `plot_save_path` (default is your working directory). The corresponding `.jpg` file will be saved inside that folder.
