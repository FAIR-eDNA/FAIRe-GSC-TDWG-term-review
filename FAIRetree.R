#' About the function: 
#' These R functions are to generate decision trees that capture and display the FAIRe term review outcomes.
#' tree_fun produces the whole tree whereas half_tree_fun only shows a half to reduce contents and increase font sizes (only showing relevant parts, defined by Y/N of Q1 Does the term exist in {map_to})
#' 
#' About the input: 
#' Information to generate the tree are read from the Google sheet FAIRe_checklist_v1.0.2_decision_tree.
#' Avoid using special characters '\"{}[]<>:; in the note columns e.g., A1a_name_mod_reason. These characters will be replaced to __ in the tree.
#' 
#' Instructions 
#' 
#' Step 1: Set working directory 
#' 
#' STEP 2. Install packages
#' packages <- c("readxl", "openxlsx", "RColorBrewer", "dplyr", "here", 
#' "yaml", "xml2" ,"jsonlite", "purrr", "googledrive", "googlesheets4",
#' "DiagrammeR", "DiagrammeRsvg", "rsvg", "glue", "magick") 
#' for (i in packages) {
#'   if (!require(i, character.only = TRUE)) {
#'     install.packages(i, dependencies = TRUE)
#'     library(i, character.only = TRUE)
#'   }
#' }
#' 
#' Step 3: Source the FAIRetree.R script to load the functions into your R environment.
#' 
#' Step 4: Run the tree_fun or half_tree_fun function with the below arguments
#' Arguments:
#' @term A FAIRe term
#' @map_to Data standard to compare the term to. Select from "MIxS", "DwC", "DwC-DP", "DNA-extension"
#' @plot_save (optional) T or F to save the tree plot. Default is F.
#' @plot_save_path (optional) A path where a tree plot is saved. Do not include / at the end. Default is a current working directly. 
#' 
#' @examples:
#' tree_fun(term = 'project_name', map_to = 'DwC')
#' half_tree_fun(term = 'project_name', map_to = 'MIxS', plot_save = T)
#' 
#' Expected output:
#' A decision tree will be generated and displayed in the Viewer pane in RStudio.
#' If you choose to save the plot, a tree_plot or half_tree_plot folder will be created within the specified plot_save_path (default is your working directory). 
#' The corresponding .jpg file will be saved inside that folder.


##### Tree function ####
tree_fun <- function(term, map_to, plot_save = F, plot_save_path = getwd()){
  gs4_deauth() # Deauthorize so it doesn't prompt for login (works only for public sheets)
  sheet_url <- "https://docs.google.com/spreadsheets/d/1WxiiFNDMOjaucp5WXk-fKSXC3dH2e7jzsAiPnSR8pdU/edit?usp=sharing" # link to the google sheet "FAIRe_checklist_v1.0_decision_tree"
  mapping_df <- as.data.frame(read_sheet(sheet_url, sheet = map_to)) 
  temp <- mapping_df[which(mapping_df$term_name == term),]
  
  # FAIRe term info
  faire_req_lev <- temp$requirement_level
  if (!is.na(temp$requirement_level_condition)) faire_req_lev <- paste0(faire_req_lev, ' (', temp$requirement_level_condition, ')')
  faire_definition <- temp$description
  #faire_definition_wrapped <- paste(strwrap(faire_definition, width = 80), collapse = "\\n") # Wrap to ~80 characters per line, then collapse with \n
  faire_example <- temp$example
  #faire_example_wrapped <- paste(strwrap(faire_example, width = 80), collapse = "\\n") # Wrap to ~80 characters per line, then collapse with \n
  
  faire_term_type <- temp$term_type
  if (temp$term_type == 'controlled vocabulary') {
    cont_vocab_ls <- paste0("Controlled vocabulary options: ", temp$controlled_vocabulary_options)
    #cont_vocab_ls <- paste(strwrap(cont_vocab_ls, width = 80), collapse = "\\n")
  } else {
    cont_vocab_ls <- ""
  }
  
  # Object term info
  object_id <- temp[["object_id"]]
  object_label <- temp[["object_label"]]
  object_definition <- temp[["object_definition"]]
  # Wrap object_definition to ~80 characters per line, then collapse with \n
  #object_definition_wrapped <- paste(strwrap(object_definition, width = 80), collapse = "\\n")
  object_example <- temp[["object_example"]]
  object_term_type <- temp[["object_term_type"]]
  
  ### Specify arrow colors 
  red_arrow <- "color = 'red', penwidth = 3"
  gray_arrow <- "color = '#696969', penwidth = 1"
  black_arrow <- "color = 'black', penwidth = 1"
  
  #make all default settings first
  arrow_ls <- c("q1_y", "q1_n", "q1_1_y", "q1_1_n", "q2_y", "q2_n", "q3_y", "q3_n", "q5_y", "q5_n", 
                "q6_term_name", "q6_definition", "q6_example", "q6_term_type", 
                "q7a_y", "q7a_n", "q7b_y", "q7b_n", "q7c_y", "q7c_n", "q7d_y", "q7d_n", 
                "d1_p2_arrow", "q8_y", "q8_n", "q9_y", "q9_n", "q10_y", "q10_n", "q11_y", "q11_n", 
                "q12_y", "q12_n", "q13_y", "q13_n", "q14_y", "q14_n", "q15_y", "q15_n")
  for (i in arrow_ls) {
    assign(i, gray_arrow)
  }
  
  entry_ls <- c('q1_2_entry', 'q4_entry', 'q9_1_entry','a3_entry', 'a1a_entry', 'a1b_entry', 'a1c_entry', 'a1d_entry')
  for (i in entry_ls) {
    assign(i, "")
  }
  
  
  
  # customise arrows and entries
  if (is.na(object_id)) { # Q1 = no, doesn't exist in {map_to} hence created new
    q1_y <- black_arrow
    q1_n <- red_arrow
    if (!is.na(temp$Q1_1_term_in_other_standard)) {
      if (temp$Q1_1_term_in_other_standard == 1) {
        q1_1_y <- red_arrow
        q1_1_n <- black_arrow
        if (!is.na(temp$Q1_2_which_other_standard)) q1_2_entry <- temp$Q1_2_which_other_standard
        
      } else if (temp$Q1_1_term_in_other_standard == 0) {
        q1_1_y <- black_arrow
        q1_1_n <- red_arrow
      }
      
    }
    
    
    if (!is.na(temp$Q11_propose_term_0_1)) {
      if (temp$Q11_propose_term_0_1 == 1) { # Q11 = yes
        q11_y <- red_arrow
        q11_n <- black_arrow
        if (temp$term_type == 'controlled vocabulary') {
          q12_y <- red_arrow
          q12_n <- black_arrow
          if (!is.na(temp$Q13_cont_vocab_critical_0_1)) {
            if (temp$Q13_cont_vocab_critical_0_1 == 1) {
              q13_y <- red_arrow
              q13_n <- black_arrow
            } else if (temp$Q13_cont_vocab_critical_0_1 == 0) {
              q13_y <- black_arrow
              q13_n <- red_arrow
            }
          }
          
        } else {
          q12_y <- black_arrow
          q12_n <- red_arrow
          
        }
      } else if (temp$Q11_propose_term_0_1 == 0) { # Q11 = No
        q11_y <- black_arrow
        q11_n <- red_arrow
        if (!is.na(temp$A3_reason_not_propose)) a3_entry <- temp$A3_reason_not_propose  
        
      }
    }
  } else if (!is.na(object_id)) { # Q1 = yes, exists in {map_to}
    q1_y <- red_arrow
    q1_n <- black_arrow
    
    ## Modification
    cols <- c("Q6_name_mod_0_1", "Q6_description_mod_0_1", "Q6_example_mod_0_1", "Q6_type_mod_0_1")
    if (sum(temp[cols], na.rm=T) > 0) {# Q2 = Yes, modified from {map_to}
      q2_y <- red_arrow
      q2_n <- black_arrow
    } else if (all(!is.na(temp[cols]))) {
      if (all(temp[cols]) == 0) { # Q2 = No, not modified
        q2_y <- black_arrow
        q2_n <- red_arrow
      } 
    }
    if (identical(temp[["Q3_align_other_std_0_1"]], 1)) {
      q2_y <- red_arrow
      q2_n <- black_arrow
      q3_y <- red_arrow
      q3_n <- black_arrow
      if (!is.na(temp$Q4_other_std_id)) q4_entry <- temp$Q4_other_std_id
      
      if (identical(temp[["Q5_mapped_0_1"]], 1)) {
        q5_y <- red_arrow
        q5_n <- black_arrow
      } else if (identical(temp[["Q5_mapped_0_1"]], 0)){
        q5_y <- black_arrow
        q5_n <- red_arrow
        
      }
    } else if (identical(temp[["Q3_align_other_std_0_1"]], 0)) { # Q3 = No
      q3_y <- black_arrow
      q3_n <- red_arrow
      # Q6 what was modified?
      if (!is.na(temp[["Q6_name_mod_0_1"]])) {
        if (temp[["Q6_name_mod_0_1"]] == 1) {
          q6_term_name <- red_arrow
          if (!is.na(temp$A1a_name_mod_reason)) a1a_entry <- temp$A1a_name_mod_reason
          if (!is.na(temp[["Q7a_name_mod_critical_0_1"]])){
            if (temp[["Q7a_name_mod_critical_0_1"]] == 1) {
              q7a_y <- red_arrow
              q7a_n <- black_arrow
            } else if (temp[["Q7a_name_mod_critical_0_1"]] == 0) {
              q7a_y <- black_arrow
              q7a_n <- red_arrow
            }
          }
        } else if (temp[["Q6_name_mod_0_1"]] == 0) {
          q6_term_name <- black_arrow
        }
      }
      if (!is.na(temp[["Q6_description_mod_0_1"]])) {
        if (temp[["Q6_description_mod_0_1"]] == 1) {
          q6_definition <- red_arrow
          if (!is.na(temp$A1b_description_mod_reason)) a1b_entry <- temp$A1b_description_mod_reason
          if (!is.na(temp[["Q7b_description_mod_critical_0_1"]])){
            if (temp[["Q7b_description_mod_critical_0_1"]] == 1) {
              q7b_y <- red_arrow
              q7b_n <- black_arrow
            } else if (temp[["Q7b_description_mod_critical_0_1"]] == 0) {
              q7b_y <- black_arrow
              q7b_n <- red_arrow
            }
          }
        } else if (temp[["Q6_description_mod_0_1"]] == 0) {
          q6_definition <- black_arrow
        }
      }
      
      if (!is.na(temp[["Q6_example_mod_0_1"]])) {
        if (temp[["Q6_example_mod_0_1"]] == 1) {
          q6_example <- red_arrow
          if (!is.na(temp$A1c_example_mod_reason)) a1c_entry <- temp$A1c_example_mod_reason
          if (!is.na(temp[["Q7c_example_mod_critical_0_1"]])){
            if (temp[["Q7c_example_mod_critical_0_1"]] == 1) {
              q7c_y <- red_arrow
              q7c_n <- black_arrow
            } else if (temp[["Q7c_example_mod_critical_0_1"]] == 0) {
              q7c_y <- black_arrow
              q7c_n <- red_arrow
            }
          }
        } else if (temp[["Q6_example_mod_0_1"]] == 0) {
          q6_example <- black_arrow
        }
      }
      
      if (!is.na(temp[["Q6_type_mod_0_1"]])) {
        if (temp[["Q6_type_mod_0_1"]] == 1) {
          q6_term_type <- red_arrow
          if (!is.na(temp$A1d_type_mod_reason) & !is.null(temp$A1d_type_mod_reason[[1]])) a1d_entry <- temp$A1d_type_mod_reason
          
          
          if (!is.na(temp[["Q7d_type_mod_critical_0_1"]])){
            if (temp[["Q7d_type_mod_critical_0_1"]] == 1) {
              q7d_y <- red_arrow
              q7d_n <- black_arrow
              if (temp[['term_type']]== "controlled vocabulary") {
                q8_y <- red_arrow
                q8_n <- black_arrow
                if (!is.na(temp[["Q9_vocab_ls_source_0_1"]])) {
                  if (temp[["Q9_vocab_ls_source_0_1"]] == 1) {
                    q9_y <- red_arrow
                    q9_n <- black_arrow
                    if (!is.na(temp$Q9_1_which_source)) q9_1_entry <- temp[["Q9_1_which_source"]]
                    if (!is.na(temp[["Q10_vocab_ls_source_mod_0_1"]])) {
                      if (temp[["Q10_vocab_ls_source_mod_0_1"]] == 1) {
                        q10_y <- red_arrow
                        q10_n <- black_arrow
                      } else if (temp[["Q10_vocab_ls_source_mod_0_1"]] == 0) {
                        q10_y <- black_arrow
                        q10_n <- red_arrow
                      }
                    }
                  } else if (temp[["Q9_vocab_ls_source_0_1"]] == 0) {
                    q9_y <- black_arrow
                    q9_n <- red_arrow
                  }
                }
              } else {
                q8_y <- black_arrow
                q8_n <- red_arrow
              }
              
            } else if (temp[["Q7d_type_mod_critical_0_1"]] == 0) {
              q7d_y <- black_arrow
              q7d_n <- red_arrow
            }
          }
        } else if (temp[["Q6_type_mod_0_1"]] == 0) {
          q6_term_type <- black_arrow
        }
      }
      
      q6_term_name <- if (identical(temp[["Q6_name_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_name_mod_0_1"]], 0)) black_arrow
      q6_definition <- if (identical(temp[["Q6_description_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_description_mod_0_1"]], 0)) black_arrow
      if (!is.na(temp[["Q6_example_mod_0_1"]])) {
        q6_example <- if (identical(temp[["Q6_example_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_example_mod_0_1"]], 0)) black_arrow
      }
      if (!is.na(temp[["Q6_type_mod_0_1"]])) {
        q6_term_type <- if (identical(temp[["Q6_type_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_type_mod_0_1"]], 0)) black_arrow
        
      }
      
    } 
    if (all(!is.na(temp[cols]))) {
      if (all(temp[cols]) == 0) { # Q2 = No, not modified
        q2_y <- black_arrow
        q2_n <- red_arrow
      }
      
      
    }
        
    
    
  }
  
  if (map_to == 'MIxS') {
    D9_label <- "D9.Discuss whether the FAIRe term should also be included in other MIxS checklists beyond eDNA (e.g., MINAS)."
  } else if (map_to == 'DwC' | map_to =='DNA-extension' | map_to =='DwC-DP') {
    D9_label <- "D9.Discuss which DwC-DP table(s) the FAIRe term should be included in (e.g., Event, Molecular Analyses, Nucleotide Analyses, etc.)."
  }
  
  # Remove special character that grViz() doesn't accept, and wrap them (add \\n every X characters) to change lines
  ls <- c(entry_ls, "faire_definition", "object_definition", "faire_example", "object_example", "cont_vocab_ls")
  
  safe_text_tab <- data.frame(matrix(nrow=length(ls), ncol=4)) 
  colnames(safe_text_tab) <- c('name', 'original_text', 'safe_text', 'safe_wrapped_text')
  safe_text_tab$name <- ls
  
  for (i in ls) {
    original <- get(i)
    if (!is.null(original[[1]])) {
      safe_text_tab[safe_text_tab$name==i, 'original_text'] <- original
    #safe_text <- gsub("['\"{}<>:;]", "__", original)
    safe_text <- gsub("['\"{}<>]", "__", original)
    
    safe_text <- gsub("&", " and ", safe_text)
    safe_text <- gsub("%", " percent ", safe_text)
    safe_text_tab[safe_text_tab$name==i, 'safe_text'] <- safe_text
    
    if (i %in% c("faire_definition", "object_definition", "faire_example", "object_example", "cont_vocab_ls")) text_n = 80 else text_n = 50
    wrapped <- paste(strwrap(safe_text, width = text_n), collapse = "\\n") 
    
    safe_text_tab[safe_text_tab$name==i, 'safe_wrapped_text'] <- wrapped  
    assign(i, wrapped)
    }
  }
  
  
  # Note about glue(): Double braces {{ ... }} must be used to mean literal { ... } within glue().
  
  tree_code <- glue("
  digraph flowchart {{
  
  # Node styles
  node [shape = box, fontname = Helvetica, fontsize = 20]
  
  
  subgraph info {{
  label = 'Info'
  style = dashed
  label1 [
  label = 'FAIRe term: {term}\\nRequirement Level: {faire_req_lev}\\nDefinition: {faire_definition}\\nExample: {faire_example}\\nTerm type:{faire_term_type}\\n{cont_vocab_ls}\\n \\n
  Object ID: {object_id}\\nObject label: {object_label}\\nObject definition: {object_definition}\\nObject example: {object_example}\\nObject term type: {object_term_type}\\n
  Predicate ID: {temp$predicate_id}'
  ]
  }}

  #Legend box (top left)
  
  subgraph cluster_legend {{
  label = 'Legend'
  style = dashed
  legend1 [label = 'Defined by eDNA TG\\n(Red text)', fontcolor='red']
  legend2 [label = 'To discuss with TDWG/GSC community\\n(Numbered D1, D2, ...)', fillcolor = '#AFEEEE', style = 'filled']
  legend3 [label = 'Action item for eDNA TG\\n(Numbered A1, A2, ...)', fillcolor='#FFF59D', style = 'filled']
  legend4 [label = 'Question\\n(Numbered Q1, Q2, ...)', fillcolor = '#D3D3D3', style = 'filled']
  legend5 [label = 'Porposal from eDNA TG\\n(Include/Exclude/Propose)\\n(Numbered P1, P2, ...)']
  }}

  # Define nodes
  faireterm [label = 'FAIRe term: {term}\\nMapping to: {map_to}\\nObject ID: {object_id}\\nObject label: {object_label}']
  Q1_existInStd [label = 'Q1.Does the term exist in {map_to}?', fillcolor = '#D3D3D3', style = 'filled']
  Q1_1 [label = 'Q1.1. Does the term exist in another standard?', fillcolor = '#D3D3D3', style = 'filled']
  Q1_2 [label ='Q1.2. Which standard?\\n{q1_2_entry}', fillcolor = '#D3D3D3', style = 'filled']
  Q2_modFromOriginal [label = 'Q2.Any modification made\\nfrom the original term?', fillcolor = '#D3D3D3', style = 'filled']
  Q3_modToAlign [label = 'Q3.Was the modification to align with equivalent terms from other standards?\\n(i.e., dwc:eventDate vs. mixs:collection_date)', fillcolor = '#D3D3D3', style = 'filled']
  Q4_otherStd [label = 'Q4.What was the other stanadrd aligned?\\n{q4_entry}', fillcolor = '#D3D3D3', style = 'filled']
  Q5_mapEstablished [label = 'Q5.Have the mapping and interoperability between\\nthe standards been extablished?', fillcolor = '#D3D3D3', style = 'filled']
  Q6_whatMod [label = 'Q6.What was modified?', fillcolor = '#D3D3D3', style = 'filled']
  
  termName [label = 'Term name']
  description [label = 'Description']
  example [label = 'Example']
  termType [label = 'Term type']
  
  Q7a_isCritical_name [label = 'Q7-a.Is the modification critical?', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  Q7d_isCritical_type [label = 'Q7-d.Is the modification critical? (i.e., to\\nenhance machine readability/indexing', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  Q7b_isCritical_description [label = 'Q7-b.Is the modification critical?', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  Q7c_isCritical_example [label = 'Q7-c.Is the modification critical?', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  
  Q8_contVocab2 [label = 'Q8.Was the term type modified to a controlled vocabulary?', fillcolor = '#D3D3D3', style ='filled']
  Q9_vocabListSource [label = 'Q9.Was the FAIRe vocabulary list sourced from somewhere\\n(e.g., Ontobee, ENVO, MIxS enumerations)?', fillcolor = '#D3D3D3', style = 'filled']
  Q9_1 [label = 'Q9.1.Which source?\\n{q9_1_entry}', fillcolor = '#D3D3D3', style = 'filled']
  Q10_vocabListSourceMod [label = 'Q10.Was the sourced vocablary list modified (i.e., extended)?', fillcolor = '#D3D3D3', style = 'filled']
  Q11_keepTerm [label = 'Q11.Propose the FAIRe {term} term\\nin the draft {map_to} eDNA extension?', fontcolor ='red', fillcolor = '#D3D3D3', style = 'filled']
  Q12_contVocab [label ='Q12.Is it controlled vocabulary?', fillcolor = '#D3D3D3', style ='filled']
  Q13_isCritical [label = 'Q13.Is it critical to define the term\\nas a controlled vocabulary?\\n(i.e., to enhance machine readability)', fontcolor='red', fillcolor = '#D3D3D3', style = 'filled']
  Q14_vocabListSource2 [label = 'Q14.Was the FAIRe vocabulary list sourced from somewhere\\n(e.g., Ontobee, ENVO, MIxS enumerations)?', fillcolor = '#D3D3D3', style = 'filled']
  Q15_vocabListSourceMod2 [label = 'Q15.Was the existing vocablary list modified (i.e., extended)?', fillcolor = '#D3D3D3', style = 'filled']
  
  P1 [label = 'P1.INCLUDE the {map_to} {object_label} term (without the\\nmodification) in the draft {map_to} eDNA checklist.', style ='']
  P2 [label = 'P2.INCLUDE the {map_to} {object_label} term (WITH or WITHOUT the\\nmodification, depending on the outcome above)\\nin the draft {map_to} eDNA checklist.']
  P3 [label = 'P3.INCLUDE the {map_to} {object_label} term (as-is) in the\\ndraft {map_to} eDNA checklist.', style ='']
  P4 [label = 'P4.INCLUDE the {map_to} {object_label} term (as-is) in the\\ndraft {map_to} eDNA checklist.', style ='']
  P5 [label = 'P5.EXCLUDE the FAIRe {term} term\\nfrom the draft {map_to} eDNA checklist.']
  P6 [label = 'P6.PROPOSE the FAIRe {term} term (as-is) in the {map_to} eDNA checklist.']
  P7 [label = 'P7.PROPOSE the FAIRe {term} term as a non-controlled vocabulary\\nterm in the draft {map_to} eDNA checklist.']
  P8 [label = 'P8.PROPOSE the FAIRe {term} term (as-is, as a controlled vocabulary)\\nin the draft {map_to} eDNA checklist']
  P9 [label = 'P9.PROPOSE the FAIRe {term} term (as-is, as a controlled\\nvocabulary) in the {map_to} eDNA checklist.']
  
  A1a_whyMod_name [label= 'A1-a.Describe the reason\\nfor the modification.\\n{a1a_entry}', fillcolor = '#FFF59D', style = 'filled']
  A1b_whyMod_description [label= 'A1-b.Describe the reason\\nfor the modification.\\n{a1b_entry}', fillcolor = '#FFF59D', style = 'filled']
  A1c_whyMod_example [label= 'A1-c.Describe the reason\\nfor the modification.\\n{a1c_entry}', fillcolor = '#FFF59D', style = 'filled']
  A1d_whyMod_type [label= 'A1-d.Describe the reason\\nfor the modification.\\n{a1d_entry}', fillcolor = '#FFF59D', style = 'filled']
  A2_establishMap [label = 'A2.Establish mapping', fillcolor = '#FFF59D', style = 'filled']
  A3 [label = 'A3.Describe the reason why the term was needed in\\nFAIRe but not in the draft DwC/MIxS eDNA checklist.\\n{a3_entry}', fillcolor = '#FFF59D', style = 'filled']
  
  D1 [label = 'D1.Discuss the need for the modification, and decide whether it should\\nbe applied only to the draft {map_to} eDNA checklist or more widely.', fillcolor = '#AFEEEE', style = 'filled']
  D2 [label = 'D2.Discuss the need for the modification, and decide whether it should\\nbe applied only to the draft {map_to} eDNA checklist or more widely.', fillcolor = '#AFEEEE', style = 'filled']
  D3 [label = 'D3.Discuss the need to:\\n1) Create a new controlled vocabulary list with ontology term servers (i.e., Ontobee\\n2) Modify the type of {map_to} {object_label} term to a controlled vocabulary', fillcolor = '#AFEEEE', style = 'filled']
  D4 [label = 'D4.Discuss the need to modify the type of {map_to} {object_label} term to a\\ncontrolled vocabulary, using an exisitng controlled vocabulary list', fillcolor = '#AFEEEE', style = 'filled']
  D5 [label = 'D5.Discuss the need to:\\n1) Propose additional controlled vocabulary terms to ongology term servers (i.e., Ontobee)\\n2) Modify the type of a {map_to} {object_label} term to a controlled vocabulary, using exisitng/additional controlled vocabulary lists', fillcolor = '#AFEEEE', style = 'filled']
  D6 [label = 'D6.Confirm inclusion of the proposed term in the {map_to} eDNA checklist.', fillcolor = '#AFEEEE', style = 'filled']
  D7 [label = 'D7.Discuss the need to propose the new controlled\\nvocabulary list with ontology term servers (i.e. Ontobee).', fillcolor = '#AFEEEE', style = 'filled']
  D9 [label = '{D9_label}', fillcolor = '#AFEEEE', style = 'filled']
  D8 [label = 'D8.Discuss the need to propose the additional controlled\\nvocabulary terms to ongology term servers (i.e., Ontobee).', fillcolor = '#AFEEEE', style = 'filled']
  
  
  # Define edges
  
  faireterm -> Q1_existInStd [{red_arrow}]
  
  Q1_existInStd -> Q2_modFromOriginal [label = 'Yes', {q1_y}]
  Q1_existInStd -> Q1_1 [label = 'No', {q1_n}]
  Q1_1 -> Q1_2 [label = 'yes', {q1_1_y}]
  Q1_1 -> Q11_keepTerm [label = 'No', {q1_1_n}]
  Q1_2 -> Q11_keepTerm [{q1_1_y}]
  
  Q2_modFromOriginal -> Q3_modToAlign [label = 'Yes', {q2_y}]
  Q2_modFromOriginal -> P4[label = 'No', {q2_n}]
  
  Q3_modToAlign -> Q4_otherStd [label = 'Yes', {q3_y}]
  Q3_modToAlign -> Q6_whatMod [label = 'No', {q3_n}]
  
  Q4_otherStd -> Q5_mapEstablished [{q3_y}]
  Q5_mapEstablished -> P3 [label = 'Yes', {q5_y}]
  Q5_mapEstablished -> A2_establishMap [label = 'No', {q5_n}]
  
  A2_establishMap -> P3 [{q5_n}]
  
  Q6_whatMod -> termName [{q6_term_name}]
  Q6_whatMod -> description [{q6_definition}]
  Q6_whatMod -> example [{q6_example}]
  Q6_whatMod -> termType [{q6_term_type}]
  termName -> A1a_whyMod_name [{q6_term_name}]
  description -> A1b_whyMod_description [{q6_definition}]
  example -> A1c_whyMod_example [{q6_example}]
  
  A1a_whyMod_name -> Q7a_isCritical_name [{q6_term_name}]
  A1b_whyMod_description -> Q7b_isCritical_description [{q6_definition}]
  A1c_whyMod_example -> Q7c_isCritical_example [{q6_example}]
  
  
  Q7a_isCritical_name -> P1 [label ='No', {q7a_n}]
  Q7a_isCritical_name -> D1 [label = 'Yes', {q7a_y}]
  D1 -> P2 [{d1_p2_arrow}]
  Q7b_isCritical_description -> P1 [label ='No', {q7b_n}]
  Q7b_isCritical_description -> D1 [label = 'Yes', {q7b_y}]

  Q7c_isCritical_example -> P1 [label ='No', {q7c_n}]
  Q7c_isCritical_example -> D1 [label = 'Yes', {q7c_y}]

  
  
  termType -> A1d_whyMod_type [{q6_term_type}]
  A1d_whyMod_type -> Q7d_isCritical_type [{q6_term_type}]
  Q7d_isCritical_type -> P1 [label = 'No', {q7d_n}]
  Q7d_isCritical_type -> Q8_contVocab2 [label ='Yes', {q7d_y}]
  Q8_contVocab2 -> Q9_vocabListSource [label = 'Yes', {q8_y}]
  Q8_contVocab2 -> D2 [label = 'No', {q8_n}]
  
  Q9_vocabListSource -> Q9_1 [label = 'Yes', {q9_y}]
  Q9_1 -> Q10_vocabListSourceMod [{q9_y}]
  Q9_vocabListSource -> D3 [label = 'No (created new)', {q9_n}]
  Q10_vocabListSourceMod -> D4 [label ='No', {q10_n}]
  Q10_vocabListSourceMod -> D5 [label ='Yes', {q10_y}]
  
  Q11_keepTerm -> A3 [label = 'No', {q11_n}] 
  A3 -> P5 [{q11_n}]
  Q11_keepTerm -> Q12_contVocab [label = 'Yes', {q11_y}]

  Q12_contVocab -> P6 [label ='No', {q12_n}]
  P6 -> D6 [{q12_n}]
  Q12_contVocab -> Q13_isCritical [label ='Yes', {q12_y}]
  Q13_isCritical -> P9 [label ='Yes', {q13_y}]
  P9 -> Q14_vocabListSource2 [{q13_y}]
  Q14_vocabListSource2 -> Q15_vocabListSourceMod2 [label = 'Yes', {q14_y}]
  Q13_isCritical -> P7 [label ='No', {q13_n}]
  P7 -> D6 [{q13_n}]
  D6 -> D9 [{q13_n}]
  Q14_vocabListSource2 -> D7 [label = 'No (created new)', {q14_n}]
  D7 -> D9 [{q14_n}]
  Q15_vocabListSourceMod2 -> D8 [label = 'Yes', {q15_y}]
  Q15_vocabListSourceMod2 -> P8 [label = 'No', {q15_n}]
  D8 -> D9 [{q15_y}]
  P8 -> D9 [{q15_n}]
}}
")
  print(paste0('faire:',term))
  
  graph <- grViz(as.character(tree_code))
  print(graph)
  if (plot_save == TRUE) {
    if (dir.exists(paste0(plot_save_path, '/tree_plot')) == F) dir.create(paste0(plot_save_path,'/tree_plot'))
    
    ## Save tree
    svg <- export_svg(graph) # Convert to SVG string
    svg_raw <- charToRaw(svg) # Convert SVG string to raw vector
    img <- rsvg::rsvg(svg_raw) # Render and save as JPG
    image <- magick::image_read(img)
    image_write(image, path = paste0(plot_save_path, '/tree_plot/', term, '_map_to_', map_to, '.jpg'), format = "jpg")
    
  }
  
}
##### Half tree function ####
half_tree_fun <- function(term, map_to, plot_save = F, plot_save_path=getwd()){
  gs4_deauth() # Deauthorize so it doesn't prompt for login (works only for public sheets)
  sheet_url <- "https://docs.google.com/spreadsheets/d/1WxiiFNDMOjaucp5WXk-fKSXC3dH2e7jzsAiPnSR8pdU/edit?usp=sharing" # link to the google sheet "FAIRe_checklist_v1.0_decision_tree"
  mapping_df <- as.data.frame(read_sheet(sheet_url, sheet = map_to)) 
  #mapping_df <- read_excel('data_std/TDWG_GSC_TG/mapping/FAIRe_checklist_v1.0_decision_tree_downloaded20250723.xlsx', sheet = map_to)
  
  temp <- mapping_df[which(mapping_df$term_name == term),]
  
  # FAIRe term info
  faire_req_lev <- temp$requirement_level
  if (!is.na(temp$requirement_level_condition)) faire_req_lev <- paste0(faire_req_lev, ' (', temp$requirement_level_condition, ')')
  faire_definition <- temp$description
  #faire_definition_wrapped <- paste(strwrap(faire_definition, width = 80), collapse = "\\n") # Wrap to ~80 characters per line, then collapse with \n
  faire_example <- temp$example
  #faire_example_wrapped <- paste(strwrap(faire_example, width = 80), collapse = "\\n") # Wrap to ~80 characters per line, then collapse with \n
  
  faire_term_type <- temp$term_type
  if (temp$term_type == 'controlled vocabulary') {
    cont_vocab_ls <- paste0("Controlled vocabulary options: ", temp$controlled_vocabulary_options)
    #cont_vocab_ls <- paste(strwrap(cont_vocab_ls, width = 80), collapse = "\\n")
  } else {
    cont_vocab_ls <- ""
  }
  
  # Object term info
  object_id <- temp[["object_id"]]
  object_label <- temp[["object_label"]]
  object_definition <- temp[["object_definition"]]
  # Wrap object_definition to ~80 characters per line, then collapse with \n
  #object_definition_wrapped <- paste(strwrap(object_definition, width = 80), collapse = "\\n")
  object_example <- temp[["object_example"]]
  object_term_type <- temp[["object_term_type"]]
  
  ### Specify arrow colors 
  red_arrow <- "color = 'red', penwidth = 3"
  gray_arrow <- "color = 'gray', penwidth = 1"
  black_arrow <- "color = 'black', penwidth = 1"
  
  #make all default settings first
  arrow_ls <- c("q1_y", "q1_n", "q1_1_y", "q1_1_n", "q2_y", "q2_n", "q3_y", "q3_n", "q5_y", "q5_n", 
                "q6_term_name", "q6_definition", "q6_example", "q6_term_type", 
                "q7a_y", "q7a_n", "q7b_y", "q7b_n", "q7c_y", "q7c_n", "q7d_y", "q7d_n", 
                "d1_p2_arrow", "q8_y", "q8_n", "q9_y", "q9_n", "q10_y", "q10_n", "q11_y", "q11_n", 
                "q12_y", "q12_n", "q13_y", "q13_n", "q14_y", "q14_n", "q15_y", "q15_n")
  for (i in arrow_ls) {
    assign(i, gray_arrow)
  }
  
  entry_ls <- c('q1_2_entry', 'q4_entry', 'q9_1_entry','a3_entry', 'a1a_entry', 'a1b_entry', 'a1c_entry', 'a1d_entry')
  for (i in entry_ls) {
    assign(i, "")
  }
  
  
  
  # customise arrows and entries
  if (is.na(object_id)) { # Q1 = no, doesn't exist in {map_to} hence created new
    q1_y <- black_arrow
    q1_n <- red_arrow
    if (!is.na(temp$Q1_1_term_in_other_standard)) {
      if (temp$Q1_1_term_in_other_standard == 1) {
        q1_1_y <- red_arrow
        q1_1_n <- black_arrow
        if (!is.na(temp$Q1_2_which_other_standard)) q1_2_entry <- temp$Q1_2_which_other_standard
        
      } else if (temp$Q1_1_term_in_other_standard == 0) {
        q1_1_y <- black_arrow
        q1_1_n <- red_arrow
      }
      
    }
    
    
    if (!is.na(temp$Q11_propose_term_0_1)) {
      if (temp$Q11_propose_term_0_1 == 1) { # Q11 = yes
        q11_y <- red_arrow
        q11_n <- black_arrow
        if (temp$term_type == 'controlled vocabulary') {
          q12_y <- red_arrow
          q12_n <- black_arrow
          if (!is.na(temp$Q13_cont_vocab_critical_0_1)) {
            if (temp$Q13_cont_vocab_critical_0_1 == 1) {
              q13_y <- red_arrow
              q13_n <- black_arrow
            } else if (temp$Q13_cont_vocab_critical_0_1 == 0) {
              q13_y <- black_arrow
              q13_n <- red_arrow
            }
          }
          
        } else {
          q12_y <- black_arrow
          q12_n <- red_arrow
          
        }
      } else if (temp$Q11_propose_term_0_1 == 0) { # Q11 = No
        q11_y <- black_arrow
        q11_n <- red_arrow
        if (!is.na(temp$A3_reason_not_propose)) a3_entry <- temp$A3_reason_not_propose  
        
      }
    }
  } else if (!is.na(object_id)) { # Q1 = yes, exists in {map_to}
    q1_y <- red_arrow
    q1_n <- black_arrow
    
    ## Modification
    cols <- c("Q6_name_mod_0_1", "Q6_description_mod_0_1", "Q6_example_mod_0_1", "Q6_type_mod_0_1")
    if (sum(temp[cols], na.rm=T) > 0) {# Q2 = Yes, modified from {map_to}
      q2_y <- red_arrow
      q2_n <- black_arrow
    } else if (all(!is.na(temp[cols]))) {
      if (all(temp[cols]) == 0) { # Q2 = No, not modified
        q2_y <- black_arrow
        q2_n <- red_arrow
      }
      
    }

    
    
    if (identical(temp[["Q3_align_other_std_0_1"]], 1)) {
      q2_y <- red_arrow
      q2_n <- black_arrow
      q3_y <- red_arrow
      q3_n <- black_arrow
      if (!is.na(temp$Q4_other_std_id)) q4_entry <- temp$Q4_other_std_id
      
      if (identical(temp[["Q5_mapped_0_1"]], 1)) {
        q5_y <- red_arrow
        q5_n <- black_arrow
      } else if (identical(temp[["Q5_mapped_0_1"]], 0)){
        q5_y <- black_arrow
        q5_n <- red_arrow
        
      }
    } else if (identical(temp[["Q3_align_other_std_0_1"]], 0)) { # Q3 = No
      q3_y <- black_arrow
      q3_n <- red_arrow
      # Q6 what was modified?
      if (!is.na(temp[["Q6_name_mod_0_1"]])) {
        if (temp[["Q6_name_mod_0_1"]] == 1) {
          q6_term_name <- red_arrow
          if (!is.na(temp$A1a_name_mod_reason)) a1a_entry <- temp$A1a_name_mod_reason
          if (!is.na(temp[["Q7a_name_mod_critical_0_1"]])){
            if (temp[["Q7a_name_mod_critical_0_1"]] == 1) {
              q7a_y <- red_arrow
              q7a_n <- black_arrow
            } else if (temp[["Q7a_name_mod_critical_0_1"]] == 0) {
              q7a_y <- black_arrow
              q7a_n <- red_arrow
            }
          }
        } else if (temp[["Q6_name_mod_0_1"]] == 0) {
          q6_term_name <- black_arrow
        }
      }
      if (!is.na(temp[["Q6_description_mod_0_1"]])) {
        if (temp[["Q6_description_mod_0_1"]] == 1) {
          q6_definition <- red_arrow
          if (!is.na(temp$A1b_description_mod_reason)) a1b_entry <- temp$A1b_description_mod_reason
          if (!is.na(temp[["Q7b_description_mod_critical_0_1"]])){
            if (temp[["Q7b_description_mod_critical_0_1"]] == 1) {
              q7b_y <- red_arrow
              q7b_n <- black_arrow
            } else if (temp[["Q7b_description_mod_critical_0_1"]] == 0) {
              q7b_y <- black_arrow
              q7b_n <- red_arrow
            }
          }
        } else if (temp[["Q6_description_mod_0_1"]] == 0) {
          q6_definition <- black_arrow
        }
      }
      
      if (!is.na(temp[["Q6_example_mod_0_1"]])) {
        if (temp[["Q6_example_mod_0_1"]] == 1) {
          q6_example <- red_arrow
          if (!is.na(temp$A1c_example_mod_reason)) a1c_entry <- temp$A1c_example_mod_reason
          if (!is.na(temp[["Q7c_example_mod_critical_0_1"]])){
            if (temp[["Q7c_example_mod_critical_0_1"]] == 1) {
              q7c_y <- red_arrow
              q7c_n <- black_arrow
            } else if (temp[["Q7c_example_mod_critical_0_1"]] == 0) {
              q7c_y <- black_arrow
              q7c_n <- red_arrow
            }
          }
        } else if (temp[["Q6_example_mod_0_1"]] == 0) {
          q6_example <- black_arrow
        }
      }
      
      if (!is.na(temp[["Q6_type_mod_0_1"]])) {
        if (temp[["Q6_type_mod_0_1"]] == 1) {
          q6_term_type <- red_arrow
          if (!is.na(temp$A1d_type_mod_reason) & !is.null(temp$A1d_type_mod_reason[[1]])) a1d_entry <- temp$A1d_type_mod_reason
          
          
          if (!is.na(temp[["Q7d_type_mod_critical_0_1"]])){
            if (temp[["Q7d_type_mod_critical_0_1"]] == 1) {
              q7d_y <- red_arrow
              q7d_n <- black_arrow
              if (temp[['term_type']]== "controlled vocabulary") {
                q8_y <- red_arrow
                q8_n <- black_arrow
                if (!is.na(temp[["Q9_vocab_ls_source_0_1"]])) {
                  if (temp[["Q9_vocab_ls_source_0_1"]] == 1) {
                    q9_y <- red_arrow
                    q9_n <- black_arrow
                    if (!is.na(temp$Q9_1_which_source)) q9_1_entry <- temp[["Q9_1_which_source"]]
                    if (!is.na(temp[["Q10_vocab_ls_source_mod_0_1"]])) {
                      if (temp[["Q10_vocab_ls_source_mod_0_1"]] == 1) {
                        q10_y <- red_arrow
                        q10_n <- black_arrow
                      } else if (temp[["Q10_vocab_ls_source_mod_0_1"]] == 0) {
                        q10_y <- black_arrow
                        q10_n <- red_arrow
                      }
                    }
                  } else if (temp[["Q9_vocab_ls_source_0_1"]] == 0) {
                    q9_y <- black_arrow
                    q9_n <- red_arrow
                  }
                }
              } else {
                q8_y <- black_arrow
                q8_n <- red_arrow
              }
              
            } else if (temp[["Q7d_type_mod_critical_0_1"]] == 0) {
              q7d_y <- black_arrow
              q7d_n <- red_arrow
            }
          }
        } else if (temp[["Q6_type_mod_0_1"]] == 0) {
          q6_term_type <- black_arrow
        }
      }
      
      q6_term_name <- if (identical(temp[["Q6_name_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_name_mod_0_1"]], 0)) black_arrow
      q6_definition <- if (identical(temp[["Q6_description_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_description_mod_0_1"]], 0)) black_arrow
      if (!is.na(temp[["Q6_example_mod_0_1"]])) {
        q6_example <- if (identical(temp[["Q6_example_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_example_mod_0_1"]], 0)) black_arrow
      }
      if (!is.na(temp[["Q6_type_mod_0_1"]])) {
        q6_term_type <- if (identical(temp[["Q6_type_mod_0_1"]], 1)) red_arrow else if (identical(temp[["Q6_type_mod_0_1"]], 0)) black_arrow 
      }
    } 
    if (all(!is.na(temp[cols]))) {
      if (all(temp[cols]) == 0) { # Q2 = No, not modified
        q2_y <- black_arrow
        q2_n <- red_arrow
      } 
    }
  }
  
  if (map_to == 'MIxS') {
    D9_label <- "D9.Discuss whether the FAIRe term should also be included in other MIxS checklists beyond eDNA (e.g., MINAS)."
  } else if (map_to == 'DwC' | map_to =='DNA-extension' | map_to =='DwC-DP') {
    D9_label <- "D9.Discuss which DwC-DP table(s) the FAIRe term should be included in (e.g., Event, Molecular Analyses, Nucleotide Analyses, etc.)."
  }
  
  # Remove special character that grViz() doesn't accept, and wrap them (add \\n every X characters) to change lines
  ls <- c(entry_ls, "faire_definition", "object_definition", "faire_example", "object_example", "cont_vocab_ls")
  
  safe_text_tab <- data.frame(matrix(nrow=length(ls), ncol=4)) 
  colnames(safe_text_tab) <- c('name', 'original_text', 'safe_text', 'safe_wrapped_text')
  safe_text_tab$name <- ls
  
  for (i in ls) {
    original <- get(i)
    if (!is.null(original[[1]])) {
      safe_text_tab[safe_text_tab$name==i, 'original_text'] <- original
    #safe_text <- gsub("['\"{}<>:;]", "__", original)
    safe_text <- gsub("['\"{}<>]", "__", original)
    
    safe_text <- gsub("&", " and ", safe_text)
    safe_text <- gsub("%", " percent ", safe_text)
    safe_text_tab[safe_text_tab$name==i, 'safe_text'] <- safe_text
    
    if (i %in% c("faire_definition", "object_definition", "faire_example", "object_example", "cont_vocab_ls")) text_n = 80 else text_n = 50
    wrapped <- paste(strwrap(safe_text, width = text_n), collapse = "\\n") 
    
    safe_text_tab[safe_text_tab$name==i, 'safe_wrapped_text'] <- wrapped  
    assign(i, wrapped)
    }
    
  }
  
  # Note about glue(): Double braces {{ ... }} must be used to mean literal { ... } within glue().
  
  if (q1_y == red_arrow) {
    tree_code <- glue("
  digraph flowchart {{
  
  
  # Node styles
  node [shape = box, fontname = Helvetica, fontsize = 40]
  edge [fontsize = 30]
  
  
  subgraph info {{
  label = 'Info'
  style = dashed
  label1 [
  label = 'FAIRe term: {term}\\nRequirement Level: {faire_req_lev}\\nDefinition: {faire_definition}\\nExample: {faire_example}\\nTerm type:{faire_term_type}\\n{cont_vocab_ls}\\n \\n
  Object ID: {object_id}\\nObject label: {object_label}\\nObject definition: {object_definition}\\nObject example: {object_example}\\nObject term type: {object_term_type}\\n
  Predicate ID: {temp$predicate_id}'
  ]
  }}

  #Legend box (top left)
  
  subgraph cluster_legend {{
  label = 'Legend'
  style = dashed
  legend1 [label = 'Defined by eDNA TG\\n(Red text)', fontcolor='red']
  legend2 [label = 'To discuss with TDWG/GSC community\\n(Numbered D1, D2, ...)', fillcolor = '#AFEEEE', style = 'filled']
  legend3 [label = 'Action item for eDNA TG\\n(Numbered A1, A2, ...)', fillcolor='#FFF59D', style = 'filled']
  legend4 [label = 'Question\\n(Numbered Q1, Q2, ...)', fillcolor = '#D3D3D3', style = 'filled']
  legend5 [label = 'Porposal from eDNA TG\\n(Include/Exclude/Propose)\\n(Numbered P1, P2, ...)']
  }}

  # Define nodes
  faireterm [label = 'FAIRe term: {term}\\nMapping to: {map_to}\\nObject ID: {object_id}\\nObject label: {object_label}']
  Q1_existInStd [label = 'Q1.Does the term exist in {map_to}?', fillcolor = '#D3D3D3', style = 'filled']
  Q2_modFromOriginal [label = 'Q2.Any modification made\\nfrom the original term?', fillcolor = '#D3D3D3', style = 'filled']
  Q3_modToAlign [label = 'Q3.Was the modification to align with equivalent terms from other standards?\\n(i.e., dwc:eventDate vs. mixs:collection_date)', fillcolor = '#D3D3D3', style = 'filled']
  Q4_otherStd [label = 'Q4.What was the other stanadrd aligned?\\n{q4_entry}', fillcolor = '#D3D3D3', style = 'filled']
  Q5_mapEstablished [label = 'Q5.Have the mapping and interoperability between\\nthe standards been extablished?', fillcolor = '#D3D3D3', style = 'filled']
  Q6_whatMod [label = 'Q6.What was modified?', fillcolor = '#D3D3D3', style = 'filled']
  
  termName [label = 'Term name']
  description [label = 'Description']
  example [label = 'Example']
  termType [label = 'Term type']
  
  Q7a_isCritical_name [label = 'Q7-a.Is the modification critical?', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  Q7d_isCritical_type [label = 'Q7-d.Is the modification critical? (i.e., to\\nenhance machine readability/indexing', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  Q7b_isCritical_description [label = 'Q7-b.Is the modification critical?', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  Q7c_isCritical_example [label = 'Q7-c.Is the modification critical?', fontcolor = 'red', fillcolor = '#D3D3D3', style = 'filled']
  
  Q8_contVocab2 [label = 'Q8.Was the term type modified to a controlled vocabulary?', fillcolor = '#D3D3D3', style ='filled']
  Q9_vocabListSource [label = 'Q9.Was the FAIRe vocabulary list sourced from somewhere\\n(e.g., Ontobee, ENVO, MIxS enumerations)?', fillcolor = '#D3D3D3', style = 'filled']
  Q9_1 [label = 'Q9.1.Which source?\\n{q9_1_entry}', fillcolor = '#D3D3D3', style = 'filled']
  Q10_vocabListSourceMod [label = 'Q10.Was the sourced vocablary list modified (i.e., extended)?', fillcolor = '#D3D3D3', style = 'filled']
  
  P1 [label = 'P1.INCLUDE the {map_to} {object_label} term (without the\\nmodification) in the draft {map_to} eDNA checklist.', style ='']
  P2 [label = 'P2.INCLUDE the {map_to} {object_label} term (WITH or WITHOUT the\\nmodification, depending on the outcome above)\\nin the draft {map_to} eDNA checklist.']
  P3 [label = 'P3.INCLUDE the {map_to} {object_label} term (as-is) in the\\ndraft {map_to} eDNA checklist.', style ='']
  P4 [label = 'P4.INCLUDE the {map_to} {object_label} term (as-is) in the\\ndraft {map_to} eDNA checklist.', style ='']
  
  A1a_whyMod_name [label= 'A1-a.Describe the reason\\nfor the modification.\\n{a1a_entry}', fillcolor = '#FFF59D', style = 'filled']
  A1b_whyMod_description [label= 'A1-b.Describe the reason\\nfor the modification.\\n{a1b_entry}', fillcolor = '#FFF59D', style = 'filled']
  A1c_whyMod_example [label= 'A1-c.Describe the reason\\nfor the modification.\\n{a1c_entry}', fillcolor = '#FFF59D', style = 'filled']
  A1d_whyMod_type [label= 'A1-d.Describe the reason\\nfor the modification.\\n{a1d_entry}', fillcolor = '#FFF59D', style = 'filled']
  A2_establishMap [label = 'A2.Establish mapping', fillcolor = '#FFF59D', style = 'filled']
  
  D1 [label = 'D1.Discuss the need for the modification, and decide whether it should\\nbe applied only to the draft {map_to} eDNA checklist or more widely.', fillcolor = '#AFEEEE', style = 'filled']
  D2 [label = 'D2.Discuss the need for the modification, and decide whether it should\\nbe applied only to the draft {map_to} eDNA checklist or more widely.', fillcolor = '#AFEEEE', style = 'filled']
  D3 [label = 'D3.Discuss the need to:\\n1) Create a new controlled vocabulary list with ontology term servers (i.e., Ontobee\\n2) Modify the type of {map_to} {object_label} term to a controlled vocabulary', fillcolor = '#AFEEEE', style = 'filled']
  D4 [label = 'D4.Discuss the need to modify the type of {map_to} {object_label} term to a\\ncontrolled vocabulary, using an exisitng controlled vocabulary list', fillcolor = '#AFEEEE', style = 'filled']
  D5 [label = 'D5.Discuss the need to:\\n1)propose additional controlled vocabulary terms to ongology term servers (i.e., Ontobee)\\n2)Modify the type of a {map_to} {object_label} term to a controlled vocabulary, using exisitng/additional controlled vocabulary lists', fillcolor = '#AFEEEE', style = 'filled']

  
  # Define edges
  
  faireterm -> Q1_existInStd [{red_arrow}]
  
  Q1_existInStd -> Q2_modFromOriginal [label = 'Yes', {q1_y}]
  
  Q2_modFromOriginal -> Q3_modToAlign [label = 'Yes', {q2_y}]
  Q2_modFromOriginal -> P4[label = 'No', {q2_n}]
  
  Q3_modToAlign -> Q4_otherStd [label = 'Yes', {q3_y}]
  Q3_modToAlign -> Q6_whatMod [label = 'No', {q3_n}]
  
  Q4_otherStd -> Q5_mapEstablished [{q3_y}]
  Q5_mapEstablished -> P3 [label = 'Yes', {q5_y}]
  Q5_mapEstablished -> A2_establishMap [label = 'No', {q5_n}]
  
  A2_establishMap -> P3 [{q5_n}]
  
  Q6_whatMod -> termName [{q6_term_name}]
  Q6_whatMod -> description [{q6_definition}]
  Q6_whatMod -> example [{q6_example}]
  Q6_whatMod -> termType [{q6_term_type}]
  termName -> A1a_whyMod_name [{q6_term_name}]
  description -> A1b_whyMod_description [{q6_definition}]
  example -> A1c_whyMod_example [{q6_example}]
  
  A1a_whyMod_name -> Q7a_isCritical_name [{q6_term_name}]
  A1b_whyMod_description -> Q7b_isCritical_description [{q6_definition}]
  A1c_whyMod_example -> Q7c_isCritical_example [{q6_example}]
  
  
  Q7a_isCritical_name -> P1 [label ='No', {q7a_n}]
  Q7a_isCritical_name -> D1 [label = 'Yes', {q7a_y}]
  D1 -> P2 [{d1_p2_arrow}]
  Q7b_isCritical_description -> P1 [label ='No', {q7b_n}]
  Q7b_isCritical_description -> D1 [label = 'Yes', {q7b_y}]

  Q7c_isCritical_example -> P1 [label ='No', {q7c_n}]
  Q7c_isCritical_example -> D1 [label = 'Yes', {q7c_y}]

  
  
  termType -> A1d_whyMod_type [{q6_term_type}]
  A1d_whyMod_type -> Q7d_isCritical_type [{q6_term_type}]
  Q7d_isCritical_type -> P1 [label = 'No', {q7d_n}]
  Q7d_isCritical_type -> Q8_contVocab2 [label ='Yes', {q7d_y}]
  Q8_contVocab2 -> Q9_vocabListSource [label = 'Yes', {q8_y}]
  Q8_contVocab2 -> D2 [label = 'No', {q8_n}]
  
  Q9_vocabListSource -> Q9_1 [label = 'Yes', {q9_y}]
  Q9_1 -> Q10_vocabListSourceMod [{q9_y}]
  Q9_vocabListSource -> D3 [label = 'No (created new)', {q9_n}]
  Q10_vocabListSourceMod -> D4 [label ='No', {q10_n}]
  Q10_vocabListSourceMod -> D5 [label ='Yes', {q10_y}]
  
  
}}
")
  } else if (q1_y == black_arrow) {
    tree_code <- glue("
  digraph flowchart {{
  
  # Node styles
  node [shape = box, fontname = Helvetica, fontsize=40]
  edge [fontsize = 30]
  
  subgraph info {{
  label = 'Info'
  style = dashed
  label1 [
  label = 'FAIRe term: {term}\\nRequirement Level: {faire_req_lev}\\nDefinition: {faire_definition}\\nExample: {faire_example}\\nTerm type:{faire_term_type}\\n{cont_vocab_ls}'
  ]
  }}

  #Legend box (top left)
  # subgraph cluster_legend {{ 
  # label = 'Legend'
  # style = dashed
  # legend1 [label = 'Defined by eDNA TG\\n(Red text)', fontcolor='red']
  # legend2 [label = 'To discuss with TDWG/GSC community\\n(Numbered D1, D2, ...)', fillcolor = '#AFEEEE', style = 'filled']
  # legend3 [label = 'Action item for eDNA TG\\n(Numbered A1, A2, ...)', fillcolor='#FFF59D', style = 'filled']
  # legend4 [label = 'Question\\n(Numbered Q1, Q2, ...)', fillcolor = '#D3D3D3', style = 'filled']
  # legend5 [label = 'Porposal from eDNA TG\\n(Include/Exclude/Propose)\\n(Numbered P1, P2, ...)']
  # }} 
  
  # Define nodes
  faireterm [label = 'FAIRe term: {term}\\nMapping to: {map_to}\\nObject ID: {object_id}\\nObject label: {object_label}']
  Q1_existInStd [label = 'Q1.Does the term exist in {map_to}?', fillcolor = '#D3D3D3', style = 'filled']
  Q1_1 [label = 'Q1.1. Does the term exist in another standard?', fillcolor = '#D3D3D3', style = 'filled']
  Q1_2 [label ='Q1.2. Which standard?\\n{q1_2_entry}', fillcolor = '#D3D3D3', style = 'filled']
  
  Q11_keepTerm [label = 'Q11.Propose the FAIRe {term} term\\nin the draft {map_to} eDNA extension?', fontcolor ='red', fillcolor = '#D3D3D3', style = 'filled']
  Q12_contVocab [label ='Q12.Is it controlled vocabulary?', fillcolor = '#D3D3D3', style ='filled']
  Q13_isCritical [label = 'Q13.Is it critical to define the term\\nas a controlled vocabulary?\\n(i.e., to enhance machine readability)', fontcolor='red', fillcolor = '#D3D3D3', style = 'filled']
  Q14_vocabListSource2 [label = 'Q14.Was the FAIRe vocabulary list sourced from somewhere\\n(e.g., Ontobee, ENVO, MIxS enumerations)?', fillcolor = '#D3D3D3', style = 'filled']
  Q15_vocabListSourceMod2 [label = 'Q15.Was the existing vocablary list modified (i.e., extended)?', fillcolor = '#D3D3D3', style = 'filled']
  
  P5 [label = 'P5.EXCLUDE the FAIRe {term} term\\nfrom the draft {map_to} eDNA checklist.']
  P6 [label = 'P6.PROPOSE the FAIRe {term} term (as-is) in the {map_to} eDNA checklist.']
  P7 [label = 'P7.PROPOSE the FAIRe {term} term as a non-controlled vocabulary\\nterm in the draft {map_to} eDNA checklist.']
  P8 [label = 'P8.PROPOSE the FAIRe {term} term (as-is, as a controlled vocabulary)\\nin the draft {map_to} eDNA checklist']
  P9 [label = 'P9.PROPOSE the FAIRe {term} term (as-is, as a controlled\\nvocabulary) in the {map_to} eDNA checklist.']
  
  A3 [label = 'A3.Describe the reason why the term was needed in\\nFAIRe but not in the draft DwC/MIxS eDNA checklist.\\n{a3_entry}', fillcolor = '#FFF59D', style = 'filled']
  
  D6 [label = 'D6.Confirm inclusion of the proposed term in the {map_to} eDNA checklist.', fillcolor = '#AFEEEE', style = 'filled']
  D7 [label = 'D7.Discuss the need to propose the new controlled\\nvocabulary list with ontology term servers (i.e. Ontobee).', fillcolor = '#AFEEEE', style = 'filled']
  D9 [label = '{D9_label}', fillcolor = '#AFEEEE', style = 'filled']
  D8 [label = 'D8.Discuss the need to propose the additional controlled\\nvocabulary terms to ongology term servers (i.e., Ontobee).', fillcolor = '#AFEEEE', style = 'filled']
  
  
  # Define edges
  
  faireterm -> Q1_existInStd [{red_arrow}]
  
  Q1_existInStd -> Q1_1 [label = 'No', {q1_n}]
  Q1_1 -> Q1_2 [label = 'yes', {q1_1_y}]
  Q1_1 -> Q11_keepTerm [label = 'No', {q1_1_n}]
  Q1_2 -> Q11_keepTerm [{q1_1_y}]
  
  
  
  Q11_keepTerm -> A3 [label = 'No', {q11_n}] 
  A3 -> P5 [{q11_n}]
  Q11_keepTerm -> Q12_contVocab [label = 'Yes', {q11_y}]

  Q12_contVocab -> P6 [label ='No', {q12_n}]
  P6 -> D6 [{q12_n}]
  Q12_contVocab -> Q13_isCritical [label ='Yes', {q12_y}]
  Q13_isCritical -> P9 [label ='Yes', {q13_y}]
  P9 -> Q14_vocabListSource2 [{q13_y}]
  Q14_vocabListSource2 -> Q15_vocabListSourceMod2 [label = 'Yes', {q14_y}]
  Q13_isCritical -> P7 [label ='No', {q13_n}]
  P7 -> D6 [{q13_n}]
  D6 -> D9 [{q13_n}]
  Q14_vocabListSource2 -> D7 [label = 'No (created new)', {q14_n}]
  D7 -> D9 [{q14_n}]
  Q15_vocabListSourceMod2 -> D8 [label = 'Yes', {q15_y}]
  Q15_vocabListSourceMod2 -> P8 [label = 'No', {q15_n}]
  D8 -> D9 [{q15_y}]
  P8 -> D9 [{q15_n}]
}}
")
  }
  
  print(paste0('faire:',term))
  graph <- grViz(as.character(tree_code))
  print(graph)
  
  if (plot_save == T) {
    if (dir.exists(paste0(plot_save_path, '/half_tree_plot')) == F) dir.create(paste0(plot_save_path, '/half_tree_plot')) 
    ## Save tree
    svg <- export_svg(graph) # Convert to SVG string
    svg_raw <- charToRaw(svg) # Convert SVG string to raw vector
    img <- rsvg::rsvg(svg_raw) # Render and save as JPG
    image <- magick::image_read(img)
    image_write(image, path = paste0(plot_save_path, '/half_tree_plot/', term, '_map_to_', map_to, '.jpg'), format = "jpg")
    
  }
  
}
