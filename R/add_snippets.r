#' @title Add qcscript Snippets to Rstudio
#'
#' @description \code{add_snippets} copies all (missing) snippet definitions
#'   in 'inst/Rsnippets.txt' to the RStudios user snippet location.
#'
#' @return boolean invisible(FALSE) if nothing was added, invisible(TRUE) if snipped definitions were added
#' @export
#'
#' @examples \dontrun{add_snippets()}
add_snippets <- function() {
  
  added <- FALSE
  
  # if not on RStudio or RStudioServer exit
  #
  if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    return(NULL)
  }
  
  # Name of files containing snippet code to copy
  #
  pckgSnippetsFiles <- "Rsnippets.txt"
  
  # Name of files to copy into. Order has to be the same
  # as in 'pckgSnippetsFiles'
  #
  rstudioSnippetsFiles <- "r.snippets"
  
  # Path to directory for RStudios user files depends on OS
  #
  if (.Platform$OS.type == "windows") {
    rstudioSnippetsPathBase <- file.path(Sys.getenv("APPDATA"), "RStudio", "snippets")
  } else {
    rstudioSnippetsPathBase <- file.path(path.expand('~'), ".config/rstudio", "snippets")
  }
  
  # Read each file in pckgSnippetsFiles and add its contents
  #
  for (i in seq_along(pckgSnippetsFiles)) {
    
    # Try to get template, if template is not found skip it
    #
    pckgSnippetsFilesPath <- system.file("rstudio", pckgSnippetsFiles[i], package = "QCfuns")
    if (pckgSnippetsFilesPath == "") {
      next()
    }
    
    # load package snippets definitions
    #
    pckgSnippetsFileContent <- readLines(pckgSnippetsFilesPath, warn = FALSE)
    
    # Extract names of package snippets
    #
    pckgSnippetsFileDefinitions <- pckgSnippetsFileContent[grepl("^snippet (.*)", pckgSnippetsFileContent)]
    
    
    # Construct path for destination file
    #
    rstudioSnippetsFilePath <- file.path(rstudioSnippetsPathBase, rstudioSnippetsFiles[i])
    
    # If targeted RStudios user file does not exist, raise error (otherwise we would 'remove')
    # the default snippets from the 'user file'
    #
    if (!file.exists(rstudioSnippetsFilePath)) {
      cat(paste0( "'", rstudioSnippetsFilePath, "' does not exist yet\n."))
      
      answer <- readline("Do you want to create an empty one (y/n): ")
      
      if (substr(tolower(answer), 1, 1) == "y"){
        dir.create(dirname(rstudioSnippetsFilePath),recursive = TRUE,showWarnings = FALSE)
        file.create(rstudioSnippetsFilePath)
      } else {
        next()
      }
    }
    
    # Extract 'names' of already existing snitppets
    #
    rstudioSnippetsFileContent <- readLines(rstudioSnippetsFilePath, warn = FALSE)
    rstudioSnippetDefinitions <- rstudioSnippetsFileContent[grepl("^snippet (.*)", rstudioSnippetsFileContent)]
    
    # replace two spaces with tab, ONLY at beginning of string
    #
    pckgSnippetsFileContentSanitized <- gsub("(?:^ {2})|\\G {2}|\\G\t", "\t", pckgSnippetsFileContent, perl = TRUE)
    
    # find defintions appearing in packageSnippets but not in rstudioSnippets
    # if no snippets are missing go to next file
    #
    snippetsAll <- trimws(pckgSnippetsFileDefinitions)
    snippetsToCopy <- setdiff(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    snippetsNotToCopy <- intersect(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    
    # Inform user about changes, ask to confirm action
    #
    if (interactive()) {
      cat(paste0("You are about to add the following ", length(snippetsAll),
                 " snippets to '", rstudioSnippetsFilePath, "':\n",
                 paste0(paste0("-", snippetsAll), collapse="\n")))
      if (length(snippetsNotToCopy) > 0) {
        cat(paste0("\n(The following snippets already exist:\n",
                   paste0(snippetsNotToCopy, collapse=", ") ,")"))
        answer0 <- readline(prompt="Do you want to remove the original snippets (y/n): ")
      } else {
        answer0 <- "No"
      }
      answer <- readline(prompt="Do you want to proceed to add the snippets (y/n): ")
      if (substr(tolower(answer), 1, 1) == "n") {
        next()
      }
    }
    
    # Remove original snippets if permitted
    # Create list of line numbers where snippet definitons start
    # This list is used to determine the end of each definition block
    #
    allrstudioSnippetDefinitonStarts <- grep("^snippet .*", rstudioSnippetsFileContent)
    allPckgSnippetDefinitonStarts <- grep("^snippet .*", pckgSnippetsFileContentSanitized)
    
    if (substr(tolower(answer0), 1, 1) == "y"){
      for (s in snippetsNotToCopy) {
        startLine0 <- grep(paste0("^", s, ".*"), rstudioSnippetsFileContent)
        
        # Find last line of snippet definition:
        # First find start of next defintion and return
        # previous line number or lastline if already in last definiton
        #
        endLine0 <- allrstudioSnippetDefinitonStarts[allrstudioSnippetDefinitonStarts > startLine0][1] -1
        if (is.na(endLine0)) {
          endLine0 <- length(rstudioSnippetsFileContent)
        }
        
        rstudioSnippetsFileContent1 <- rstudioSnippetsFileContent[-c(startLine0:endLine0)]
        snippetText0 <- paste0(rstudioSnippetsFileContent1, collapse = "\n")
        
        ### Identify the repeated snippet in package
        startLine <- grep(paste0("^", s, ".*"), pckgSnippetsFileContentSanitized)
        
        # Find last line of snippet definition:
        # First find start of next defintion and return
        # previous line number or lastline if already in last definiton
        #
        endLine <- allPckgSnippetDefinitonStarts[allPckgSnippetDefinitonStarts > startLine][1] -1
        if (is.na(endLine)) {
          endLine <- length(pckgSnippetsFileContentSanitized)
        }
        
        snippetText <- paste0(pckgSnippetsFileContentSanitized[startLine:endLine], collapse = "\n")
        
        if (length(rstudioSnippetsFileContent1) > 0){
          if (tail(rstudioSnippetsFileContent1, n=1) != "") {
            snippetText <- paste0("\n", snippetText)
          }
        }
        
        # Remove and add snippet block, print message
        #
        cat(paste0(snippetText0, snippetText), file = rstudioSnippetsFilePath)
        cat(paste0("* Removed original and added new '", s, "' to '", rstudioSnippetsFilePath, "'\n"))
        added <- TRUE
      }
    }
    
    # Add new snippets 
    
    for (s in snippetsToCopy) {
      startLine <- grep(paste0("^", s, ".*"), pckgSnippetsFileContentSanitized)
      
      # Find last line of snippet definition:
      # First find start of next defintion and return
      # previous line number or lastline if already in last definiton
      #
      endLine <- allPckgSnippetDefinitonStarts[allPckgSnippetDefinitonStarts > startLine][1] -1
      if (is.na(endLine)) {
        endLine <- length(pckgSnippetsFileContentSanitized)
      }
      
      snippetText <- paste0(pckgSnippetsFileContentSanitized[startLine:endLine], collapse = "\n")
      
      # Make sure there is at least one empty line between entries
      if (length(tail(readLines(rstudioSnippetsFilePath, warn = FALSE), n=1)) != 0){
        if (tail(readLines(rstudioSnippetsFilePath, warn = FALSE), n=1) != "") {
          snippetText <- paste0("\n", snippetText)
        }
      }
      
      # Append snippet block, print message
      #
      cat(paste0(snippetText, "\n"), file = rstudioSnippetsFilePath, append = TRUE)
      cat(paste0("* Added '", s, "' to '", rstudioSnippetsFilePath, "'\n"))
      added <- TRUE
    }
  }
  
  if (added) {
    cat("Restart RStudio to use new snippets")
  } else {
    cat("Snippets not added")
  }
  
  return(invisible(added))
  
}

