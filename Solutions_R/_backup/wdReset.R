## Check for correct project directory as working directory

wdReset <- function() {
  
  ## Check, if backup variable for original working directory exists
  if(exists("dir_project")) {
    ## If current working directory not consistent with backup
    if(getwd() != dir_project) {
      cat("Working directory will be reset to project directory")
      setwd(dir_project)
    } else {
        cat("Working directory consistent with backup variable")
      } 
    } else {
      cat("Backup variable 'dir_project' for project directory created")
      dir_project <- assign("dir_project", getwd(), env = .GlobalEnv)
    }
  }