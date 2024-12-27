add_fair_theory <- function(path = ".",
                            theory_file = NULL,
                            add_license = "CC0"
                            ){
  # 1. Add theory file
  has_theory_file <- !is.null(theory_file)
  if(has_theory_file){
    existing_theory_file <- file.exists(theory_file)
  }
  if(existing_theory_file) theory_file <- normalizePath(theory_file)
  
  # 1. Create project folder
  oldwd <- getwd()
  if(!dir.exists(path)){
    dir.create(path)
    setwd(path)
  }
  if(has_theory_file){
    if(existing_theory_file){
      file.copy(theory_file, file.path(getwd(), basename(theory_file)))
    } else {
      file.create(file.path(getwd(), theory_file))
    }
  }
  # 1. Add LICENSE file
  # Begin license
  if(!add_license == "none"){
    tryCatch({
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      
      # copy 'resources' folder to path
      license_dir = system.file('rstudio', 'templates', 'project', 'licenses', package = 'worcs', mustWork = TRUE)
      license_file <- file.path(license_dir, paste0(add_license, ".txt"))
      file.copy(license_file, file.path(path, "LICENSE"), copy.mode = FALSE)
      col_message("Writing license file.", verbose = verbose)
    }, error = function(e){
      col_message("Writing license file.", success = FALSE)
    })
  }
  # End license
  # 1. Add readme.md
  # * Include project Title, Description, Usage of the theory file, How to Contribute, How to Cite the FAIR theory, Related Works (e.g., the paper that documents the theory)
  # 1. Add .zenodo.json file to provide metadata that will allow Zenodo to index the repository as a FAIR theory
  # 1. Initialize a Git repository in the project folder
  # 1. Create Git remote repository (e.g., on GitHub)
  # 1. Connect local to remote Git repository
  # 1. Add the repository to Zenodo
  # * Sync Zenodo to import recent GitHub repositories (may take some time before it shows)
  # * Flip Switch
  # 1. Publish a release on GitHub
  # 1. *Optional:* Edit Zenodo metadata (e.g., to add formal cross-references to related works) https://help.zenodo.org/docs/deposit/manage-records/#edit
  #   
    
    tmp <- getwd(); setwd("c:/git_repositories/empirical_cycle"); usethis::use_cc0_license(); setwd(tmp)
}
library(jsonlite)
tmp <- jsonlite::read_json("c:/git_repositories/empirical_cycle/.zenodo.json")

dput(tmp, "clipboard")

to_json <- list(
  resource_type = "model",
  keywords = list("FAIRtheory"),
  communities = list(list(identifier = "fairtheory")),
  # related_identifiers = list(list(scheme = "doi", identifier = "10.1515/9783112313121", 
  #                                    relation = "References", resource_type = "publication-book"))
  )

