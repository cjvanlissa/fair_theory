add_fair_theory <- function(path = ".",
                            theory_file = NULL,
                            remote_repo = "https",
                            add_license = c("cc0", "ccby", "gpl", "gpl3", "agpl", "agpl3", "apache", "apl2", "lgpl", "mit", "proprietary", "none")
                            ){
  # Select first license
  if(is.null(add_license)) add_license = "none"
  add_license <- tryCatch(add_license[1], error = function(e){"none"})
  
  # 1. Create project folder
  if(!dir.exists(path)){
    dir.create(path)
  }
  
  # 2. Initialize Git repo
  gert::git_init(path = path)
  # Connect to remote repo if possible
  repo_url <- worcs:::parse_repo(remote_repo = remote_repo, verbose = FALSE)
  valid_repo <- !is.null(repo_url)
  if(valid_repo){
    tryCatch({
      Args_gert <- list(
        name = "origin",
        url = remote_repo,
        repo = path
      )
      do.call(gert::git_remote_add, Args_gert)
      col_message(paste0("Connected to remote repository at ", remote_repo), verbose = verbose)
    }, error = function(e){
      col_message("Could not connect to a remote 'GitHub' repository. You are working with a local 'Git' repository only.", success = FALSE, verbose = verbose)
    })
  } else {
    col_message("No valid 'GitHub' address provided. You are working with a local 'Git' repository only.", success = FALSE)
  }
  
  # 3. Add theory file
  has_theory_file <- !is.null(theory_file)
  if(has_theory_file){
    existing_theory_file <- file.exists(theory_file)
    theory_file <- normalizePath(theory_file)
    if(existing_theory_file){
      file.copy(theory_file, file.path(getwd(), basename(theory_file)))
    } else {
      file.create(file.path(path, theory_file))
    }
  }
  
  # 1. Add LICENSE file
  if(!add_license == "none"){
    add_license_file(repo = path, license = add_license)
  }
  
  # 1. Add readme.md
  lines_readme <- c("# FAIR theory: Theory Title Goes Here", "", "# Description", 
    "", "This is a FAIR theory, see Van Lissa et al., in preparation.", "", "# Interoperability", "", "Explain what the theory can be reused for, and how.", 
    "", "# Contributing", 
    "", "If you want to contribute to this project, please get involved. You can do so in three ways:", 
    "",
    "1. **To discuss the current implementation and discuss potential changes**, file a ‘GitHub’ issue", 
    "2. **To directly propose changes**, send a pull request containing the proposed changes",
    "3. **To create a derivative theory**, please fork the repository",
    "",
    "If you fork the repository, please cite this repository (see below), and add it as a related work (below and by adding the appropriate metadata on Zenodo).",
    "",
    "By participating in this project, you agree to abide by the [Contributor Covenant](https://www.contributor-covenant.org/version/2/0/code_of_conduct.html).", 
    "", "## Related works", "", "Optionally, cite the canonical reference for the theory implemented in this repository here. This is redundant with adding the cross-reference in Zenodo, but may be useful nonetheless.", 
    "", "## Citing this work", "", "See this project's Zenodo page for the preferred citation."
    ) 
  if(valid_repo){
    lines_readme[15:17] <- paste0(lines_readme[15:17], " [here](", repo_url, c("/issues)", "/pulls)", "/fork)"))
  }

  )
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

