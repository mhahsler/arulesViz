## this is a modified version from package rlang that only uses base R functionality.

check_installed <- function (pkg, repo = "cran")
{
  
  if (!is.character(pkg)) {
    stop("`pkg` must be a package name or a vector of package names.")
  }
  
  needs_install <-
    sapply(pkg, function(x)
      ! requireNamespace(x,
        quietly = TRUE))
  
  if (any(needs_install)) {
    missing_pkgs <- pkg[needs_install]
    missing_pkgs_enum <- paste(missing_pkgs, collapse = ", ")
    
    info <-
      paste("The", missing_pkgs_enum,
        "package(s) is/are required.")
    
    if (!interactive()) {
      stop(info)
    }
    
    if (tolower(repo) != "cran") {
      cat(info, "\n", "Packages need to be installed manually from ", repo, sep = '')
      invokeRestart("abort")
    }
    
    question <-
      "Would you like to install the package(s)?"
    cat(info, "\n", question, sep = '')
    if (utils::menu(c("Yes", "No")) != 1) {
      invokeRestart("abort")
    }
    
    utils::install.packages(missing_pkgs)
  }
  
  invisible(TRUE)
}