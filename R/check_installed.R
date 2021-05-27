## this is a modified version from package rlang that only uses base R functionality.


## manual can be either TRUE or a string with installation instructions.
check_installed <- function (pkg, manual = FALSE)
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
    
    if (is.logical(manual) && !manual) {
      question <-
        "Would you like to install the package(s)?"
      cat(info, "\n", question, sep = '')
      if (utils::menu(c("Yes", "No")) != 1) {
        invokeRestart("abort")
      }
      
      utils::install.packages(missing_pkgs)
    }else{
      cat(info,
        "\n",
        "The Package(s) need to be installed manually.",
        "\n",
        sep = '')
      if (is.character(manual))
        cat(manual)
      
      invokeRestart("abort")
    }
  }
     
  invisible(TRUE)
}