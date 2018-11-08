detachAllPackages <- function(unload_v = F) {
  #' Clear loaded packages
  #' @description Clear all loaded packages (excluding base). Useful for determining if a script has the appropriate packages loaded. 
  #' @param unload argument to detach. See ?detach for explanation.
  #' @return None. Detaches loaded packages.
  #' @examples
  #' detachAllPackages()
  #' @export
  
  ## Get names of packages to remove
  pkgToDetach_v <- paste0('package:', names(sessionInfo()$otherPkgs))
  
  ## Remove them
  if (length(pkgToDetach_v) > 0) invisible(lapply(pkgToDetach_v, detach, character.only = T, unload = unload_v))
    
} # detachAllPackages
