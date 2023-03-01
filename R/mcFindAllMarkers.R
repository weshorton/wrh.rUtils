mcFindAllMarkers <- function(obj, nCore_v, onlyPos_v = F, test_v = "MAST", latent_v) {
  #' Run FindAllMarkers() on multiple cores
  #' @description 
  #' Adapted from https://gist.github.com/diazdc/1735102c243cd16acb1b1f3fd09a26e1
  #' https://github.com/diazdc/seurat-extensions/blob/master/R/cluster_markers.R
  #' Instead of running FindAllMarkers, run FindMarkers() in parallel for each cluster.
  #' @param obj seurat object. Make sure clusters of interest are set as Idents()
  #' @param nCore_v number of cores to split on.
  #' @param onlyPos_v logical, passed to only.pos argument in FindMarkers.
  #'                  default F. TRUE - only output positive log2FC markers
  #'                             FALSE - output positive and negative markers
  #' @param test_v which test to use. Passed to test.use in FindMarkers
  #' @param latent_v vector of latent variables to account for in DE testing.
  #' @return data.table of markers
  #' @export

  ### Get extra arguments
  extraParams_lsv <- list(...)
  
  ### Grab clusters
  clusters_v <- levels(obj)
  
  ### Empty list
  markers_ls <- list()[length(clusters_v)]
  
  ### Run
  markers_ls <- parallel::mclapply(clusters_v, mc.cores = nCore_v, function(x) {
    ### Get other clusters
    other_v <- clusters_v[clusters_v != x]
    ### Run Find Markers
    markers_dt <- FindMarkers(object = obj,
                              ident.1 = x,
                              ident.2 = other_v,
                              only.pos = onlyPos_v,
                              test.use = test_v,
                              latent.vars = latent_v)
    ### Turn to data.table
    markers_dt <- convertDFT(markers_df, newName_v = "Gene")
    
    ### Add cluster ID
    markers_dt$cluster <- rep(x, nrow(markers_dt))
    
    ### Output
    return(markers_dt)
  })
  
  ### Combine results into a large data.table
  allMarkers_dt <- dplyr::bind_rows(markers_ls)
  
  ### Return
  return(allMarkers_dt)
  
} # mcFindAllMarkers