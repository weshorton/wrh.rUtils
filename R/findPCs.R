findPCs <- function(seurat_obj, drName_v="pca", min_v = T, elbow_v = T, ndims_v = 30) {
  #' Find maximum recommended PC to use.
  #' @description 
    #' Using a seurat object with PCA dimensional reduction run on it, determine the highest PC that 
    #' still provides information about variance between cells. 
    #' Adapted from: https://hbctraining.github.io/scRNA-seq/lessons/elbow_plot_metric.html
  #' @param seurat_obj A standard seurat object that has a "pca" dimensional reduction
  #' @param drName_v Name of pca dimensional reduction. Default is 'pca', which is Seurat default. 
  #' If you create a PCA on specific genes and label it something else, provide that label name 
  #' here (e.g. ccs_pca) for a PCA on cell cycle genes.
  #' @param min_v logical indicating to take the minimum cut-off of the two
  #' @param elbow_v logical indicating to make elbow plot
  #' @param ndims_v number of PCs to print in elbow plot
  #' @details Two cut-offs are calculated
    #' 1. The point where an individual PC contributes less than 5% of standard deviation
    #' and the cumulative PCs contribute at least 90% of the standard deviation
    #' 1. The point where the percent change in variation between the consecutive PCs is less than 0.1%
  #' @return A single numeric value with the recommended PC cut-off
  #' @export
  
  # pct var associated with each PC
  pctVar <- seurat_obj[[drName_v]]@stdev / sum(seurat_obj[[drName_v]]@stdev) * 100
  
  # calculate cumulative percents
  cumPct <- cumsum(pctVar)
  
  # Get cut-offs
  co1 <- which(cumPct > 90 & pctVar < 5)[1]
  co2 <- sort(which((pctVar[1:length(pctVar) - 1] - pctVar[2:length(pctVar)]) > 0.1), decreasing = T)[1] + 1
  
  # select and update
  cat(sprintf("Cut-off one: %s\nCut-off two: %s\n", co1, co2))
  if (min_v) {
    pc <- min(co1, co2)
    cat(sprintf("Taking smaller cut-off: %s.\n", pc))
  } else {
    pc <- max(co1, co2)
    cat(sprintf("Taking larger cut-off: %s.\n", pc))
  }
  
  # plot
  if (elbow_v) {
    print(ElbowPlot(seurat_obj, ndims = ndims_v, reduction = drName_v))
  }
  
  # Return selected PC
  return(pc)
  
} # findPCs
