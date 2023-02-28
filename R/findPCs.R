findPCs <- function(seurat_obj, drName_v="pca") {
  #' Find maximum recommended PC to use.
  #' @description 
  #' Using a seurat object with PCA dimensional reduction run on it, determine the highest PC that 
  #' still provides information about variance between cells.
  #' @param seurat_obj A standard seurat object that has a "pca" dimensional reduction
  #' @param drName_v Name of pca dimensional reduction. Default is 'pca', which is Seurat default. 
  #' If you create a PCA on specific genes and label it something else, provide that label name 
  #' here (e.g. ccs_pca) for a PCA on cell cycle genes.
  #' @return A single numeric value with the recommended PC cut-off
  #' @export
  
  # pct var associated with each PC
  pctVar <- seurat_obj[[drName_v]]@stdev / sum(seurat_obj[[drName_v]]@stdev) * 100
  
  # calculate cumulative percents
  cumPct <- cumsum(pctVar)
  
  # Get cut-offs
  co1 <- which(cumPct > 90 & pctVar < 5)[1]
  co2 <- sort(which((pctVar[1:length(pctVar) - 1] - pctVar[2:length(pctVar)]) > 0.1), decreasing = T)[1] + 1
  
  # select
  pc <- min(co1, co2)
  return(pc)
  
} # findPCs
