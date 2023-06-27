standardPlots <- function(seurat_obj, reduction_v, clustCol_v, res_v, name_v) {
  #' Standard Seurat Plots
  #' @description
    #' Make some quick plots for unintegrated, harmony, or other objects
  #' @param seurat_obj seurat object to use for plotting
  #' @param reduction_v name of reduction to use for UMAP - usually "umap" or "harmony" 
  #' @param clustCol_v column name of seurat clusters to plot
  #' @param res_v resolution used for clusters
  #' @param name_v name to add to plot titles
  #' @return list of ggplot objects
  #' @export
  
  ### Empty list to hold plots
  out_lsgg <- list()
  
  ### Default
  out_lsgg[["clusters"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = clustCol_v, label = T, pt.size = 1) +
    coord_equal() +
    ggtitle(paste0(name_v, " clusters; res - ", res_v))
  
  ### Treatment plots
  if (!"Treatment" %in% colnames(seurat_obj@meta.data)) {
    warning("Treatment column not found. Will not make those plots")
  } else {
    
    ### Color by treatment
    out_lsgg[["cTreat"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = "Treatment", label = F, pt.size = 1) +
      coord_equal() +
      ggtitle(paste0(name_v, " clusters by Treatment"))
    
    ### Split by batch, color treatment
    out_lsgg[["fBatchcTreat"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = "Treatment", 
                                          split.by = "batchID", label = F, pt.size = 1) +
      coord_equal() + theme(legend.position = "bottom") +
      ggtitle(paste0(name_v, " clusters; res - ", res_v))
    
    ### Split by treatment
    out_lsgg[["fTreat"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = clustCol_v, 
                                    split.by = "Treatment", label = F, pt.size = 1, ncol = 2) +
      coord_equal() + theme(legend.position = "bottom") +
      ggtitle(paste0(name_v, " clusters; res - ", res_v))
  }
  
  ### Batch plots
  if (!"batchID" %in% colnames(seurat_obj@meta.data)) {
    warning("batchID column not found. Will not make those plots.")
  } else {
    
    ### Color by batch
    out_lsgg[["cBatch"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = "batchID", label = F, pt.size = 1) +
      coord_equal() +
      ggtitle(paste0(name_v, " clusters by Batch"))
    
    ### Split by batch
    out_lsgg[["fBatch"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = clustCol_v, 
                                    split.by = "batchID", label = F, pt.size = 1) +
      coord_equal() + theme(legend.position = "bottom") +
      ggtitle(paste0(name_v, " clusters; res - ", res_v))
  }
  
  ### TCR/BCR
  if (!"tbcr" %in% colnames(seurat_obj@meta.data)) {
    warning("tbcr column not found. Will not make plot.")
  } else {
    out_lsgg[["tbcr"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = "tbcr", cols = c("red", "grey", "blue"), 
                                  label = F, pt.size = 1) + coord_equal() +
      ggtitle(paste0(name_v, " clusters - TCR/BCR"))
  }
  
  ### Color by original populations
  if (!"ind_mPop" %in% colnames(seurat_obj@meta.data)) {
    warning("ind_mPop column not found. Will not make plot.")
  } else {
    out_lsgg[["origMpop"]] <- DimPlot(seurat_obj, reduction = reduction_v, group.by = "ind_mPop", label = F, pt.size = 1) +
      coord_equal() + ggtitle(paste0(name_v, " Embedding,\nindividual-batch Cell Classes"))
  }
  
  ### Output
  return(out_lsgg)
}