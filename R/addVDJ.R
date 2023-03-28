addVDJ <- function(seurat_obj, type_v, dir_v) {
  #' Add 10x VDJ Data to Seurat Obj
  #' @description Add 10x VDJ T or B data to an initialized GEX Seurat object. Modified from function found:
  #' https://ucdavis-bioinformatics-training.github.io/2020-Advanced_Single_Cell_RNA_Seq/data_analysis/VDJ_Analysis_fixed
  #' @param seurat_obj gene expression seurat object
  #' @param type_v either "t" for TCR or "b" for BCR
  #' @param dir_v directory where "filtered_contig_annotations.csv" and "clonotypes.csv" are found.
  #' @return Exact same seurat object with new metadata columns with clonotype information: 
  #' [type_v]_clonotype_id and [type_v]_cdr3s_aa
  #' @export
  
  ### Load files
  whichDir_v <- ifelse(type_v == "t", "vdj_t", "vdj_b")
  contigs_dt <- fread(file.path(dir_v, whichDir_v, "filtered_contig_annotations.csv"))
  clonotypes_dt <- fread(file.path(dir_v, whichDir_v, "clonotypes.csv"))
  
  ### Subset to get one row for each barcode and just retain clonotype id
  ### So now we have all the cells that had a clonotype and what the id of that clonotype is
  ### Can have the same clonotype id in multiple rows if multiple cells had it
  contigs_dt <- contigs_dt[!duplicated(contigs_dt$barcode), mget(c("barcode", "raw_clonotype_id"))]
  colnames(contigs_dt)[colnames(contigs_dt) == "raw_clonotype_id"] <- "clonotype_id"
  
  ### Merge amino acid sequences from clonotypes table
  contigs_dt <- merge(contigs_dt, clonotypes_dt[,mget(c("clonotype_id", "cdr3s_aa"))],
                      by = "clonotype_id", sort = F)
  
  ### Reformat with barcodes as rownames for integration with Seurat
  contigs_df <- convertDFT(contigs_dt, col_v = "barcode", rmCol_v = T)
  
  ### Add type designator to column names
  colnames(contigs_df) <- paste(type_v, colnames(contigs_df), sep = "_")
  
  ### Add to object
  seurat_obj <- AddMetaData(object = seurat_obj, metadata = contigs_df)
  
  ### Return
  return(seurat_obj)
} # addVDJ