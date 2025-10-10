findCommonCorrGenes <- function(corrData_lsdt, corrCol_v = "Corr", geneCol_v = "Gene", thresh_v = NULL,
                                option_v = "count") {
  #' Find Common Corr Genes
  #' @description Find genes common between corr tables
  #' @param corrData_lsdt list of correlation results tables from tcgaCorrelation()
  #' @param corrCol_v colname of tables that holds correlation values. Should always be 'Corr'
  #' @param geneCol_v colnames of tables that holds the gene names. Should always be 'Gene'
  #' @param thresh_v correlation threshold to use before finding intersection (abs value)
  #' @param option_v need a better name. Either 'count' or 'exact'
  #' @return vector of common genes
  #' @export
  
  ### Subset for genes above provided correlation level
  if (!is.null(thresh_v)) {
    corrData_lsdt <- lapply(corrData_lsdt, function(x) x[abs(get(corrCol_v)) >= thresh_v,])
    corrData_lsdt <- corrData_lsdt[names(which(sapply(corrData_lsdt, nrow) != 0))]
  } # fi
  
  if (option_v == "exact") {
    ### Get Genes
    genes_lsv <- lapply(corrData_lsdt, function(x) x[[geneCol_v]])
    
    ### Intersect
    common_v <- Reduce(intersect, genes_lsv)
    
    return(common_v)
    
  } else if (option_v == "count") {
    
    genes_dt <- dtListBind(corrData_lsdt)
    common_dt <- as.data.table(table(genes_dt[[geneCol_v]]))
    if (nrow(common_dt) > 0) setorder(common_dt, -N)
    
    ### Output
    return(common_dt)
    
  } else {
    stop(sprintf("Only 'count' and 'exact' supported for option_v. %s provided.\n"))
  }
  
  
} # findCommonCorrGenes