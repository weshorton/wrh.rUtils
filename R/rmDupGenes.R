rmDupGenes <- function(data_dt, idCol_v = "ID", symbolCol_v = "Symbol", method_v = "max") {
  #' Remove Duplicate Genes
  #' @description Can't make data.frames with duplicate rownames. Have to remove duplicates.
  #' @param data_dt data.table of gene expression.
  #' @param idCol_v don't think this is needed actually
  #' @param symbolCol_v Name of column that contains gene identifiers
  #' @param method_v One of "max" (default) or "mean". Max means that the gene with max count will be taken, "mean" means will average them all.
  #' @export
  
  ## Get genes
  genes_v <- data_dt[[symbolCol_v]]
  
  ## Get their counts
  geneCounts_dt <- as.data.table(table(genes_v))
  
  ## Get which have > 1 observation
  dupGenes_dt <- geneCounts_dt[N > 1,]
  
  ## Empty data.table for rows to keep
  keepRows_dt <- NULL
  
  ## Get count columns
  countCol_v <- setdiff(colnames(data_dt), c(idCol_v, symbolCol_v))
  
  ## Filter
  for (i in 1:dupGenes_dt[,.N]) {
    
    ## Get gene and subset
    currGene_v <- dupGenes_dt[i, genes_v]
    currData_dt <- data_dt[get(symbolCol_v) == currGene_v,]
    
    ## Make output row
    if (method_v == "max") {
      currMean_v <- rowMeans(currData_dt[,mget(countCol_v)])
      currMax_v <- which.max(currMean_v)
      keepRows_dt <- rbind(keepRows_dt, currData_dt[currMax_v,])
    } else if (method_v == "mean") {
      currMean_dt <- currData_dt[, lapply(.SD, mean, na.rm = T), by = "Symbol", .SDcols = countCol_v]
      keepRows_dt <- rbind(keepRows_dt, currMean_dt)
    } # fi
    
    ## Remove duplicate rows
    rmIdx_v <- which(data_dt[[symbolCol_v]] == currGene_v)
    data_dt <- data_dt[-rmIdx_v,]
    
  } # for i
  
  ## Add back chosen columns
  data_dt <- rbind(data_dt, keepRows_dt)
  
  ## Return
  return(data_dt)
}