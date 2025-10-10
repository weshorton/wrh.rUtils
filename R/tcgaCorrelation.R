tcgaCorrelation <- function(data_dt, name_v = NULL, refGene_v = "IL27", corType_v = "pearson", sampleCol_v = "sample",
                            sigThresh_v = NULL, corrThresh_v = NULL) {
  #' TCGA Correlation
  #' @description
  #' Get correlation between ref gene and all other genes
  #' @param data_dt TCGA cancer data (see details)
  #' @param name_v optional name for cancer type
  #' @param refGene_v which gene to correlate others to
  #' @param corType_v type of correlation to run
  #' @param sampleCol_v column name that refers to unique rows. Should be sample
  #' @param sigThresh_v optional value to subset for significant results
  #' @param corrThresh_v optional value to subset for high correlations (abs value)
  #' @details
  #' data_dt has rows as samples and columns as gene expression and metadata
  #' meta columns are theoretically variable, but the LAST column should end in "time"
  #' and the sample column should be 'sample'
  #' @return tbd
  #' @export
  
  ### Get meta columns
  lastMetaCol_v <- grep("[Tt]ime", colnames(data_dt))
  lastMetaCol_v <- lastMetaCol_v[length(lastMetaCol_v)]
  metaCols_v <- colnames(data_dt)[1:lastMetaCol_v]
  
  ### Convert
  data_mat <- as.matrix(convertDFT(data_dt[,mget(c(sampleCol_v, setdiff(colnames(data_dt), metaCols_v)))], 
                                   col_v = sampleCol_v))
  
  ### Remove samples with missing ref gene
  sub_mat <- data_mat[!is.na(data_mat[,refGene_v]),]
  
  ### Calculate correlations
  corrRes_mat <- apply(sub_mat, 2, function(x) {
    ### Skip 'empty' gene expression
    if (sum(!is.na(x)) < 10 | sum(x, na.rm = T) == 0) return(c(cor = NA, p_value = NA))
    
    ### Test
    cor_test <- cor.test(sub_mat[,refGene_v], x, method = corType_v, exact = F)
    return(c(cor = cor_test$estimate, p_value = cor_test$p.value))
  })
  
  ### Reformat
  corrRes_dt <- data.table(
    Gene = colnames(sub_mat),
    Corr = corrRes_mat[1,],
    pVal = corrRes_mat[2,])
  
  ### Subset (no IL27 and no NAs)
  corrRes_dt <- corrRes_dt[Gene != refGene_v & !is.na(Corr),]
  
  ### Adjust p-value
  corrRes_dt[, pAdj := p.adjust(pVal, method = "BH")]
  
  ### Sort by absolute correlation
  corrRes_dt <- corrRes_dt[order(-abs(Corr))]
  
  ### Add name
  if (!is.null(name_v)) corrRes_dt$Cancer <- name_v
  
  ### Subset
  if (!is.null(sigThresh_v)) corrRes_dt <- corrRes_dt[pAdj <= sigThresh_v,]
  if (!is.null(corrThresh_v)) corrRes_dt <- corrRes_dt[abs(Corr) > corrThresh_v]
  
  return(corrRes_dt)
  
} # tcgaCorrelation