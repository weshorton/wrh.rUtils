convertMouseHumanGenes <- function(genes_v, species_v = "mmu", host_v = "https://dec2021.archive.ensembl.org/") {
  #' Convert Gene Names
  #' @description Convert gene names between mouse and human using ensembl database (adapted from Rossin Erbe)
  #' @param genes_v vector of genes to convert
  #' @param species_v species identity to convert FROM. If "mouse" or "mmu", will be converted to human.
  #' If "human" or "hg", will be converted to mouse.
  #' @description TO DO - add either verbose option or multiple output option to also output the genes that didn't get converted.
  #' original host: "https://dec2021.archive.ensembl.org/" this worked at first, but keep on getting connection time outs. The biomart
  #' functions should check other mirrors, but can also try "https://www.ensembl.org".
  #' @return genes_v with updated names
  #' @export
  
  ### Load ensemble marts
  if ("humanMart" %in% ls(envir = .GlobalEnv)) {
    get("humanMart", envir = .GlobalEnv)
  } else {
    humanMart = biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl", host = host_v)
    assign("humanMart", humanMart, envir = .GlobalEnv)
  } # fi
  
  if ("mouseMart" %in% ls(envir = .GlobalEnv)) {
    get("mouseMart", envir = .GlobalEnv)
  } else {
    mouseMart = biomaRt::useMart("ensembl", dataset = "mmusculus_gene_ensembl", host = host_v)
    assign("mouseMart", mouseMart, envir = .GlobalEnv)
  }
  
  ### Map genes
  if (species_v %in% c("mmu", "mouse")) {
    
    genes_df <- biomaRt::getLDS(attributes = "mgi_symbol", filters = "", values = genes_v,
                       mart = mouseMart, attributesL = "hgnc_symbol", martL = humanMart, uniqueRows = T)
    
    ref_v <- "MGI.symbol"
    target_v <- "HGNC.symbol"
    
  } else if (species_v %in% c("hg", "human")) {
    
    genes_df <- biomaRt::getLDS(attributes = "hgnc_symbol", filters = "", values = genes_v,
                       mart = humanMart, attributesL = "mgi_symbol", martL = mouseMart, uniqueRows = T)
    
    ref_v <- "HGNC.symbol"
    target_v <- "MGI.symbol"
    
  } else {
    
    stop(sprintf("species_v must be one of 'mmu', 'mouse', 'hg', or 'human'. '%s' provided.\n", species_v))
    
  } # fi
  
  ### Empty vectors to hold outputs
  out_v <- vector("character", length(genes_v))
  #missing_v <- NULL
  
  ### For each gene, change name
  for (i in seq_along(genes_v)) {
    
    which_v <- which(genes_df[[ref_v]] == genes_v[i])
    
    if (length(which_v) > 0) {
      
      out_v[i] <- genes_df[[target_v]][which_v[1]]
      
    } # fi
    
  } # for i
  
  ### Output
  return(out_v)
  
} # convertMouseHumanGenes

