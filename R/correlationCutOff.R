correlationCutOff <- function(data_dt, name_v = NULL, corrCol_v = "Corr", cutType_v = "kneedle", corrType_v = "Pearson") {
  #' Correlation Cut Off
  #' @description
    #' Find appropriate rank and value for cut-off of correlation distribution
  #' @param data_dt data.table of correlations. Rows = genes; columns = corr
  #' @param name_v optional vector to be prepended to title.
  #' @param corrCol_v column name that holds correlation values. Default is "Corr"
  #' @param cutType_v vector indicating which method to use. kneedle (default), "arrow", or "both"
  #' @param corrType_v should always be 'Pearson', adding as an argument in case you want to run this on different corr results (just used for plot titles)
  #' @description
    #' Use "elbow" methods to find a non-arbitrary cut-off for a set of correlation values
  #' @return list of 3 plots and data.tables with results
  #' @export
  
  ###
  ### Prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  ### Split into positive and negative
  data_lsdt <- list("all" = data_dt, "pos" = data_dt[get(corrCol_v) > 0,], "neg" = data_dt[get(corrCol_v) < 0,])
  
  ### Add ranks
  data_lsdt <- sapply(data_lsdt, function(x) {
    x$Rank <- 1:nrow(x)
    return(x)}, simplify = F, USE.NAMES = T)
  
  ###
  ### Cut Offs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  res_lsdt <- list()
  
  ### Kneedle Method
  if (cutType_v %in% c("kneedle", "both")) {
    
    kneedle_lsdt <- sapply(data_lsdt, function(x) {
      y_v <- kneedle::kneedle(x = x$Rank, y = x[[corrCol_v]])
      y_dt <- data.table(x = y_v[1], y = y_v[2])
      return(y_dt)}, simplify = F, USE.NAMES = T)
    kneedle_dt <- wrh.rUtils::dtListBind(kneedle_lsdt, newName_v = "corrSign")
    kneedle_dt$Method <- "kneedle"
    res_lsdt[["kneedle"]] <- kneedle_dt
    
  } # fi kneedle or both
  
  if (cutType_v %in% c("arrow", "both")) {
    
    arrow_lsdt <- sapply(data_lsdt, function(x) {
      y_dt <- wrh.rUtils::listToDT(KneeArrower::findCutoff(x = x$Rank, y = x[[corrCol_v]]))
      return(y_dt)}, simplify = F, USE.NAMES = T)
    arrow_dt <- wrh.rUtils::dtListBind(arrow_lsdt, newName_v = "corrSign")
    arrow_dt$Method <- "arrow"
    res_lsdt[["arrow"]] <- arrow_dt
    
  } # fi arrow or both
  
  ### Handle res output
  if (length(res_lsdt) == 2) {
    res_dt <- do.call(rbind, res_lsdt)
  } else {
    res_dt <- res_lsdt[[1]]
  } # fi length
  
  ###
  ### Base Plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###
  
  out_ls <- list()
  
  for (i in 1:length(data_lsdt)) {
    
    ### Get sign
    currCorrSign_v <- names(data_lsdt)[i]
    
    ### Get appropriate data
    if (currCorrSign_v != "all") {
      currData_dt <- data_lsdt[[currCorrSign_v]]
    } else {
      currData_dt <- data_lsdt$pos
      currOther_dt <- data_lsdt$neg
    } # fi
    
    ### Base Plot
    curr_gg <- ggplot(currData_dt, aes(x = Rank, y = !!sym(corrCol_v))) +
      geom_point() + my_theme()
    
    ### Handle 'all'
    if (currCorrSign_v == "all") {
      curr_gg <- curr_gg + geom_point(data = currOther_dt, aes(x = Rank, y = !!sym(corrCol_v)), inherit.aes = F)
    } # fi
    
    ### Add title
    title_v <- paste(currCorrSign_v, corrType_v, "Corr", sep = " ")
    if (!is.null(name_v)) title_v <- paste(name_v, title_v, sep = " ")
    curr_gg <- curr_gg + ggtitle(title_v)
    
    ###
    ### Points and Labels ~~~~~~~~~~~~~~~~~~~~~~~
    ###
    
    subTitle_lsv <- list()
    
    ### Add for positive/all and kneedle/arrow/both
    if (currCorrSign_v %in% c("pos", "all")) {
      
      if (cutType_v %in% c("kneedle", "both")) {
        tmpRes_dt <- res_dt[corrSign == "pos" & Method == "kneedle",]
        curr_gg <- curr_gg + geom_point(data = tmpRes_dt, aes(x = x, y = y), color = "red", size = 3, inherit.aes = F)
        # curr_gg <- curr_gg + labs(subtitle = paste0("Kneedle - posRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3)))
        subTitle_lsv[["kneedlePos"]] <- paste0("Kneedle - posRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3))
      } # fi cutType_v kneedle
      
      if (cutType_v %in% c("arrow", "both")) {
        tmpRes_dt <- res_dt[corrSign == "pos" & Method == "arrow",]
        curr_gg <- curr_gg + geom_point(data = tmpRes_dt, aes(x = x, y = y), color = "blue", size = 3, inherit.aes = F)
        #curr_gg <- curr_gg + labs(subtitle = paste0("Arrow - posRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3)))
        subTitle_lsv[["arrowPos"]] <- paste0("Arrow - posRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3))
      } # fi cutType_v arrow
      
    } # fi pos/all
    
    ### Add for neg/all and kneedle/arrow/both
    if (currCorrSign_v %in% c("neg", "all")) {
      
      if (cutType_v %in% c("kneedle", "both")) {
        tmpRes_dt <- res_dt[corrSign == "neg" & Method == "kneedle",]
        curr_gg <- curr_gg + geom_point(data = tmpRes_dt, aes(x = x, y = y), color = "red", size = 3, inherit.aes = F)
        #curr_gg <- curr_gg + labs(subtitle = paste0("Kneedle - negRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3)))
        subTitle_lsv[["kneedleNeg"]] <- paste0("Kneedle - negRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3))
      } # fi cutType_v kneedle
      
      if (cutType_v %in% c("arrow", "both")) {
        tmpRes_dt <- res_dt[corrSign == "neg" & Method == "arrow",]
        curr_gg <- curr_gg + geom_point(data = tmpRes_dt, aes(x = x, y = y), color = "blue", size = 3, inherit.aes = F)
        #curr_gg <- curr_gg + labs(subtitle = paste0("Arrow - negRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3)))
        subTitle_lsv[["arrowNeg"]] <- paste0("Arrow - negRank: ", floor(tmpRes_dt$x), "; corr: ", round(tmpRes_dt$y, digits = 3))
      } # fi cutType_v arrow
      
    } # fi neg/all
    
    ### Add final subtitle
    curr_gg <- curr_gg + labs(subtitle = paste0(unlist(subTitle_lsv), collapse = "\n"))
    
    ### Add to list
    out_ls[[currCorrSign_v]] <- curr_gg
    
  } # for i
  
  ### Final Output
  out_ls$results <- res_dt
  return(out_ls)
  
} # correlationCutOff