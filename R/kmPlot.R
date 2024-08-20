kmPlot <- function(fit, model, colors_v = c("blue", "red"), labels_v = c("Low" = "low", "High" = "up"), main_v = "KM Survival", 
                   xlab_v = "Overall Survival", ylab_v = "Prop. Survival", max_x, leg_y = c('med' = 1, 'rec' = 0.83, 'p' = 0.7),
                   fileName_v = NA) {
  #' Custom Kaplan Meier plot with median lines and legends
  #' @description Specific Kaplan Meier plot
  #' @param fit "survfit" object created by survfit() function
  #' @param model "survdiff" object created by survdiff() function
  #' @param colors_v vector of colors to plot. Must be same length as number of divisions in fit
  #' @param labels_v named vector of divisions in survival model. Names will be displayed on legend, values must match those of fit/model.
  #' @param main_v title for plot
  #' @param xlab_v label for X-axis. Default is "Overall Survival"
  #' @param ylab_v label for y-axis. Default is "Prop. Survival"
  #' @param max_x Max x-value from original data that created fit. Used for placing legends
  #' @param leg_y named vector of legend y-axis placements. Names are 'med', 'p', and 'rec'. Legends will only be printed if they have a leg_y value.
  #' @param fileName_v path to output file where plot will be printed. If NA, will print to stdout.
  #' @return plot to console of Kaplan Meier survival estimate
  #' @export
  
  if (!is.na(fileName_v)) pdf(file = fileName_v, width = 10, height = 10)
  ## Generate base plot
  plot(fit, col = colors_v, frame = F, lwd = 2,
       main = main_v, xlab = xlab_v, ylab = ylab_v,
       mark.time = T, cex.main = 2, cex.lab = 1.5, cex.axis = 1.2)
  
  ## Get medians and records
  medians_v <- summary(fit)$table[,'median']; names(medians_v) <- gsub("^.*=", "", names(medians_v))
  records_v <- summary(fit)$table[,'records']; names(records_v) <- gsub("^.*=", "", names(records_v))
  
  ## Add median lines
  lines(c(0, max(medians_v)), c(0.5, 0.5), lty = "dashed")
  mapply(function(x,y) lines(rep(x,2),c(0,0.5),col=y, lty="dashed"), medians_v, colors_v)
  
  ## Construct median and record legend
  if ('med' %in% names(leg_y) & 'rec' %in% names(leg_y)) {
    final_leg <- sapply(names(labels_v), function(x) {
      paste0(x, " = ", medians_v[[ labels_v[x] ]], " ; ", records_v[[ labels_v[x] ]])})
    legendTitle_v <- "Med. Surv. ; N. Records"
  } else if ('med' %in% names(leg_y) & !('rec' %in% names(leg_y))) {
    final_leg <- sapply(names(labels_v), function(x) paste(x, medians_v[[labels_v[x] ]], sep = " = "))
    legendTitle_v <- "Med. Surv."
  } else if (!('med' %in% names(leg_y)) & 'rec' %in% names(leg_y)) {
    final_leg <- sapply(names(labels_v), function(x) paste(x, records_v[[ labels_v[x] ]], sep = " = "))
    legendTitle_v <- "N. Records"
  }
  
  ## Make it
  if ('med' %in% names(leg_y) | 'rec' %in% names(leg_y)){
    legend("topright", legend = final_leg, col = colors_v, title = legendTitle_v, bty = "n", xjust = 1, cex = 1.2, lwd = 2)
  }
  # ## Add median legend
  # if ('med' %in% names(leg_y)) {
  #   med_leg <- sapply(names(labels_v), function(x) paste(x, medians_v[[labels_v[x] ]], sep = " = "))
  #   legend(max_x, leg_y['med'], legend = med_leg, col = colors_v, title = "Med. Surv.", bty = 'n', xjust = 1, cex = 1.2, lwd = 2)
  # } # fi
  # 
  # ## Add Record legend
  # if ('rec' %in% names(leg_y)) {
  #   rec_leg <- sapply(names(labels_v), function(x) paste(x, records_v[[ labels_v[x] ]], sep = " = "))
  #   legend(max_x, leg_y['rec'], legend = rec_leg, col = colors_v, title = "N. Records", bty = 'n', xjust = 1, cex = 1.2, lwd = 2)
  # } # fi
  
  ## Add pvalue legend
  if ('p' %in% names(leg_y)) {
    pval <- pchisq(model$chisq, df = 1, lower = F)
    pv_leg <- paste0("p.val = ", round(pval, digits = 4))
    legend("bottomleft", legend = pv_leg, bty = 'n', xjust = 1, cex = 1.2)
  }
  
  ## Close device
  if (!is.na(fileName_v)) graphics.off()
  
} # kmPlot

