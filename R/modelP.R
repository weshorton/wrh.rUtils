modelP <- function(model, var_v = NULL, round_v = NULL, verbose_v = F) {
  #' Extract p-value from model
  #' @description Extract the p-value estimate from either a linear model (lm) or ANOVA (aov) object.
  #' @param model model object of class 'lm' for linear model, classes 'lm' and 'aov' for ANOVA.
  #' @param var_v character string of variable to get p-value from. If NULL (default) will take 1st.
  #' @param round_v numeric. how many digits to round p-value output to. If NULL (default) will not round.
  #' @param round_v logical. TRUE - round p-value to 3 decimals; FALSE - do not round.
  #' @param verbose_v logical. TRUE - print model type; FALSE - don't print
  #' @return numeric vector of p-value
  #' @examples
  #' set.seed(1)
  #' x <- sample(1:100, size = 10)
  #' y <- sample(1:100, size = 10)
  #' my_lm <- lm(y ~ x)
  #' modelP(my_lm)
  #' @export
  
  ## Check class
  class_v <- class(model)
  if (length(class_v) == 2 & class_v[1] == "aov") {
    if (verbose_v) print("Supplied aov model.")
    class_v <- "aov"
  } else if (length(class_v) == 1 & class_v == "lm") {
    if (verbose_v) print("Supplied lm model.")
  } else {
    if (verbose_v) print("Supplied model is not an object of class 'aov' or 'lm'.")
  } # fi
  
  ## Take summary
  summary_v <- summary(model)
  
  ## Get p-value
  if (class_v == "aov") {
    summary_v <- summary_v[[1]]
    which_v <- ifelse(is.null(var_v), 1, which(trimws(rownames(summary_v)) == var_v))
    p_v <- summary_v[["Pr(>F)"]][[which_v]]
  } else {
    temp <- summary_v$fstatistic
    p_v <- pf(temp[1], temp[2], temp[3], lower.tail = F)
    attributes(p_v) <- NULL
  } # fi
  
  ## Round
  out_p_v <- ifelse(is.null(round_v), p_v, round(p_v, digits = round_v))
  
  ## If 0, make it < 2.2e-16
  if (!is.na(out_p_v)) {
    if (out_p_v == 0 & p_v > 0) {
      if (verbose_v) print("Rounded p-value is 0, so using unrounded p-value instead.")
      out_p_v <- p_v
    } # fi
  } # fi
  
  ## Output
  return(out_p_v)
} # modelP
