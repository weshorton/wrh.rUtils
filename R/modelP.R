modelP <- function(model, round_v = T, verbose_v = F) {
  #' Extract p-value from model
  #' @description Extract the p-value estimate from either a linear model (lm) or ANOVA (aov) object.
  #' @param model model object of class 'lm' for linear model, classes 'lm' and 'aov' for ANOVA.
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
    p_v <- model[[1]][["Pr(>F)"]][[1]]
  } else {
    temp <- summary_v$fstatistic
    p_v <- pf(temp[1], temp[2], temp[3], lower.tail = F)
    attributes(p_v) <- NULL
  } # fi
  
  ## Round
  p_v <- ifelse(round_v, round(p_v, digits = 3), p_v)
  
  ## If 0, make it < 2.2e-16
  p_v <- ifelse(p_v == 0, p_v <- "< 2.2e-16", p_v)
  
  ## Output
  return(p_v)
} # modelP
