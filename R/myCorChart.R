myCorChart <- function (R, histogram = TRUE, method = c("pearson", "kendall", 
                                                        "spearman"), pch = 20, main = "Correlation") 
{
  #' My Correlation Chart
  #' @description
  #' Copy of PerformanceAnalytics::chart.Correlation (v2.0.4) with a few adjustments
  #' @param R data for the x axis, can take matrix, vector, or timeseries
  #' @param histogram TRUE/FALSE whether or not to display a histogram
  #' @param method a character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman", can be abbreviated.
  #' @param pch plotting symbol to use. see 'pch values' under ?pch
  #' @param main optional title for plot
  #' @details
  #' PerformanceAnalytics::chart.Correlation() has a '...' argument to pass arguments to pairs(), but it doesn't work.
  #' Needed to change the point type and add a title so copied the function and added that. Everything else is the same.
  #' @export
  
  x = PerformanceAnalytics::checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  cormeth <- method
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = cormeth, cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 1e-03, 1e-02, 5e-02, 0.1, 1), symbols = c("***", 
                                                                                "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel, pch = pch, main = main)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, pch = pch, main = main)
}
