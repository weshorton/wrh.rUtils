profitMarginCalculator <- function(price_v, margin_v = NULL, shipping_v = NULL) {
  #' Profit Margin Calculator
  #' @description
    #' Calculate appropriate price depending on desired margin and potential shipping costs.
  #' @param price_v numeric vector of the price of the product in dollars
  #' @param margin_v numeric value from 0.0 to 1.0 that is the profit margin in a decimal percentage (e.g. 15% == 0.15)
  #' @param shipping_v numeric value of shipping cost in dollars.
  #' @details
    #' Use supplied info to calculate the appropriate price for a product.
  #' @return numeric vector of suggested price
  #' @export
  
  # Add shipping cost to price
  if (!is.null(shipping_v)) {
    price_v <- price_v + shipping_v
  } # fi
  
  # Calculate markup
  markup_v <- price_v * margin_v
  
  # Final price
  finalPrice_v <- round(price_v + markup_v, digits = 2)
  
  # Output
  return(finalPrice_v)
  
} # profitMarginCalculator