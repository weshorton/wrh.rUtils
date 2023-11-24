profitMarginCalculator <- function(price_v, margin_v = NULL, shippingWeight_v = 0, shop_v,
                                   includeShippingInMargin_v = F, method_v = 1, factor_v = 2) {
  #' Profit Margin Calculator
  #' @description
    #' Calculate appropriate price depending on desired margin and potential shipping costs.
  #' @param price_v numeric vector of the price of the product in dollars
  #' @param margin_v numeric value from 0.0 to 1.0 that is the profit margin in a decimal percentage (e.g. 15% == 0.15)
  #' @param shippingWeight_v numeric value of shipping weight in pounds.
  #' @param shop_v either "PlantBoy" or "PlantShop" indicating which supplier
  #' @param includeShippingInMargin_v logical to add shipping cost before calculating margin
  #' @param method_v either 1, meaning the original method, or 2, the 2x cost - shipping method.
  #' @param factor_v factor to multiply wholesale price by when determining margin if using method 2.
  #' @details
    #' Use supplied info to calculate the appropriate price for a product.
  #' @return numeric vector of suggested price
  #' @export
  
  ### Handle null
  if (is.null(shippingWeight_v)) print("Waiting on shipping weight.")
  #if (is.null(potSize_v)) print("Waiting on pot size.")
  
  ### Get shipping price
  shippingCost_v <- wrh.rUtils::shippingCalculator(weight_v = shippingWeight_v, shop_v = shop_v)
    
  ### Add to total cost
  if (includeShippingInMargin_v) {
    markup_v <- (price_v + shippingCost_v) * margin_v
    paste_v <- "Markup ((price + shipping) * margin): %s\n"
  } else {
    markup_v <- price_v * margin_v
    paste_v <- "Markup (price * margin): %s\n"
  } # fi
  
  markup_v <- round(markup_v, digits = 2)
  
  if (method_v == 1) {
    
    # Final price
    finalPrice_v <- round(price_v + shippingCost_v + markup_v, digits = 2)

  } else if (method_v == 2) {
    
    # Final price
    finalPrice_v <- round(factor_v*price_v + shippingCost_v, digits = 2)
    
    # Backcalc markup
    calculatedMarkup_v <- finalPrice_v - (price_v + shippingCost_v)
    calculatedMargin_v <- round(calculatedMarkup_v / finalPrice_v, digits = 2)
    
    # Change
    markup_v <- calculatedMarkup_v
    margin_v <- calculatedMargin_v
    
  } # fi
  
  #if (is.null(potSize_v)) potSize_v <- "none"
  
  # Update
  cat(sprintf(paste0("Wholesale price: %s\n
    Shipping Weight: %s\n
    Shipping Cost: %s\n",
    paste_v,
    "Sale Price (price + shipping + markup): %s\n
    Margin:%s\n"), price_v, shippingWeight_v, shippingCost_v, markup_v, finalPrice_v, margin_v))
  
} # profitMarginCalculator