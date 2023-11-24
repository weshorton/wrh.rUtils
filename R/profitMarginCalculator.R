profitMarginCalculator <- function(price_v, margin_v = NULL, shippingWeight_v = 0, shop_v,
                                   includeShippingInMargin_v = F) {
  #' Profit Margin Calculator
  #' @description
    #' Calculate appropriate price depending on desired margin and potential shipping costs.
  #' @param price_v numeric vector of the price of the product in dollars
  #' @param margin_v numeric value from 0.0 to 1.0 that is the profit margin in a decimal percentage (e.g. 15% == 0.15)
  #' @param shippingWeight_v numeric value of shipping weight in pounds.
  #' @param shop_v either "PlantBoy" or "PlantShop" indicating which supplier
  #' @param includeShippingInMargin_v logical to add shipping cost before calculating margin
  #' @details
    #' Use supplied info to calculate the appropriate price for a product.
  #' @return numeric vector of suggested price
  #' @export
  
  ### Get shipping price
  shippingCost_v <- shippingCalculator(weight_v = shippingWeight_v, shop_v = shop_v)
    
  ### Add to total cost
  if (includeShippingInMargin_v) {
    markup_v <- (price_v + shippingCost_v) * margin_v
  } else {
    markup_v <- price_v * margin_v
  } # fi
  
  # Final price
  finalPrice_v <- round(price_v + shippingCost_v + markup_v, digits = 2)
  
  # Update
  cat(sprintf("Item weight: %s\n
              Shipping Cost: %s\n
              Pre-markup price: %s\n
              Price+shipping: %s\n
              Markup: %s\n
              Final Price: %s\n",
              shippingWeight_v, shippingCost_v, price_v, (price_v+shippingCost_v), markup_v, finalPrice_v))
  
} # profitMarginCalculator