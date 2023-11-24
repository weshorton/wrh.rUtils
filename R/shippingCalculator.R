shippingCalculator <- function(weight_v = 0, shop_v) {
  #' Calculate Shipping
  #' @description
    #' Calculate shipping cost based on weight for plantboy
  #' @param weight_v numeric value of product weight in pounds. Can also be a character vector of pot size.
  #' @param shop_v character vector. either "PlantBoy" or "PlantShop"
  #' @return vector of price
  #' @export
  
  
  ### Conversion table
  plantBoyTable_dt <- data.table("Weight" = c(0.5, 1, 1.5, 3, 5, 8, 12, 16, 20, 40, 50, 60, 70, 80, 100),
                                 "Cost" = c(5, 7, 9, 12, 15, 20, 25, 35, 41, 55, 98, 130, 150, 178, 210))
  
  plantShopTable_dt <- data.table("Weight" = c(0.75, 1, 1.51, 3, 5, 8, 12, 16, 20, 40, 50, 60, 70, 80, 100),
                                  "Cost" = c(7.5, 9, 11, 15, 19, 24, 29, 35, 41, 55, 98, 130, 150, 178, 210))
  
  potWeightTable_dt <- data.table("Size" = paste0(c(2, 3, 4, 5, 6, 8, 10, 12), "in"),
                                   "Weight" = c(0.75, 0.75, 1, 3, 5, 8, 12, 16))
  
  ### Get which table
  if (shop_v %in% c("pb", "PB", "Pb", "plantboy", "Plantboy", "PlantBoy", "plantBoy")) {
    shop_v <- "pb"
    table_dt <- plantBoyTable_dt
  } else if (shop_v %in% c("ps", "PS", "Ps", "plantshop", "Plantshop", "PlantShop", "plantShop")) {
    table_dt <- plantShopTable_dt
    shop_v <- "ps"
  } # fi shop_v
  
  ### Handle pot size weights
  if (grepl("in", weight_v)) {
    
    ### Warn
    if (shop_v == "pb") warning("Have only verified character weight functionality for PlantShop, you have PlantBoy selected!\n")
    
    ### Map tables
    map_dt <- merge(table_dt, potWeightTable_dt, by = "Weight", sort = F)
    
    ### Get cost
    cost_v <- map_dt[Size == weight_v, Cost]
    
  } else {

    ### Only calculate if weight is not 0
    if (weight_v == 0) {
      
      cost_v <- 0
      
    } else {
      
      ### Get index of product
      if (shop_v == "pb") {
        index_v <- min(which(table_dt$Weight >= weight_v))
      } else if (shop_v == "ps") {
        index_v <- min(which(table_dt$Weight >= weight_v))
      } # fi shop_v
      
      #index_v <- max(which(table_dt$Weight < weight_v))
      
      ### Get shipping cost
      cost_v <- table_dt[index_v, Cost]
      
    } # fi weight == 0
    
  } # fi is.null

  ### Output
  return(cost_v)
  
} # shipping Calculator
