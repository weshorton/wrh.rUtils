### THIS COULD HAVE MORE FUNCTIONALITY

myTableGrob <- function(data_dt, title_v, fontsize_v = 14){
  #' Create custom table grob with title
  #' @description Creates table grob in format that is most common for my usage.
  #' @param data_dt Data.table that the grob will be made out of
  #' @param title_v Title for display
  #' @param fontsize_v Fontsize for title. Default is 14 (goes well with my_theme)
  #' @return gtable object
  #' @examples
  #' data_dt <- data.table("A" = LETTERS[1:5], "B" = 1:5)
  #' myGrob <- myTableGrob(data_dt, title_v = "Test")
  #' @export
  
  ## Table
  table_grob <- tableGrob(data_dt, rows = rep('', nrow(data_dt)), theme = ttheme_minimal())
  ## Title
  title_grob <- textGrob(title_v, gp = gpar(fontsize = fontsize_v))
  ## Add title
  table_grob <- gtable_add_rows(table_grob, heights = grobHeight(title_grob) + unit(5,'mm'), pos = 0)
  table_grob <- gtable_add_grob(table_grob, title_grob, 1, 1, 1, ncol(table_grob), clip = "off")
}