library(ggplot2)

###
### Standard ggplot theme #######################################################################################################
###

my_theme <- function() {
  #' Base Custom Theme
  #' @description Minor changes to theme_classic() - slightly larger text and centered title
  #' @export
  
  theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
}

###
### Big Label  ggplot theme #####################################################################################################
###

big_label <- function() {
  #' Big Label Theme
  #' @description Same as my_theme(), but even larger text and also y-axis labels are angled 45
  #' @export
  
  theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5, size = 16),
          axis.text = element_text(size = 16),
          #axis.text.y = element_text(angle = 45),
          axis.title = element_text(size = 18),
	  strip.text = element_text(size = 18),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18))
}

###
### Massive Label  ggplot theme #####################################################################################################
###

massive_label <- function(multiplier_v = 1, angleX_v = T, angleY_v = F) {
  #' Massive Label Theme
  #' @description Same as big_label() theme, but even LARGER text and also y-axis labels are angled 45
  #' @param multiplier_v numeric. multiple text sizes by this factor. 
  #' @export
  
  xAngle_v <- ifelse(angleX_v, 45, 0)
  xJust_v <- ifelse(angleX_v, 1, 0)
  
  yAngle_v <- ifelse(angleY_v, 45, 0)
  yJust_v <- ifelse(angleY_v, 1, 0)
  
  
  theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 40*multiplier_v),
          plot.subtitle = element_text(hjust = 0.5, size = 26*multiplier_v),
          legend.title = element_text(size = 28*multiplier_v),
          axis.title = element_text(size = 32*multiplier_v),
          
          axis.text.x = element_text(size = 28*multiplier_v, angle = xAngle_v, hjust = xJust_v),
          strip.text = element_text(size = 32*multiplier_v),
          axis.text.y = element_text(size = 24*multiplier_v, angle = yAngle_v),
          legend.text = element_text(size = 22*multiplier_v),
          
          axis.ticks = element_line(linewidth=1.5),
          axis.ticks.length = unit(15, "points"),
          axis.line = element_line(linewidth = 1.5),
          strip.background = element_rect(linewidth = 2.5))
}

###
### Angle Text theme ############################################################################################################
###

angle_x <- function() {
  #' Angle X Theme
  #' @description Just add 45 degree adjustment to X axis
  #' @export
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

angle_y <- function() {
  #' Angle Y Theme
  #' @description Just add 45 degree adjustment to Y axis
  #' @export
  
  theme(axis.text.y = element_text(angle = 45))
}

angle_both <- function() {
  #' Angle Both Theme
  #' @description Add 45 degree adjustment to both axes
  #' @export
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45))
}

###
### Times New Roman #############################################################################################################
###

times <- function() {
  #' Times New Roman Theme
  #' @description Same as my_theme(), but font is all Times New Roman
  #' @export
  
  my_theme +
    theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5, size = 18),
          plot.subtitle = element_text(family = "Times New Roman", hjust = 0.5, size = 14),
          axis.text = element_text(family = "Times New Roman", size = 12),
          axis.title = element_text(family = "Times New Roman", size = 14),
          legend.text = element_text(family = "Times New Roman", size = 12),
          legend.title = element_text(family = "Times New Roman", size = 12),
          strip.text = element_text(family = "Times New Roman", size = 14))
}

###
### Big Label Times New Roman ###################################################################################################
###

bl_times <- function() {
  #' Big Label Times
  #' @description Same as big_label(), but font is all Times New Roman
  #' @export
  
  my_theme +
    theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5, size = 20),
          plot.subtitle = element_text(family = "Times New Roman", hjust = 0.5, size = 16),
          axis.text = element_text(family = "Times New Roman", size = 16),
          axis.title = element_text(family = "Times New Roman", size = 18),
          legend.text = element_text(family = "Times New Roman", size = 16),
          legend.title = element_text(family = "Times New Roman", size = 18),
          strip.text = element_text(family = "Times New Roman", size = 18))
}

###
### Black ggplot theme #######################################################################################################
###

my_black <- function() {
  #' Base Custom Theme
  #' @description Minor changes to theme_classic() - slightly larger text and centered title. Along with black bkgd, white text.
  #' @export
  
  theme_classic() +
    theme(
      ## Text
      plot.title = element_text(hjust = 0.5, size = 18, color = "white"),
      plot.subtitle = element_text(hjust = 0.5, size = 14, color = "white"),
      axis.text = element_text(size = 12, color = "white"),
      axis.title = element_text(size = 14, color = "white"),
      legend.text = element_text(size = 12), color = "white",
      legend.title = element_text(size = 14, color = "white"),
      ## Axis
      axis.ticks = element_line(color = "white"),
      ## Legend
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white", fill = "grey30"),
      ## Panel
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(fill = NA, color = "white"),
      ## Facet
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text = element_text(size = 12, color = "white"),
      ## Background
      plot.background = element_rect(color = "black", fill = "black")
      )
}

###
### Big Label - Black ggplot theme #####################################################################################################
###

bl_black <- function() {
  #' Big Label Theme
  #' @description Same as my_theme(), but even larger text and also y-axis labels are angled 45
  #' @export
  
  theme_classic() +
    theme(
      ## Text
      plot.title = element_text(hjust = 0.5, size = 20, color = "white"),
      plot.subtitle = element_text(hjust = 0.5, size = 16, color = "white"),
      axis.text = element_text(size = 16, color = "white"),
      axis.text.y = element_text(angle = 45, color = "white"),
      axis.title = element_text(size = 18, color = "white"),
      legend.text = element_text(size = 16, color = "white"),
      legend.title = element_text(size = 18, color = "white"),
      ## Axis
      axis.ticks = element_line(color = "white"),
      ## Legend
      legend.background = element_rect(color = NA, fill = "black"),
      legend.key = element_rect(color = "white", fill = "grey30"),
      ## Panel
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(fill = NA, color = "white"),
      ## Facet
      strip.background = element_rect(fill = "grey30", color = "grey10"),
      strip.text = element_text(size = 16, color = "white"),
      ## Background
      plot.background = element_rect(color = "black", fill = "black"))
}
