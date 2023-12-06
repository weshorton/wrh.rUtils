# fdSubchunkify <- function(g, fig_height=7, fig_width=5, id = NULL, title_v = "Title") {
#   #' FD Subchunkify
#   #' @description Output a flexdashboard plotting chunk
#   #' Code taken from stack overflow and adapted.
#   #' https://stackoverflow.com/questions/15365829/dynamic-height-and-width-for-knitr-plots/47339394#47339394
#   #' References a blog post by Michael J Williams, but link is broken
#   #' Addition of ID from this post: https://stackoverflow.com/questions/61620768/rmarkdown-explicitly-specify-the-figure-size-of-a-plot-within-a-chunk
#   #' Had to modify the paste0() call for sub_chunk because r markdown was giving warnings about mismatched ticks for some reason.
#   #' Function to output variable figure sizes
#   #' @param g chunk
#   #' @param fig_height height of chunk
#   #' @param fig_width width of chunk
#   #' @param id optional chunk ID
#   #' @param title_v optional title
#   #' @return flexdashboard chunk
#   #' @export
#   
#   
#   ### Make function to de-parse plot
#   # g_deparsed <- paste0(deparse(
#   #   function() {g}
#   # ), collapse = '')
#   
#   ### Make chunk id be title if not provided
#   if (is.null(id)) id <- gsub(" .*$", "", title_v)
#   
#   ### Build Tab Title
#   one_v <- paste0('
#                   #', '## ', title_v, '
#                   ')
#   
#   ### Construct code chunk header
#   two_v <- paste0('
#                   `', '``{r sub_chunk_', id, '_', floor(runif(1) * 10000000),
#                   ', fig.height=', fig_height, ', fig.width=', fig_width, ', echo = F}\n
#                   ')
#   
#   ### Insert plot
#   three_v <- paste0('
#                     \n\n (',
#                     g_deparsed
#                     , ")()")
#   
#   ### Close code chunk
#   four_v <- paste0('
#                    \n`', '``
#                    ')
#   
#   ### Combine
#   out_v <- paste(trimws(one_v), trimws(two_v), trimws(three_v), trimws(four_v), sep = "\n")
#   
#   ### Output chunk
#   cat('\n\n')
#   cat(trimws(out_v))
#   cat('\n\n')
#   
# } # subchunkify



# Hold for now
# fdSubchunkify <- function(g, fig_height=7, fig_width=5, id = NULL, title_v = "Title") {
#   #' FD Subchunkify
#   #' @description Output a flexdashboard plotting chunk
#   #' Code taken from stack overflow and adapted.
#   #' https://stackoverflow.com/questions/15365829/dynamic-height-and-width-for-knitr-plots/47339394#47339394
#   #' References a blog post by Michael J Williams, but link is broken
#   #' Addition of ID from this post: https://stackoverflow.com/questions/61620768/rmarkdown-explicitly-specify-the-figure-size-of-a-plot-within-a-chunk
#   #' Had to modify the paste0() call for sub_chunk because r markdown was giving warnings about mismatched ticks for some reason.
#   #' Function to output variable figure sizes
#   #' @param g chunk
#   #' @param fig_height height of chunk
#   #' @param fig_width width of chunk
#   #' @param optional chunk ID
#   #' @export
#   #'
#   
#   ### Make function to de-parse plot
#   # g_deparsed <- paste0(deparse(
#   #   function() {g}
#   # ), collapse = '')
#   g_deparsed <- paste0(deparse(
#     invisible(print(g))
#   ), collapse = '')
#   
#   ### Make chunk id be title if not provided
#   if (is.null(id)) id <- gsub(" .*$", "", title_v)
#   
#   ### Can look into this further, not sure why the ``` need to be separated like that, for example.
# sub_chunk <- paste0('
#                     #', '## ', title_v,                                                          # Row/Column Header
#                     '
#                     `', '``{r sub_chunk_', id, '_', floor(runif(1) * 10000000),              # Code chunk ID
#                     ', fig.height=', fig_height, ', fig.width=', fig_width, ', echo = F}
#                     ',      # Height/width parameters
#                     '
#                     (',                                                                      # begin plot fxn
#                     g_deparsed,                                                                 # deparsed plot
#                     ")()",                                                                      # end plot fxn
#                     "
#                     `", '``
#                     ')                                                              # close code chunk
#   
#   one_v <- paste0('
#                   #', '## ', title_v, '
#                   ')
#   
#   two_v <- paste0('
#                   `', '``{r sub_chunk_', id, '_', floor(runif(1) * 10000000),
#                   ', fig.height=', fig_height, ', fig.width=', fig_width, ', echo = F}\n
#                   ')
#   
#   three_v <- paste0('
#                     \n\n (',
#                     g_deparsed
#                     , ")()")
#   
#   four_v <- paste0('
#                    \n`', '``
#                    ')
#   
#   out_v <- paste(trimws(one_v), trimws(two_v), trimws(three_v), trimws(four_v), sep = "\n")
#   
#   ### Output chunk
#   cat('\n\n')
#   # cat(trimws(sub_chunk))
#   cat(trimws(out_v))
#   #cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
#   cat('\n\n')
#   
# } # subchunkify
