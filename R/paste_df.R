#' Paste object from clipboard
#'
#' Returns data on clipboard as a \code{data.frame}.
#'
#' @return \code{data.frame} of the data on the clipboard.
#'
#' @export
#'
paste_df <- function() {

  f <- file(description = 'clipboard', open = 'r')
  on.exit(close(f))
  df <- utils::read.table(f, sep = '\t', header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

  return(df)
}
