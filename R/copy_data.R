#' Copy data from R
#'
#' @param obj data of type \code{data.frame}, \code{zoo}, \code{xts} or \code{List}
#' @param row_names include row names - default FALSE
#' @param max_size specify max directory size - default 2^20 bytes
#'
#' @return nothing. data attached to clipboard
#' @export
#'
copy_data <- function(obj, row_names = FALSE, max_size = 2^20) UseMethod("copy_data")

#' Copy data from R - default
#'
#' @param obj data of type \code{data.frame}, \code{zoo}, \code{xts} or \code{List}
#' @param row_names include row names - default FALSE
#' @param max_size specify max directory size - default 2^20 bytes
#'
#' @export
#'
copy_data.default <- function(obj, row_names = FALSE, max_size = 2^20) {

  size <- utils::object.size(obj)

  try(if(size > max_size) stop("the object you're attempting to copy is larger than max_size."))

  clip <- paste0('clipboard-', size)
  f <- file(description = clip, open = 'w')
  utils::write.table(obj, f, row.names = row_names, sep = '\t')

  close(f)
}

#' Copy data from R - data.frame
#'
#' @param obj data of type \code{data.frame}, \code{zoo}, \code{xts} or \code{List}
#' @param row_names include row names - default FALSE
#' @param max_size specify max directory size - default 2^20 bytes
#'
#' @export
#'
copy_data.data.frame <- function(obj, row_names = FALSE, max_size = 2^20) {

  size <- utils::object.size(obj)

  try(if(size > max_size) stop("the object you're attempting to copy is larger than max_size."))

  clip <- paste0('clipboard-', size)
  f <- file(description = clip, open = 'w')
  col_names <- colnames(obj)

  utils::write.table(obj, f, row.names = row_names, col.names = col_names, sep = '\t')

  close(f)
}

#' Copy data from R - zoo/xts
#'
#' @param obj data of type \code{data.frame}, \code{zoo}, \code{xts} or \code{List}
#' @param row_names include row names - default FALSE
#' @param max_size specify max directory size - default 2^20 bytes
#'
#' @export
#'
copy_data.zoo <- function(obj, row_names = FALSE, max_size = 2^20) {

  size <- utils::object.size(obj)

  try(if(size > max_size) stop("the object you're attempting to copy is larger than max_size."))

  obj_df <- data.frame(date = zoo::index(obj), zoo::coredata(obj))
  colnames(obj_df) <- c("date", colnames(obj))

  copy_data.data.frame(obj_df, row_names)
}
