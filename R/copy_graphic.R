#' copy graphics generated in R to clipboard
#'
#' @param graphic graphic generated in R
#' @param size string indicating the size (sar, wide or custom) of the new graphics device to copy to. defaults to standard aspect ratio
#' @param ... additional arguments to pass to the graphics device such as width, height and mar
#'
#' @return nothing. graphic is attached to clipboard
#' @export
#'
copy_graphic <- function(graphic, size = c("sar", "wide", "custom"), ...) {

  size <- match.arg(size) # if aggregate_by is not specified, default to the first option

  switch(size,
         sar = grDevices::dev.new(width = 1024, height = 768, units ="px", noRStudioGD = TRUE), # standard aspect ratio (4:3)
         wide = grDevices::dev.new(width = 1920, height = 1080, units ="px", noRStudioGD = TRUE), # wide screen (16:9)
         custom = grDevices::dev.new(..., noRStudioGD = TRUE) # user specified width and height. other arguments such as margin may also be specified
  )

  print(graphic) # draw graphic on this new active window

  grDevices::savePlot("clipboard", type = "wmf") # save to clipboard as type windows meta file

  grDevices::dev.off()
}
