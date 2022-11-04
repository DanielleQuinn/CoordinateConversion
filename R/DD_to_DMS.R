#' DD_to_DMS
#'
#' @param DD_input FINISH
#' @param axis FINISH
#'
#' @return DMS
#'
#' @examples
#' library(sp)
#' DD_to_DMS(DD_input = 63.5, axis = "vertical")
#' 
#' @export
DD_to_DMS <-  function(DD_input, axis) {
  # Stop conditions
  if (length(DD_input) != 1)
    stop("DD_input must be of length 1")
  if (!is.numeric(DD_input))
    stop("DD_input must be numeric")
  if (!axis %in% c("horizontal", "vertical"))
    stop("axis must be one of horizontal or vertical")
  
  # Warning conditions
  if (abs(DD_input) > 90 & axis == "vertical") {
    warning("Vertical axis maximum is 90; NAs produced")
    return(NA)
  } else {
    if (abs(DD_input) > 180 & axis == "horizontal") {
      warning("Horizontal axis maximum is 180; NAs produced")
      return(NA)
    } else {
      return(paste0(
        floor(abs(DD_input)),
        intToUtf8(176),
        " ",
        floor((abs(DD_input) - floor(abs(DD_input))) * 60),
        "' ",
        round(((abs(DD_input) - floor(abs(DD_input))) * 60 - floor((abs(DD_input) - floor(abs(DD_input))) * 60)) * 60),
        '" ',
        case_when(
          axis == "horizontal" & DD_input < 0 ~ "W",
          axis == "horizontal" & !DD_input < 0 ~ "E",
          axis == "vertical" & DD_input < 0 ~ "S",
          axis == "vertical" & !DD_input < 0 ~ "N"
        )
      ))
    }
  }
}
