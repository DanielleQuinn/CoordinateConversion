#' DD_to_DDM
#'
#' @param DD_input
#' @param axis
#'
#' @return DDM
#' @export
#'
#' @examples
DD_to_DDM <-  function(DD_input, axis) {
  # Stop conditions
  if (length(DD_input) != 1)
    stop("DD_input must be of length 1")
  if (!is.numeric(DD_input))
    stop("DD_input must be numeric")
  if (!axis %in% c("horizontal", "vertical"))
    stop("axis must be one of horizontal or vertical")
  
  if (abs(DD_input) > 90 & axis == "vertical") {
    warning("Vertical axis maximum is 90; NAs produced")
    return(NA)
  } else {
    if (abs(DD_input) > 180 & axis == "horizontal") {
      warning("Horizontal axis maximum is 180; NAs produced")
      return(NA)
    } else {
      return(paste0(
        trunc(absDeg),
        intToUtf8(176),
        " ",
        (abs(DD_input) - trunc(abs(DD_input))) * 60,
        " ",
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
