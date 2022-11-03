#' DD_to_DMS
#'
#' @param DD_input 
#' @param axis 
#'
#' @return DMS
#' @export
#'
#' @examples
DD_to_DMS <-  function(DD_input, axis){
  
  # Stop conditions
  if(!is.numeric(DD_input)) stop("DD_input must be numeric")
  if(length(DD_input) != 1) stop("DD_input must be of length 1")
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  # Warning conditions
  if(abs(DD_input) > 90 & axis == "vertical") warning("Vertical axis maximum is 90; NAs produced")
  if(abs(DD_input) > 180 & axis == "horizontal") warning("Horizontal axis maximum is 180; NAs produced")
  
  if(DD_input < 0) DD_input <-  DD_input * -1
  degree <- floor(DD_input)
  minute <- floor((DD_input - degree) * 60)
  second <- round(((DD_input - degree) * 60 - minute) * 60)
  
  suffix <- if(axis == "horizontal") ifelse(DD_input < 0, "W", "E") else ifelse(DD_input < 0, "S", "N")

  DMS <- case_when(
    axis == "vertical"  & !between(degree, -90, 90) ~ NA_character_,
    axis == "horizontal" & !between(degree, -180, 180) ~ NA_character_,
    TRUE ~ paste0(degree, intToUtf8(176), " ", minute, "' ", second, '"'," ", suffix)
  )
  
  return(DMS)
}
