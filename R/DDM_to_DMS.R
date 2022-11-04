#' DDM_to_DMS
#'
#' @param DDM_input FINISH
#' @param axis FINISH
#'
#' @return DMS FINISH
#'
#' @examples
#' library(sp)
#' DDM_to_DMS(DDM_input = "63° 30 N", axis = "vertical")
#' 
#' @export
DDM_to_DMS <-  function(DDM_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  DD_to_DMS(DD_input = DDM_to_DD(DDM_input, axis), axis)
  
}
