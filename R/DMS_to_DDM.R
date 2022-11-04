#' DMS_to_DDM
#'
#' @param DMS_input FINISH
#' @param axis FINISH
#'
#' @return DDM FINISH
#'
#' @examples
#' library(sp)
#' DMS_to_DDM(DMS_input = "63° 30' 0\" N", axis = "vertical")
#' DMS_to_DDM(DMS_input = "63° 30' 0 N", axis = "vertical")
#' 
#' @export
DMS_to_DDM <-  function(DMS_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  DD_to_DDM(DD_input = DMS_to_DD(DMS_input, axis), axis)
  
}
