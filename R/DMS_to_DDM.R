#' DMS_to_DDM
#'
#' @param DMS_input 
#' @param axis 
#'
#' @return DDM
#' @export
#'
#' @examples
DMS_to_DDM <-  function(DMS_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  DD_to_DDM(DD_input = DMS_to_DD(DMS_input, axis), axis)
  
}
