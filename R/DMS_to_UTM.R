#' DMS_to_UTM
#'
#' @param x
#' @param y
#' @param ellipsoid
#' @param return
#'
#' @return UTM
#' @export
#'
#' @examples
DMS_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  x <- DMS_to_DD(x, axis = "horizontal")
  y <- DMS_to_DD(y, axis = "vertical")
  
  DD_to_UTM(x, y, ellipsoid, return)
  
}
