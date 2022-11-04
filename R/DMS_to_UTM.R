#' DMS_to_UTM
#'
#' @param x FINISH
#' @param y FINISH
#' @param ellipsoid FINISH
#' @param return FINISH
#'
#' @return UTM FINISH
#'
#' @examples
#' library(sp)
#' DMS_to_UTM(x = "173° 30' 0\" E", y = "63° 30' 0\" N")
#' DMS_to_UTM(x = "173° 30' 0 E", y = "63° 30' 0 N")
#' 
#' @export
DMS_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  x <- DMS_to_DD(x, axis = "horizontal")
  y <- DMS_to_DD(y, axis = "vertical")
  
  DD_to_UTM(x, y, ellipsoid, return)
  
}
