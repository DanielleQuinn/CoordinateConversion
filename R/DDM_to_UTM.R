#' DDM_to_UTM
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
#' DDM_to_UTM(x = "173° 30 E", y = "63° 30 N")
#' 
#' @export
DDM_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  x <- DDM_to_DD(x, axis = "horizontal")
  y <- DDM_to_DD(y, axis = "vertical")

  DD_to_UTM(x, y, ellipsoid, return)
  
}
