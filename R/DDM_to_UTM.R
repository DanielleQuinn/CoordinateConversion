#' DDM_to_UTM
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
DDM_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  x <- DDM_to_DD(x, axis = "horizontal")
  y <- DDM_to_DD(y, axis = "vertical")

  DD_to_UTM(x, y, ellipsoid, return)
  
}
