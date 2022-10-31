#' UTM_to_DDM
#'
#' @param x 
#' @param y
#' @param zone
#' @param ellipsoid
#' @param return
#'
#' @return DDM
#' @export
#'
#' @examples
UTM_to_DDM <-  function(x, y, zone, ellipsoid = "WGS84", return = "all") {
  
  # Stop Conditions
  if(!is.numeric(x)) stop("x argument must be numeric")
  if(!is.numeric(y)) stop("y argument must be numeric")
  if(!is.numeric(zone)) stop("zone argument must be numeric")
  if(!return %in% c("lat", "lon", "all")) stop("Invalid return argument; please specify lat, lon, or all")
  
  if(return == "all") {
    latval <- DD_to_DDM(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return = "lat"),
                        axis = "vertical")
    lonval <- DD_to_DDM(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return = "lon"),
                        axis = "horizontal")
    return(setNames(as.list(c(latval, lonval)),
                    c("lat_DDM", "lon_DDM")))
    
  } else {
    DD_to_DDM(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return),
              ifelse(return == "lon", "horizontal", "vertical"))  
  }
  
}
