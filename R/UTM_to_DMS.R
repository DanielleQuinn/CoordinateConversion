#' UTM_to_DMS
#'
#' @param x FINISH
#' @param y FINISH
#' @param zone FINISH
#' @param ellipsoid FINISH
#' @param return FINISH
#'
#' @return DMS FINISH
#'
#' @examples
#' library(sp)
#' UTM_to_DMS(x = 624436.9, y = 7043729.8, zone = 59)
#'
#' @export
UTM_to_DMS <-  function(x, y, zone, ellipsoid = "WGS84", return = "all") {
  
  # Stop Conditions
  if(!is.numeric(x)) stop("x argument must be numeric")
  if(!is.numeric(y)) stop("y argument must be numeric")
  if(!is.numeric(zone)) stop("zone argument must be numeric")
  if(!return %in% c("lat", "lon", "all")) stop("Invalid return argument; please specify lat, lon, or all")
  
  if(return == "all") {
    latval <- DD_to_DMS(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return = "lat"),
                        axis = "vertical")
    lonval <- DD_to_DMS(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return = "lon"),
                        axis = "horizontal")
    return(setNames(as.list(c(latval, lonval)),
                    c("lat_DMS", "lon_DMS")))
    
    
  } else {
    DD_to_DMS(DD_input = UTM_to_DD(x, y, zone, ellipsoid, return),
              ifelse(return == "lon", "horizontal", "vertical"))  
  }
  
}
