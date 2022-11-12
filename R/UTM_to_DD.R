#' UTM_to_DD
#'
#' @param x FINISH
#' @param y FINISH
#' @param zone FINISH
#' @param ellipsoid FINISH
#' @param return FINISH
#'
#' @return DD FINISH
#'
#' @examples
#' library(sp)
#' UTM_to_DD(x = 624436.9, y = 7043729.8, zone = 59)
#' 
#' @export
UTM_to_DD <-  function(x, y, zone, hemisphere = "N", ellipsoid = "WGS84", return = "all") {
  
  # Stop Conditions
  if(!is.numeric(x)) stop("x argument must be numeric")
  if(!is.numeric(y)) stop("y argument must be numeric")
  if(!between(x, 160000, 834000)) stop("x out of range")
  if(!between(y, 0, 10000000)) stop("y out of range")
  if(!is.numeric(zone)) stop("zone argument must be numeric")
  if(!between(zone, 0, 60)) stop("zone out of range")
  if(!hemisphere %in% c("S", "N")) stop("Invalid hemisphere argument; should be one of S or N")
  if(!return %in% c("lat", "lon", "all")) stop("Invalid return argument; should be one of lat, lon, or all")
  
  utm <- data.frame(lon = x, lat = y)
  coordinates(utm) <- ~lon+lat
  proj4string(utm) <- CRS(paste0("+proj=utm +zone=", zone, "+ellps=", ellipsoid))
  utm <- spTransform(utm, CRS(paste0("+proj=longlat +datum=", ellipsoid))) %>%
    as.data.frame()
  
  if(return == "all") {
    return(data.frame(lat_DD = utm$lat, lon_DD = utm$lon))
  } else {
    return(utm %>% pull(all_of(return)))
  }
}
