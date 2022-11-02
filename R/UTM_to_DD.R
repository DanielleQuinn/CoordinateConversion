#' UTM_to_DD
#'
#' @param x 
#' @param y
#' @param zone
#' @param ellipsoid
#' @param return
#'
#' @return DD
#' @export
#'
#' @examples
UTM_to_DD <-  function(x, y, zone, ellipsoid = "WGS84", return = "all") {
  
  # Stop Conditions
  if(!is.numeric(x)) stop("x argument must be numeric")
  if(!is.numeric(y)) stop("y argument must be numeric")
  if(!is.numeric(zone)) stop("zone argument must be numeric")
  if(!return %in% c("lat", "lon", "all")) stop("Invalid return argument; please specify lat, lon, or all")
  
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