#' DD_to_UTM
#'
#' @param lat 
#' @param lon
#' @param ellipsoid
#'
#' @return UTM
#' 
#' @importFrom sp coordinates<- proj4string<- CRS spTransform
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate pull
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' 
#' @export
#'
#' @examples
DD_to_UTM <-  function(x, y, ellipsoid = "WGS84", return = "all") {
  
  # Stop conditions
  if(!is.numeric(x)) stop("x must be numeric")
  if(!is.numeric(y)) stop("y must be numeric")
  if(length(x) != 1) stop("x must be of length 1")
  if(length(y) != 1) stop("y must be of length 1")
  if(!return %in% c("all", "easting", "northing", "zone")) stop("return must be one of all, easting, northing, or zone")
  
  xy <- data.frame(easting = x, northing = y)
  
  coordinates(xy) <- c("easting", "northing")
  proj4string(xy) <- CRS(paste0("+proj=longlat + datum=", ellipsoid))
  
  zone <- case_when(
    y >= 56 & y < 64 & x >= 3 & x < 6 ~ 32,
    y >= 72 & x >= 6  & x < 9 ~ 31,
    y >= 72 & x >= 9  & x < 12 ~ 33,
    y >= 72 & x >= 18 & x < 21 ~ 33,
    y >= 72 & x >= 21 & x < 24 ~ 35,
    y >= 72 & x >= 30 & x < 33 ~ 35,
    y >= 72 & x >= 33 & x < 42 ~ 37,
    x >= -180 & x <= 180 ~ (floor((x + 180)/6) %% 60) + 1,
    TRUE ~ NA_real_)
  
  res <- spTransform(xy, CRS(paste0("+proj=utm +zone=", zone, " ellps=", ellipsoid))) %>%
    as.data.frame() %>%
    select(.data$easting, .data$northing) %>%
    mutate(zone = zone)
  
  if(return == "all") {
    return(setNames(as.list(c(res$easting, res$northing, res$zone)),
           c("easting", "northing", "zone")))}
  else {
    return(res %>% pull(all_of(return)))
  }
  
}
