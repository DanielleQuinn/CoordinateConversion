#' DD_to_UTM
#'
#' @param x FINISH
#' @param y FINISH
#' @param ellipsoid FINISH
#' @param return FINISH
#'
#' @return UTM FINISH
#' 
#' @importFrom sp coordinates<- proj4string<- CRS spTransform
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate pull
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' 
#' @examples
#' library(sp)
#' DD_to_UTM(x = 173.5, y = 63.5)
#'
#' @export
#'
DD_to_UTM <- function(x, y, ellipsoid = "WGS84", return = "all") {
  
  # To Fix
  # y argument should be "DD_lat", x argument should be "DD_lon"
  # clean variable names in function process to clarify what's actually happening
  # Consider removing numeric stops and return NA similar to other DD_to_* functions

  # Stop conditions
  if(length(x) != 1) stop("x must be of length 1")
  if(length(y) != 1) stop("y must be of length 1")
  if(!is.numeric(x)) stop("x must be numeric")
  if(!is.numeric(y)) stop("y must be numeric")
  if(abs(x) > 180) stop("x must be between -180 and 180")
  if(abs(y) > 90) stop("y must be between -90 and 90")
  if(!return %in% c("all", "easting", "northing", "zone", "hemisphere")) stop("return must be one of all, easting, northing, zone, or hemisphere")
  
  xy <- data.frame(x, y)
  coordinates(xy) <- c("x", "y")
  proj4string(xy) <- CRS(paste0("+proj=longlat + datum=", ellipsoid))
  
  # Need to clarify why the standard formula isn't used for all cases
  # ceiling((DD_lon + 180)/6)
  # Also, don't we need to specify S vs N zone?
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
    select(easting = .data$x, northing = .data$y) %>%
    mutate(zone = zone,
           hemisphere = case_when(
             y >= 0 ~ "N",
             y < 0 ~ "S"
           ))
  
  # NOTE: With hemisphere added, will need to add step
  ## to convert others to numeric in sp_convert
  # Also, consider decimals
  if(return == "all") {
    return(setNames(as.list(c(as.numeric(res$easting), res$northing, res$zone, res$hemisphere)),
           c("easting", "northing", "zone", "hemisphere")))}
  else {
    return(res %>% pull(all_of(return)))
  }
  
}
