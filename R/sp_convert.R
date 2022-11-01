#' Convert coordinates in a data frame
#'
#' Use existing spatial data in a data frame to convert to other coordinate systems.
#'
#' @param data A data frame.
#' @param x The name of the variable representing the horizontal spatial coordinate; defaults to lon.
#' @param y The name of the variable representing the horizontal spatial coordinate; defaults to lat.
#' @param from The initial coordinate system. One of DD, DMS, DDM, or UTM. Defaults to DD.
#' @param to The initial coordinate system. One of DD, DMS, DDM, or UTM, or all. Defaults to all.
#' @param zone If converting from UTM, the name of the numeric variable containing UTM zone; defaults to zone.
#' @param ellipsoid The ellipsoid ID. Defaults to WGS84.
#'
#' @return The original data frame with converted coordinates appended as new columns
#'
#' @examples
#' library(sp)
#' df_dd <- data.frame(lon = c( -63, -89.2345, 4.5, 4.5), lat = c(44, 23.34, 57.3246, 57.3246))
#' sp_convert(df_dd, from = "DD", to = "DDM")
#' 
#' df_utm <- sp_convert(df_dd, from = "DD", to = "UTM")
#' sp_convert(df_utm, x = "easting", y = "northing", from = "UTM", to = "DDM")
#' 
#' @importFrom rlang :=
#' @importFrom tidyr drop_na
#' @importFrom dplyr rename distinct rowwise across rename_with left_join between
#' @importFrom tidyselect starts_with
#' @importFrom sp spTransform
#' 
#' @export
sp_convert <- function(data, x = "lon", y = "lat",
                       from = "DD", to = "all",
                       zone = "zone", ellipsoid = "WGS84") {
  
  . <- NULL
  
  #### Stop Conditions 
  if(!is.data.frame(data)) stop("data object must be a data frame")
  if(!x %in% names(data)) stop("x argument does not match any variables in data")
  if(!y %in% names(data)) stop("y argument does not match any variables in data")
  if(length(to) == 1) {if(from == to) stop("from argument matches to argument; no conversion required")}
  if(!from %in% c("DD", "DMS", "DDM", "UTM")) stop("Invalid from argument; please specify DD, DMS, DDM, or UTM")
  if(!from %in% c("all", "DD", "DMS", "DDM", "UTM")) stop("Invalid from argument; please specify all, DD, DMS, DDM, or UTM")
  if(from == "UTM" & !zone %in% names(data)) stop("zone variable missing from data")
  
  #### Create Reference Table 
  distinctData <- data %>%
    rename(x = all_of(x), y = all_of(y)) %>%
    drop_na(x, y) %>%
    # If UTM is being used, rename zone variable
    {if(from == "UTM") rename(., zone = all_of(zone)) else .} %>%
    # Keep unique combinations of 'x' and 'y'; if 'from' is UTM, also keep 'zone'
    {if(from == "UTM") distinct(., x, y, zone) else distinct(., x, y)}
  
  #### Identify Conversion Functions 
  # If `to` value is "all", specify all options
  if("all" %in% to) {to <- c("DD", "DDM", "DMS", "UTM")}
  
  # Remove matching 'from' and 'to' values
  if(from %in% to) to <- to[!to == from]
  
  # Identify conversion functions required using from and to arguments
  use_functions <- setNames(as.list(paste(from, "to", to, sep = "_")), to)
  non_utm_functions <- use_functions[!str_detect(use_functions, "UTM")]
  to_utm_functions <- use_functions[str_detect(use_functions, "to_UTM")]
  from_utm_functions <- use_functions[str_detect(use_functions, "UTM_to")]
  
  #### Apply Conversion Functions 
  coords_data <- distinctData %>%
    rowwise() %>%
    # Apply any functions associated with non-UTM conversions
    {if (length(non_utm_functions) > 0)
      mutate(.,
             across(.cols = x, .names = "{.col}_{.fn}",
                    .fns = non_utm_functions, axis = "horizontal"),
             across(.cols = y, .names = "{.col}_{.fn}",
                    .fns = non_utm_functions, axis = "vertical"))
      else .} %>%
    # Apply any functions associated with conversions 'to' UTM
    {if (length(to_utm_functions) > 0)
      mutate(.,
             across(.cols = x, .names = "easting",
                    .fns = to_utm_functions, y = y, return = "easting"),
             across(.cols = x, .names = "northing",
                    .fns = to_utm_functions, y = y, return = "northing"),
             across(.cols = x, .names = "zone",
                    .fns = to_utm_functions, y = y, return = "zone"))
      else .} %>%
    # Apply any functions associated with conversions 'from' UTM
    {if (length(from_utm_functions) > 0)
      mutate(.,
             across(.cols = x, .names = "lat_{.fn}",
                    .fns = from_utm_functions, y = y, zone = zone, return = "lat"),
             across(.cols = x, .names = "lon_{.fn}",
                    .fns = from_utm_functions, y = y, zone = zone, return = "lon"))
      else .} %>%
    # Rename columns to clean up
    rename("{paste(x, from, sep = '_')}" := x,
           "{paste(y, from, sep = '_')}" := y) %>%
    rename_with(.fn = ~gsub("x_", "lon_", .), .cols = starts_with("x_")) %>%
    rename_with(.fn = ~gsub("y_", "lat_", .), .cols = starts_with("y_"))
  
  #### Join and Return 
  data %>%
    rename("{paste(x, from, sep = '_')}" := all_of(x), 
           "{paste(y, from, sep = '_')}" := all_of(y)) %>%
    left_join(coords_data)
}
