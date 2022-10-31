#' DMS_to_DD
#'
#' @param DMS_input 
#' @param axis 
#'
#' @return DD
#' @export
#'
#' @examples
DMS_to_DD <-  function(DMS_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  firstNum <- str_extract(DMS_input, "-?\\d+")
  minuteString <- str_replace(DMS_input, firstNum, "")
  secondNum <- str_extract(minuteString, "\\d+")
  secondString <- str_replace(minuteString, secondNum, "")
  thirdNum <- str_extract(secondString, "\\d+\\.*\\d*")
  leftover <- str_extract(secondString, "[NSEWnsew]$")
  cleaned_DMS <- paste(firstNum, secondNum, thirdNum, leftover)
  DMS_parts <- str_split(cleaned_DMS, " ")[[1]]
  
  degrees <-  as.double(gsub("[^0-9.-]", "", DMS_parts[1]))
  minutes <- as.double(gsub("[^0-9.-]", "", DMS_parts[2]))
  seconds <- as.double(gsub("[^0-9.-]", "", DMS_parts[3]))
  
  direction <- DMS_parts[4]
  DD <- degrees + (minutes/60) + (seconds/3600) 

  DD <- case_when(
    direction %in% c("S", "W") ~ DD * -1,
    axis == "vertical" & DD > 90 ~ NaN,
    axis == "horizontal" & DD > 180 ~ NaN,
    TRUE ~ DD
  )
  
  return(DD)

}
