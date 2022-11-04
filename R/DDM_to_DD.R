#' DDM_to_DD
#'
#' @param DDM_input FINISH
#' @param axis FINISH
#'
#' @return DD
#' @importFrom stringr str_detect str_extract str_replace str_split
#' @importFrom dplyr case_when

#'
#' @examples
#' library(sp)
#' DDM_to_DD(DDM_input = "63° 30 N", axis = "vertical")
#'
#' @export
DDM_to_DD <-  function(DDM_input, axis){
  
  # Stop conditions
  if(!axis %in% c("horizontal", "vertical")) stop("axis must be one of horizontal or vertical")
  
  if(str_detect(DDM_input, "[NSEWnsew]")) { #detect whether format uses - or NESW to denote direction
    
    firstNum <- str_extract(DDM_input, "-?\\d+")
    minuteString <- str_replace(DDM_input, firstNum, "")
    secondNum <- str_extract(minuteString, "\\d+\\.*\\d*")
    leftover <- str_extract(minuteString, "[NSEWnsew]$")
    cleaned_DDM <- paste(firstNum, secondNum, leftover)
    
    DDM_parts <- str_split(cleaned_DDM, " ")[[1]]
    degrees <-  as.double(gsub("[^0-9.-]", "", DDM_parts[1]))
    minutes <- as.double(gsub("[^0-9.-]", "", DDM_parts[2]))
    direction <- DDM_parts[3]
    
    
    DD = degrees + (minutes/60)
    if(direction == "S" | direction == "W"){
      DD <- DD*-1
    }
    
    
  } else {
    
    firstNum <- str_extract(DDM_input, "-?\\d+")
    minuteString <- str_replace(DDM_input, firstNum, "")
    secondNum <- str_extract(minuteString, "\\d+\\.*\\d*")
    cleaned_DDM <- paste(firstNum, secondNum)
    
    DDM_parts <- str_split(cleaned_DDM, " ")[[1]]
    degrees <-  as.double(gsub("[^0-9.-]", "", DDM_parts[1]))
    minutes <- as.double(gsub("[^0-9.-]", "", DDM_parts[2]))
   
    DD <- abs(degrees) +
      minutes/60
    
    if(degrees < 0){
      DD <- DD*-1
    }
  }
  
  DD <- case_when(
    axis == "vertical" & DD > 90 ~ NaN,
    axis == "horizontal" & DD > 180 ~ NaN,
    TRUE ~ DD
  )

  return(DD)
  
}
