convert_tilt_to_degrees <- function(tilt) {
  # Capture the unquoted input as a symbol and convert it to a string
  tilt <- deparse(substitute(tilt))
  
  # Split the input string into hours and minutes
  time_parts <- strsplit(tilt, ":")[[1]]
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  
  
  # Convert hours and minutes to degrees
  degrees <- (hours %% 12) * 30 + (minutes / 60) * 30
  
  return(degrees)
}
