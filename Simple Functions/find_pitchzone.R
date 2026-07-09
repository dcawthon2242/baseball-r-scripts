find_pitchzone <- function(data) {
  # Create a new column 'pitchzone' with default value 'waste'
  data$pitchzone <- 'waste'
  
  # Define conditions
  condition_waste <- data$pz < 0.5 | data$pz > 4.5 | data$px < -1.67 | data$px > 1.67
  condition_chase <- data$pz >= 0.5 & data$pz <= 4.5 & data$px >= -1.67 & data$px <= 1.67
  condition_shadow <- (data$pz > 1.17 & data$pz <= 3.83) & (data$px >= -1.11 & data$px <= 1.11)
  condition_heart <- (data$pz > 1.83 & data$pz <= 3.17) & (data$px >= -0.56 & data$px <= 0.56)
  
  # Update 'pitchzone' column
  data$pitchzone[condition_waste] <- 'Waste'
  data$pitchzone[condition_chase] <- 'Chase'
  data$pitchzone[condition_shadow] <- 'Shadow'
  data$pitchzone[condition_heart] <- 'Heart'
  
  return(data)
}