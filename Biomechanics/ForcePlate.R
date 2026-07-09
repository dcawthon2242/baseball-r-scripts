t_of_c_force_plate <- force_plate_2 %>%
  filter(contact_time == time) %>%
  rename(
    toc_rear_force_x = rear_force_x,
    toc_rear_force_y = rear_force_y,
    toc_rear_force_z = rear_force_z,
    toc_lead_force_x = lead_force_x,
    toc_lead_force_y = lead_force_y,
    toc_lead_force_z = lead_force_z
  ) %>%
  select(-time)


max_forces <- force_plate_2 %>%
  group_by(session_swing) %>%
  summarise_all(.funs = max) %>%
  select(-time, -fp_10_time, -fp_100_time, -contact_time)

max_times <- force_plate_2 %>%
  group_by(session_swing) %>%
  summarise(
    time_rear_force_x_max = time[which.max(rear_force_x)],
    time_rear_force_y_max = time[which.max(rear_force_y)],
    time_rear_force_z_max = time[which.max(rear_force_z)],
    time_lead_force_x_max = time[which.max(lead_force_x)],
    time_lead_force_y_max = time[which.max(lead_force_y)],
    time_lead_force_z_max = time[which.max(lead_force_z)]
  )

force_plate_biomechanics <- left_join(t_of_c_force_plate, max_forces, by = "session_swing") %>%
  left_join(max_times, by = "session_swing")

library(dplyr)

# Assuming you have the 'dplyr' package loaded

# Check if both data frames exist and are data frames
if(exists("hitting_biomechanics") && is.data.frame(hitting_biomechanics) &&
   exists("force_plate_biomechanics") && is.data.frame(force_plate_biomechanics)) {
  
  # Find rows in hitting_biomechanics that don't exist in force_plate_biomechanics
  rows_only_in_hitting <- anti_join(hitting_biomechanics, force_plate_biomechanics, by = "session_swing")
  
  # View the rows only in hitting_biomechanics
  print(rows_only_in_hitting)
  
} else {
  print("One or both of the data frames do not exist or are not data frames.")
}

library(dplyr)

# Assuming you have the 'dplyr' package loaded

# Check if both data frames exist and are data frames
if(exists("hitting_biomechanics") && is.data.frame(hitting_biomechanics) &&
   exists("force_plate_biomechanics") && is.data.frame(force_plate_biomechanics)) {
  
  # Find rows in hitting_biomechanics that don't exist in force_plate_biomechanics
  rows_to_remove <- anti_join(hitting_biomechanics, force_plate_biomechanics, by = "session_swing")
  
  # Remove the identified rows from hitting_biomechanics
  hitting_biomechanics <- hitting_biomechanics[!rownames(hitting_biomechanics) %in% rownames(rows_to_remove), ]
  
} else {
  print("One or both of the data frames do not exist or are not data frames.")
}

force_plate_biomechanics_joined <- left_join(force_plate_biomechanics, hitting_biomechanics, by = "session_swing")
force_plate_biomechanics_joined <- na.omit(force_plate_biomechanics_joined)

library(Borute)




