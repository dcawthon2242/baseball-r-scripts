#Make a column that notes if a ball is a strike or ball
TM24Szn$zone <- ifelse(TM24Szn$PlateLocHeight >= 1.5 & TM24Szn$PlateLocHeight <= 3.6 & TM24Szn$PlateLocSide >= -.83 &
                         TM24Szn$PlateLocSide <= .83, 1, 0)
TM24Szn$middlemiddle <- ifelse(TM24Szn$PlateLocHeight >= 2.2 & TM24Szn$PlateLocHeight <= 2.9 & TM24Szn$PlateLocSide >= -0.277 & TM24Szn$PlateLocSide <= 0.277, 1, 0)
#Create a column that decides which pitches elicited chases
TM24Szn$swing <- NA
# Find swings
TM24Szn$swing[TM24Szn$PitchCall == "StrikeSwinging" |
                TM24Szn$PitchCall == "InPlay" |
                TM24Szn$PitchCall == "FoulBall"] <- 1
#Use swings and chase to find occurences of chases
TM24Szn$chase <- ifelse(TM24Szn$zone == 0 & TM24Szn$swing == 1, 1, 0)
TM24Szn$ooz <- ifelse(TM24Szn$zone == 0, 1, 0)
# Calculate barrel% by counting frequency of Exit Velo is greater than 90 and Launch Angle 10-30
TM24Szn$barrel <- ifelse(TM24Szn$ExitSpeed >= 90 & TM24Szn$Angle >= 10 & TM24Szn$Angle
                         <= 30, 1, 0)
# Calculating woba using 2020 NCAA D1 woba weights
TM24Szn$wobavalue <-
  ifelse(TM24Szn$KorBB == "Strikeout", 0,
         ifelse(TM24Szn$PlayResult == "Out", 0,
                ifelse(TM24Szn$KorBB == "Walk", 0.76,
                       ifelse(TM24Szn$PitchCall == "HitByPitch", 0.79,
                              ifelse(TM24Szn$PlayResult == "Single", 0.94,
                                     ifelse(TM24Szn$PlayResult == "Double", 1.34,
                                            ifelse(TM24Szn$PlayResult == "Triple", 1.67,
                                                   ifelse(TM24Szn$PlayResult == "HomeRun", 2.08,
                                                          NA))))))))
# Calculating when a pitch is in the upper or lower third
TM24Szn$upperthird <- ifelse(TM24Szn$zone == 1 & TM24Szn$PlateLocHeight >= 2.9, 1, 0)
TM24Szn$lowerthird <- ifelse(TM24Szn$zone == 1 & TM24Szn$PlateLocHeight <= 2.2, 1, 0)
  
  # Filter all teams to just big west teams
  filter_teams <- function(data) {
    valid_teams <- c("CAL_FUL", "CAL_MAT", "CAL_ANT", "CSU_BAK", "CAL_HIG", 
                     "CSD_TRI", "HAW_WAR", "CAL_MUS", "LON_DIR", "SAN_GAU", "CAL_AGO")
    
    filtered_data <- data[data$PitcherTeam %in% valid_teams, ]
    
    return(filtered_data)
  }
  
  # Call to function and create stats in leaderboard
  BigWestPitchingLeaderboard23 <- filter_teams(TM24Szn) %>%
    group_by(PitcherTeam) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3)
    )

  # Create Leaderboard with pitch averages for each pitch for each Big West team (including usage)
  BigWestPitchLeaderboard23 <- filter_teams(TM24Szn) %>%
    group_by(PitcherTeam, TaggedPitchType) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      # Using Location variables from earlier to find location
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
         ) %>%
    # Create a variable to find the usage of each pitch type for each team
    group_by(PitcherTeam) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
# Filter out pitches thrown less than 100 times
  filter(PitchCount >= 100)
  
# Creating a leaderboard for all D1 NCAA Teams with different pitch types
  NCAATeamPitchesLeaderboard23 <- filter(TM24Szn) %>%
    group_by(Pitcher, TaggedPitchType) %>%
    summarise(
      PitchCount = n(),
      MPH = round(mean(RelSpeed, na.rm = TRUE), 1),
      IVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      RelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      RelSide = round(mean(RelSide, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      'Zone%' = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      'Chase%' = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      'Whiff%' = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      EV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      LA = round(mean(Angle, na.rm = TRUE), 1),
      'Barrel%' = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
    ) %>%
    group_by(Pitcher) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) 
  
  # Creating a leaderboard for all D1 NCAA Teams with different pitch types
  FullertonFall23 <- filter(TM24Szn) %>%
    group_by(Pitcher, TaggedPitchType) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      SpinAxis = round(mean(SpinAxis, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
    ) %>%
    group_by(Pitcher) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
    filter(PitchCount >= 5)
  
    # Creating NCAA Leaderboard RHP vs RHH
  NCAAPitchLeaderboardRHHvsRHP23 <- filter(TM24Szn, Level == "D1", BatterSide == "Right", PitcherThrows == "Right") %>%
    group_by(PitcherTeam, TaggedPitchType) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      # Using Location variables from earlier to find location
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
    ) %>%
    group_by(PitcherTeam) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
    filter(PitchCount >= 100)
  
  # Create an NCAA leaderboard for LHH vs LHP
  NCAAPitchLeaderboardLHHvsLHP23 <- filter(TM24Szn, Level == "D1", PitcherThrows == "Left", BatterSide == "Left") %>%
    group_by(PitcherTeam, TaggedPitchType) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
    ) %>%
    group_by(PitcherTeam) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
    filter(PitchCount >= 100)
  
  # Create NCAA Leaderboard for RHP vs LHH
  NCAAPitchLeaderboardLHHvsRHP23 <- filter(TM24Szn, Level == "D1", PitcherThrows == "Right", BatterSide == "Left") %>%
    group_by(PitcherTeam, TaggedPitchType) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
    ) %>%
    group_by(PitcherTeam) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
    filter(PitchCount >= 100)
  
  # Create NCAA Leaderboard for RHH vs LHP
  NCAAPitchLeaderboardRHHvsLHP23 <- filter(TM24Szn, Level == "D1", PitcherThrows == "Left", BatterSide == "Right") %>%
    group_by(PitcherTeam, TaggedPitchType) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((abs(HorzBreak)), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
    ) %>%
    group_by(PitcherTeam) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
    filter(PitchCount >= 100)
  
  # Make a Leaderboard for Fullerton Pitchers
  FullertonPitchersFall23 <- filter(TM24Szn, PitcherTeam == "CAL_FUL") %>%
    # Separate pitchers instead of Pitcher Teams
    group_by(Pitcher) %>%
    summarise(
      PitchCount = n(),
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      AvgHMov = round(mean((HorzBreak), na.rm = TRUE), 1),
      AvgIVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      ZonePct = round(mean(zone == 1, na.rm = TRUE)* 100, 1),
      ChasePct = round(mean(chase == 1, na.rm = TRUE)*100 , 1),
      WhiffPct = round(mean(PitchCall == "StrikeSwinging", na.rm = TRUE)* 100, 1),
      AvgEV = round(mean(ExitSpeed, na.rm = TRUE), 1),
      AvgLA = round(mean(Angle, na.rm = TRUE), 1),
      BarrelPct = round(mean(barrel == 1, na.rm = TRUE)*100, 1),
      VAA = mean(VertApprAngle, na.rm = TRUE),
      HAA = mean(HorzApprAngle, na.rm = TRUE),
      AvgRelHeight = round(mean(RelHeight, na.rm = TRUE), 1),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      SpinAxis = round(mean(SpinAxis, na.rm = TRUE), 0),
      woba = round(mean(wobavalue, na.rm = TRUE), 3),
      UpperThirdPct = round(mean(upperthird == 1, na.rm = TRUE) * 100, 1),
      LowerThirdPct = round(mean(lowerthird == 1, na.rm = TRUE) * 100, 1)
    ) %>%
    group_by(Pitcher) %>%
    mutate(TotalPitches = sum(PitchCount)) %>%
    ungroup() %>%
    mutate(Usage = (PitchCount / TotalPitches) * 100) %>%
    #Change pitch filter
    filter(PitchCount >= 1)
  
  