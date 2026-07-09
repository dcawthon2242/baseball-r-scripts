#Create a function that creates a weighted mean
custom_round <- function(value, weight) {
  if(all(is.na(value))) return(NA)
  round(weighted.mean(value, weight, na.rm = TRUE), 3)
}
# Create Leaderboard which lists all pitch types thrown more than 10% of the time
MLBPitcherLeaderboard20_23 <- MLBPitchLeaderboard20_23 %>%
  group_by(name, year) %>%
  summarize(
    PitchCount = round(mean(Pitches, na.rm = TRUE), 0),
    PitchMix = sum(`Usage%` > 10, na.rm = TRUE),
    Primary = ifelse(PitchMix >= 1,
                     paste(na.omit(PitchType[which(`Usage%` == sort(`Usage%`, decreasing = TRUE)[1], arr.ind = TRUE)]), collapse = "&"),
                     NA),
    Secondary = ifelse(PitchMix >= 2,
                     paste(na.omit(PitchType[which(`Usage%` == sort(`Usage%`, decreasing = TRUE)[2], arr.ind = TRUE)]), collapse = "&"),
                     NA),
    Tertiary = ifelse(PitchMix >= 3, 
                      paste(na.omit(PitchType[which(`Usage%` == sort(`Usage%`, decreasing = TRUE)[3], arr.ind = TRUE)]), collapse = "&"), 
                      NA),
    Quarternary = ifelse(PitchMix >= 4,
                         paste(na.omit(PitchType[which(`Usage%` == sort(`Usage%`, decreasing = TRUE)[4], arr.ind = TRUE)]), collapse = "&"),
                         NA),
    Quinary = ifelse(PitchMix >= 5,
                     paste(na.omit(PitchType[which(`Usage%` == sort(`Usage%`, decreasing = TRUE)[5], arr.ind = TRUE)]), collapse = "&"),
                     NA),
    ISO = custom_round(ISO, `Usage%`),
    KPct = custom_round(`K%`, `Usage%`),
    BBPct = custom_round(`uBB%`, `Usage%`),
    KminusBB = custom_round(`K–BB%`, `Usage%`),
    HR = sum(HR),
    AVG = custom_round(AVG, `Usage%`),
    OBP = custom_round(OBP, `Usage%`),
    SLG = custom_round(SLG, `Usage%`),
    BABIP = custom_round(BABIP, `Usage%`),
    ChasePct = custom_round(Chase, `Usage%`),
    WhiffPerSw = custom_round(`Whiff/Sw`, `Usage%`),
    CSWPct = custom_round(`CSW`, `Usage%`),
    BarrelPct = custom_round(`Barrel%`, `Usage%`),
    GBPct = custom_round(`GB%`, `Usage%`),
    xwOBAcon = custom_round(xwOBAc, `Usage%`),
    xwOBA = custom_round(xwOBA, `Usage%`),
    wOBA = custom_round(wOBA, `Usage%`),
    dERA = custom_round(dERA, `Usage%`),
    pCRA = custom_round(`pCRA*`, `Usage%`),
    dKPct = custom_round(`dK%`, `Usage%`),
    dBBPct = custom_round(`dBB%`, `Usage%`),
    AvgEV = custom_round(`EV`, `Usage%`),
    HardHitPct = custom_round(`Hard%`, `Usage%`),
    DHHPct = custom_round(`DHH%`, `Usage%`),
    BlastPct = custom_round(`Blast%`, `Usage%`),
    SolidPct = custom_round(`Solid%`, `Usage%`),
    FlarePct = custom_round(`Flare%`, `Usage%`),
    PoorPct = custom_round(`Poor%`, `Usage%`),
    AvgLA = custom_round(`LA`, `Usage%`),
    sdLA = custom_round(`sd(LA)`, `Usage%`),
    LDPct = custom_round(`LD%`, `Usage%`),
    FBPct = custom_round(`FB%`, `Usage%`),
    PUPct = custom_round(`PU%`, `Usage%`),
    PullFBPct = custom_round(`PullFB%`, `Usage%`),
    SwingPct = custom_round(`Swing`, `Usage%`),
    ContactPct = custom_round(`Contact`, `Usage%`),
    SwStrkPct = custom_round(`SwStr`, `Usage%`),
    CallStrPct = custom_round(`CallStr`, `Usage%`),
    FoulPct = custom_round(`Foul`, `Usage%`),
    FPStrkPct = custom_round(`F-Strk`, `Usage%`),
    PutawayPct = custom_round(`Putaway`, `Usage%`),
    HeartPct = custom_round(`Heart`, `Usage%`),
    ShadowPct = custom_round(`Shadow`, `Usage%`)
    
    ) %>%
  filter(PitchCount >= 100) %>%
  mutate(Secondaries = paste(Secondary, Tertiary, sep = " & ")) %>%
  mutate(TwoPitchMix = paste(Primary, Secondary, Tertiary, sep = " & ")) %>%
  mutate(Arsenal = paste(Primary, Secondary, Tertiary, Quarternary, Quinary, sep = " & "))
  # Sort pitchers into arsenals and make leaderboard
  ArsenalTendencies <- MLBPitcherLeaderboard20_23 %>%
    group_by(Arsenal) %>%
    summarise(
      PitcherUsage = n(),
      ISO = custom_round(ISO, `PitchCount`),
      KPct = custom_round(KPct, `PitchCount`),
      BBPct = custom_round(BBPct, `PitchCount`),
      KminusBB = custom_round(KminusBB, `PitchCount`),
      HR = sum(HR),
      AVG = custom_round(AVG, `PitchCount`),
      OBP = custom_round(OBP, `PitchCount`),
      SLG = custom_round(SLG, `PitchCount`),
      BABIP = custom_round(BABIP, `PitchCount`),
      ChasePct = custom_round(ChasePct, `PitchCount`),
      WhiffPerSw = custom_round(WhiffPerSw, `PitchCount`),
      CSWPct = custom_round(`CSWPct`, `PitchCount`),
      BarrelPct = custom_round(`BarrelPct`, `PitchCount`),
      GBPct = custom_round(GBPct, PitchCount),
      xwOBAcon = custom_round(xwOBAcon, `PitchCount`),
      xwOBA = custom_round(xwOBA, `PitchCount`),
      wOBA = custom_round(wOBA, `PitchCount`),
      dERA = custom_round(dERA, `PitchCount`),
      pCRA = custom_round(`pCRA`, `PitchCount`),
      dKPct = custom_round(`dKPct`, `PitchCount`),
      dBBPct = custom_round(`dBBPct`, `PitchCount`),
      AvgEV = custom_round(`AvgEV`, `PitchCount`),
      HardHitPct = custom_round(`HardHitPct`, `PitchCount`),
      DHHPct = custom_round(`DHHPct`, `PitchCount`),
      BlastPct = custom_round(`BlastPct`, `PitchCount`),
      SolidPct = custom_round(`SolidPct`, `PitchCount`),
      FlarePct = custom_round(`FlarePct`, `PitchCount`),
      PoorPct = custom_round(`PoorPct`, `PitchCount`),
      AvgLA = custom_round(`AvgLA`, `PitchCount`),
      sdLA = custom_round(`sdLA`, `PitchCount`),
      LDPct = custom_round(`LDPct`, `PitchCount`),
      FBPct = custom_round(`FBPct`, `PitchCount`),
      PUPct = custom_round(`PUPct`, `PitchCount`),
      PullFBPct = custom_round(`PullFBPct`, `PitchCount`),
      SwingPct = custom_round(`SwingPct`, `PitchCount`),
      ContactPct = custom_round(`ContactPct`, `PitchCount`),
      SwStrkPct = custom_round(`SwStrkPct`, `PitchCount`),
      CallStrPct = custom_round(`CallStrPct`, `PitchCount`),
      FoulPct = custom_round(`FoulPct`, `PitchCount`),
      FPStrkPct = custom_round(`FPStrkPct`, `PitchCount`),
      PutawayPct = custom_round(`PutawayPct`, `PitchCount`),
      HeartPct = custom_round(`HeartPct`, `PitchCount`),
      ShadowPct = custom_round(`ShadowPct`, `PitchCount`),
      PitchCount = sum(PitchCount)
    ) %>%
  filter(PitchCount >= 500)
  # Two-Pitch Mix Arsenal Leaderboard
  TwoPitchMixTendencies23 <- MLBPitcherLeaderboard20_23 %>%
    filter(PitchMix == 2) %>%
    group_by(Arsenal) %>%
    summarise(
    PitcherUsage = n(),
    ISO = custom_round(ISO, `PitchCount`),
    KPct = custom_round(KPct, `PitchCount`),
    BBPct = custom_round(BBPct, `PitchCount`),
    KminusBB = custom_round(KminusBB, `PitchCount`),
    HR = sum(HR),
    AVG = custom_round(AVG, `PitchCount`),
    OBP = custom_round(OBP, `PitchCount`),
    SLG = custom_round(SLG, `PitchCount`),
    BABIP = custom_round(BABIP, `PitchCount`),
    ChasePct = custom_round(ChasePct, `PitchCount`),
    WhiffPerSw = custom_round(WhiffPerSw, `PitchCount`),
    CSWPct = custom_round(`CSWPct`, `PitchCount`),
    BarrelPct = custom_round(`BarrelPct`, `PitchCount`),
    GBPct = custom_round(`GBPct`, `PitchCount`),
    xwOBAcon = custom_round(xwOBAcon, `PitchCount`),
    xwOBA = custom_round(xwOBA, `PitchCount`),
    wOBA = custom_round(wOBA, `PitchCount`),
    dERA = custom_round(dERA, `PitchCount`),
    pCRA = custom_round(`pCRA`, `PitchCount`),
    dKPct = custom_round(`dKPct`, `PitchCount`),
    dBBPct = custom_round(`dBBPct`, `PitchCount`),
    AvgEV = custom_round(`AvgEV`, `PitchCount`),
    HardHitPct = custom_round(`HardHitPct`, `PitchCount`),
    DHHPct = custom_round(`DHHPct`, `PitchCount`),
    BlastPct = custom_round(`BlastPct`, `PitchCount`),
    SolidPct = custom_round(`SolidPct`, `PitchCount`),
    FlarePct = custom_round(`FlarePct`, `PitchCount`),
    PoorPct = custom_round(`PoorPct`, `PitchCount`),
    AvgLA = custom_round(`AvgLA`, `PitchCount`),
    sdLA = custom_round(`sdLA`, `PitchCount`),
    LDPct = custom_round(`LDPct`, `PitchCount`),
    FBPct = custom_round(`FBPct`, `PitchCount`),
    PUPct = custom_round(`PUPct`, `PitchCount`),
    PullFBPct = custom_round(`PullFBPct`, `PitchCount`),
    SwingPct = custom_round(`SwingPct`, `PitchCount`),
    ContactPct = custom_round(`ContactPct`, `PitchCount`),
    SwStrkPct = custom_round(`SwStrkPct`, `PitchCount`),
    CallStrPct = custom_round(`CallStrPct`, `PitchCount`),
    FoulPct = custom_round(`FoulPct`, `PitchCount`),
    FPStrkPct = custom_round(`FPStrkPct`, `PitchCount`),
    PutawayPct = custom_round(`PutawayPct`, `PitchCount`),
    HeartPct = custom_round(`HeartPct`, `PitchCount`),
    ShadowPct = custom_round(`ShadowPct`, `PitchCount`),
    PitchCount = sum(PitchCount)
    ) %>%
    filter(PitchCount >= 500)
  # 3-Pitch Mix Arsenal Leaderboard
  ThreePitchMixTendencies23 <- MLBPitcherLeaderboard20_23 %>%
  filter(PitchMix == 3) %>%
  group_by(Arsenal) %>%
  summarise(
    PitcherUsage = n(),
    ISO = custom_round(ISO, `PitchCount`),
    KPct = custom_round(KPct, `PitchCount`),
    BBPct = custom_round(BBPct, `PitchCount`),
    KminusBB = custom_round(KminusBB, `PitchCount`),
    HR = sum(HR),
    AVG = custom_round(AVG, `PitchCount`),
    OBP = custom_round(OBP, `PitchCount`),
    SLG = custom_round(SLG, `PitchCount`),
    BABIP = custom_round(BABIP, `PitchCount`),
    ChasePct = custom_round(ChasePct, `PitchCount`),
    WhiffPerSw = custom_round(WhiffPerSw, `PitchCount`),
    CSWPct = custom_round(`CSWPct`, `PitchCount`),
    BarrelPct = custom_round(`BarrelPct`, `PitchCount`),
    GBPct = custom_round(`GBPct`, `PitchCount`),
    xwOBAcon = custom_round(xwOBAcon, `PitchCount`),
    xwOBA = custom_round(xwOBA, `PitchCount`),
    wOBA = custom_round(wOBA, `PitchCount`),
    dERA = custom_round(dERA, `PitchCount`),
    pCRA = custom_round(`pCRA`, `PitchCount`),
    dKPct = custom_round(`dKPct`, `PitchCount`),
    dBBPct = custom_round(`dBBPct`, `PitchCount`),
    AvgEV = custom_round(`AvgEV`, `PitchCount`),
    HardHitPct = custom_round(`HardHitPct`, `PitchCount`),
    DHHPct = custom_round(`DHHPct`, `PitchCount`),
    BlastPct = custom_round(`BlastPct`, `PitchCount`),
    SolidPct = custom_round(`SolidPct`, `PitchCount`),
    FlarePct = custom_round(`FlarePct`, `PitchCount`),
    PoorPct = custom_round(`PoorPct`, `PitchCount`),
    AvgLA = custom_round(`AvgLA`, `PitchCount`),
    sdLA = custom_round(`sdLA`, `PitchCount`),
    LDPct = custom_round(`LDPct`, `PitchCount`),
    FBPct = custom_round(`FBPct`, `PitchCount`),
    PUPct = custom_round(`PUPct`, `PitchCount`),
    PullFBPct = custom_round(`PullFBPct`, `PitchCount`),
    SwingPct = custom_round(`SwingPct`, `PitchCount`),
    ContactPct = custom_round(`ContactPct`, `PitchCount`),
    SwStrkPct = custom_round(`SwStrkPct`, `PitchCount`),
    CallStrPct = custom_round(`CallStrPct`, `PitchCount`),
    FoulPct = custom_round(`FoulPct`, `PitchCount`),
    FPStrkPct = custom_round(`FPStrkPct`, `PitchCount`),
    PutawayPct = custom_round(`PutawayPct`, `PitchCount`),
    HeartPct = custom_round(`HeartPct`, `PitchCount`),
    ShadowPct = custom_round(`ShadowPct`, `PitchCount`),
    PitchCount = sum(PitchCount)
  ) %>%
  filter(PitchCount >= 500)
  # 4-Pitch Mix Arsenal Leaderboard
  FourPitchMixTendencies23 <- MLBPitcherLeaderboard20_23 %>%
    filter(PitchMix == 4) %>%
    group_by(Arsenal) %>%
    summarise(
      PitcherUsage = n(),
      ISO = custom_round(ISO, `PitchCount`),
      KPct = custom_round(KPct, `PitchCount`),
      BBPct = custom_round(BBPct, `PitchCount`),
      KminusBB = custom_round(KminusBB, `PitchCount`),
      HR = sum(HR),
      AVG = custom_round(AVG, `PitchCount`),
      OBP = custom_round(OBP, `PitchCount`),
      SLG = custom_round(SLG, `PitchCount`),
      BABIP = custom_round(BABIP, `PitchCount`),
      ChasePct = custom_round(ChasePct, `PitchCount`),
      WhiffPerSw = custom_round(WhiffPerSw, `PitchCount`),
      CSWPct = custom_round(`CSWPct`, `PitchCount`),
      BarrelPct = custom_round(`BarrelPct`, `PitchCount`),
      GBPct = custom_round(`GBPct`, `PitchCount`),
      xwOBAcon = custom_round(xwOBAcon, `PitchCount`),
      xwOBA = custom_round(xwOBA, `PitchCount`),
      wOBA = custom_round(wOBA, `PitchCount`),
      dERA = custom_round(dERA, `PitchCount`),
      pCRA = custom_round(`pCRA`, `PitchCount`),
      dKPct = custom_round(`dKPct`, `PitchCount`),
      dBBPct = custom_round(`dBBPct`, `PitchCount`),
      AvgEV = custom_round(`AvgEV`, `PitchCount`),
      HardHitPct = custom_round(`HardHitPct`, `PitchCount`),
      DHHPct = custom_round(`DHHPct`, `PitchCount`),
      BlastPct = custom_round(`BlastPct`, `PitchCount`),
      SolidPct = custom_round(`SolidPct`, `PitchCount`),
      FlarePct = custom_round(`FlarePct`, `PitchCount`),
      PoorPct = custom_round(`PoorPct`, `PitchCount`),
      AvgLA = custom_round(`AvgLA`, `PitchCount`),
      sdLA = custom_round(`sdLA`, `PitchCount`),
      LDPct = custom_round(`LDPct`, `PitchCount`),
      FBPct = custom_round(`FBPct`, `PitchCount`),
      PUPct = custom_round(`PUPct`, `PitchCount`),
      PullFBPct = custom_round(`PullFBPct`, `PitchCount`),
      SwingPct = custom_round(`SwingPct`, `PitchCount`),
      ContactPct = custom_round(`ContactPct`, `PitchCount`),
      SwStrkPct = custom_round(`SwStrkPct`, `PitchCount`),
      CallStrPct = custom_round(`CallStrPct`, `PitchCount`),
      FoulPct = custom_round(`FoulPct`, `PitchCount`),
      FPStrkPct = custom_round(`FPStrkPct`, `PitchCount`),
      PutawayPct = custom_round(`PutawayPct`, `PitchCount`),
      HeartPct = custom_round(`HeartPct`, `PitchCount`),
      ShadowPct = custom_round(`ShadowPct`, `PitchCount`),
      PitchCount = sum(PitchCount)
    ) %>%
    filter(PitchCount >=500)
  # 5-Pitch Mix Arsenal Leaderboard
  FivePitchMixTendencies23 <- MLBPitcherLeaderboard20_23 %>%
    filter(PitchMix == 5) %>%
    group_by(Arsenal) %>%
    summarise(
      PitcherUsage = n(),
      ISO = custom_round(ISO, `PitchCount`),
      KPct = custom_round(KPct, `PitchCount`),
      BBPct = custom_round(BBPct, `PitchCount`),
      KminusBB = custom_round(KminusBB, `PitchCount`),
      HR = sum(HR),
      AVG = custom_round(AVG, `PitchCount`),
      OBP = custom_round(OBP, `PitchCount`),
      SLG = custom_round(SLG, `PitchCount`),
      BABIP = custom_round(BABIP, `PitchCount`),
      ChasePct = custom_round(ChasePct, `PitchCount`),
      WhiffPerSw = custom_round(WhiffPerSw, `PitchCount`),
      CSWPct = custom_round(`CSWPct`, `PitchCount`),
      BarrelPct = custom_round(`BarrelPct`, `PitchCount`),
      GBPct = custom_round(`GBPct`, `PitchCount`),
      xwOBAcon = custom_round(xwOBAcon, `PitchCount`),
      xwOBA = custom_round(xwOBA, `PitchCount`),
      wOBA = custom_round(wOBA, `PitchCount`),
      dERA = custom_round(dERA, `PitchCount`),
      pCRA = custom_round(`pCRA`, `PitchCount`),
      dKPct = custom_round(`dKPct`, `PitchCount`),
      dBBPct = custom_round(`dBBPct`, `PitchCount`),
      AvgEV = custom_round(`AvgEV`, `PitchCount`),
      HardHitPct = custom_round(`HardHitPct`, `PitchCount`),
      DHHPct = custom_round(`DHHPct`, `PitchCount`),
      BlastPct = custom_round(`BlastPct`, `PitchCount`),
      SolidPct = custom_round(`SolidPct`, `PitchCount`),
      FlarePct = custom_round(`FlarePct`, `PitchCount`),
      PoorPct = custom_round(`PoorPct`, `PitchCount`),
      AvgLA = custom_round(`AvgLA`, `PitchCount`),
      sdLA = custom_round(`sdLA`, `PitchCount`),
      LDPct = custom_round(`LDPct`, `PitchCount`),
      FBPct = custom_round(`FBPct`, `PitchCount`),
      PUPct = custom_round(`PUPct`, `PitchCount`),
      PullFBPct = custom_round(`PullFBPct`, `PitchCount`),
      SwingPct = custom_round(`SwingPct`, `PitchCount`),
      ContactPct = custom_round(`ContactPct`, `PitchCount`),
      SwStrkPct = custom_round(`SwStrkPct`, `PitchCount`),
      CallStrPct = custom_round(`CallStrPct`, `PitchCount`),
      FoulPct = custom_round(`FoulPct`, `PitchCount`),
      FPStrkPct = custom_round(`FPStrkPct`, `PitchCount`),
      PutawayPct = custom_round(`PutawayPct`, `PitchCount`),
      HeartPct = custom_round(`HeartPct`, `PitchCount`),
      ShadowPct = custom_round(`ShadowPct`, `PitchCount`),
      PitchCount = sum(PitchCount)
    ) %>%
    filter(PitchCount >= 500)
  # Create leaderboard of arsenals which include curveballs
  # Split into individual arsenals involving curveball and another pitch
  
  curveball_rows <- MLBPitcherLeaderboard20_23[grepl("Curve", MLBPitcherLeaderboard20_23$Arsenal, ignore.case = TRUE), ]
  CUSL_rows <- curveball_rows[grepl("Slider", curveball_rows$Arsenal, ignore.case = TRUE), ]
  CUFF_rows <- curveball_rows[grepl("Four-seamer", curveball_rows$Arsenal, ignore.case = TRUE), ]
  CUSI_rows <- curveball_rows[grepl("Sinker", curveball_rows$Arsenal, ignore.case = TRUE), ]
  CUFS_rows <- curveball_rows[grepl("Splitter", curveball_rows$Arsenal, ignore.case = TRUE), ]
  CUCH_rows <- curveball_rows[grepl("Change-up", curveball_rows$Arsenal, ignore.case = TRUE), ]
  CUSW_rows <- curveball_rows[grepl("Sweeper", curveball_rows$Arsenal, ignore.case = TRUE), ]
  CUFC_rows <- curveball_rows[grepl("Cutter", curveball_rows$Arsenal, ignore.case = TRUE), ]
  
  # Create leaderboard of arsenals which include splitters
  # Split into individual arsenals involving splitter and another pitch
  
  splitter_rows <- MLBPitcherLeaderboard20_23[grepl("Splitter", MLBPitcherLeaderboard20_23$Arsenal, ignore.case = TRUE), ]
  FSSL_rows <- splitter_rows[grepl("Slider", splitter_rows$Arsenal, ignore.case = TRUE), ]
  FSFF_rows <- splitter_rows[grepl("Four-seamer", splitter_rows$Arsenal, ignore.case = TRUE), ]
  FSSI_rows <- splitter_rows[grepl("Sinker", splitter_rows$Arsenal, ignore.case = TRUE), ]
  FSCU_rows <- splitter_rows[grepl("Curve", splitter_rows$Arsenal, ignore.case = TRUE), ]
  FSCH_rows <- splitter_rows[grepl("Change-up", splitter_rows$Arsenal, ignore.case = TRUE), ]
  FSSW_rows <- splitter_rows[grepl("Sweeper", splitter_rows$Arsenal, ignore.case = TRUE), ]
  FSFC_rows <- splitter_rows[grepl("Cutter", splitter_rows$Arsenal, ignore.case = TRUE), ]
  
  # Create leaderboard of arsenals which include sliders
  # Split into individual arsenals involving slider and another pitch
  
  slider_rows <- MLBPitcherLeaderboard20_23[grepl("Slider", MLBPitcherLeaderboard20_23$Arsenal, ignore.case = TRUE), ]
  SLFS_rows <- slider_rows[grepl("Splitter", slider_rows$Arsenal, ignore.case = TRUE), ]
  SLFF_rows <- slider_rows[grepl("Four-seamer", slider_rows$Arsenal, ignore.case = TRUE), ]
  SLSI_rows <- slider_rows[grepl("Sinker", slider_rows$Arsenal, ignore.case = TRUE), ]
  SLCU_rows <- slider_rows[grepl("Curve", slider_rows$Arsenal, ignore.case = TRUE), ]
  SLCH_rows <- slider_rows[grepl("Change-up", slider_rows$Arsenal, ignore.case = TRUE), ]
  SLSW_rows <- slider_rows[grepl("Sweeper", slider_rows$Arsenal, ignore.case = TRUE), ]
  SLFC_rows <- slider_rows[grepl("Cutter", slider_rows$Arsenal, ignore.case = TRUE), ]
  
  # Create leaderboard of arsenals which include sliders
  # Split into individual arsenals involving slider and another pitch
  
  FFFSCU_rows <- FSCU_rows[grepl("Four-seamer", FSCU_rows$Arsenal, ignore.case =TRUE), ]
  FFFSSL_rows <- FSSL_rows[grepl("Four-seamer", FSSL_rows$Arsenal, ignore.case =TRUE), ]
  FFFCSL_rows <- SLFC_rows[grepl("Four-Seamer", SLFC_rows$Arsenal, ignore.case = TRUE), ]
  FFFCCU_rows <- CUFC_rows[grepl("Four-Seamer", CUFC_rows$Arsenal, ignore.case = TRUE), ]
  SLFCCU_rows <- CUFC_rows[grepl("Slider", CUFC_rows$Arsenal, ignore.case = TRUE), ]
  SLCUSW_rows <- SLSW_rows[grepl("Curve", SLSW_rows$Arsenal, ignore.case = TRUE), ]
  FCCUSW_rows <- CUFC_rows[grepl("Sweeper", CUFC_rows$Arsenal, ignore.case = TRUE), ]
  
  # Create leaderboards based off interesting trends with curveball-centric arsenals
  
  FSFF_NoCurve <- FSFF_rows[! grepl("Curve", FSFF_rows$Arsenal, ignore.case = TRUE), ]
  SLFS_NoCurve <- SLFS_rows[! grepl("Curve", SLFS_rows$Arsenal, ignore.case = TRUE), ]
  SLFC_NoCurve <- SLFC_rows[! grepl("Curve", SLFC_rows$Arsenal, ignore.case = TRUE), ]