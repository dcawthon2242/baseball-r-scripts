MLBxstatsLeaderboard20_23 <- xstats_Sheet_1
MLBxstatsLeaderboard20_23 <- separate(MLBxstatsLeaderboard20_23, YearNamePitch, into = c("year_last_name", "first_name_pitch"), sep = ", ")
MLBxstatsLeaderboard20_23 <- separate(MLBxstatsLeaderboard20_23, year_last_name, into = c("year", "last_name"), sep = " ")
MLBxstatsLeaderboard20_23 <- separate(MLBxstatsLeaderboard20_23, first_name_pitch, into = c("first_name", "pitch"), sep = "'s ")
MLBxstatsLeaderboard20_23$name <- paste(MLBxstatsLeaderboard20_23$first_name, MLBxstatsLeaderboard20_23$last_name, sep = " ")
MLBxstatsLeaderboard20_23 <- MLBxstatsLeaderboard20_23[, !(names(MLBxstatsLeaderboard20_23) %in% c("first_name", "last_name"))]
MLBxstatsLeaderboard20_23 <- MLBxstatsLeaderboard20_23[, c("name", setdiff(names(MLBxstatsLeaderboard20_23), "name"))]
MLBxstatsLeaderboard20_23 <- select(MLBxstatsLeaderboard20_23, name, year, PitchType, 'dERA','pCRA*', 'xwOBA', 'w – xw', 'wOBAcon', `xwOBAc`, `w – xw c`, `dK%`, `K – dK`, `dBB%`, `BB – dBB`)
MLBxstatsLeaderboard20_23 <- MLBxstatsLeaderboard20_23 %>%
  mutate(across(c("dK%", "K – dK", "dBB%", "BB – dBB"), ~as.numeric(gsub("%", "", .))))

MLBPitchLeaderboard20_23 <- merge(MLBxstatsLeaderboard20_23, MLBPitchLeaderboard20_23, by = c("year", "name", "PitchType"))