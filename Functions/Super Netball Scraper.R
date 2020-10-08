library(jsonlite)
library(tidyverse)

# season ids to be used in the fromJSON call
season_id <- tibble(x2020 = 11108, x2020_finals = 11109, x2019 = 10724, x2019_finals = 10725,
                    x2018 = 10393, x2018_finals = 10394, x2017 = 10083, x2017_finals = 10084)

# collect the fixture for the required season (fixture id is the number in the url)
fixture <- fromJSON("https://mc.championdata.com/data/11108/fixture.json?") %>%
  as.data.frame() %>%
  mutate(Index = 1,
         Index = cumsum(Index)) %>%
  select(Index, fixture.match.matchId)

# create a minimal df to bind to within the loop
complete <- tibble(matchId = 0)

# for loop to collect the stats for the required season (fixture id is the number in the url)
for(i in min(fixture$Index):max(fixture$Index)) {
  matchId <- fixture %>%
    filter(Index == i)
  
  url <- paste0("https://mc.championdata.com/data/11108/", matchId$fixture.match.matchId, ".json?")

  stats <- fromJSON(url)

  player_stats <- as.data.frame(stats$matchStats$playerStats)
  player_info <- as.data.frame(stats$matchStats$playerInfo)
  squad_info <- as.data.frame(stats$matchStats$teamInfo) %>%
    rename(player.squadId = team.squadId)
  match_info <- tibble(matchId = stats$matchStats$matchInfo$matchId, roundNumber = stats$matchStats$matchInfo$roundNumber, 
                       matchNumber = stats$matchStats$matchInfo$matchNumber)
  
  match_stats <- bind_cols(player_info, player_stats) %>%
    left_join(., squad_info, by = "player.squadId") %>%
    mutate(matchId = stats$matchStats$matchInfo$matchId, 
           roundNumber = stats$matchStats$matchInfo$roundNumber, 
           matchNumber = stats$matchStats$matchInfo$matchNumber,
           )
  
  complete <- bind_rows(complete, match_stats)
  
}

# separate dataframes for the required seasons
complete_2017 <- complete %>%
  # na.omit() %>%
  mutate(Season = 2017)

complete_2017_finals <- complete %>%
  # na.omit() %>%
  mutate(Season = 2017)

complete_2018 <- complete %>%
  # na.omit() %>%
  mutate(Season = 2018)

complete_2018_finals <- complete %>%
  # na.omit() %>%
  mutate(Season = 2018)

complete_2019 <- complete %>%
  # na.omit() %>%
  mutate(Season = 2019)

complete_2019_finals <- complete %>%
  # na.omit() %>%
  mutate(Season = 2019)

complete_2020 <- complete %>%             
  # na.omit() %>%
  mutate(Season = 2020)

# complete_2020_finals <- complete %>%
#   # na.omit() %>%
#   mutate(Season = 2020)

# join the separate dataframes together 
full_stats <- bind_rows(complete_2017, complete_2017_finals, complete_2018, complete_2018_finals, complete_2019, complete_2019_finals, complete_2020)

# write the full set to a csv
write.csv(full_stats, "Super Netball Stats.csv")
