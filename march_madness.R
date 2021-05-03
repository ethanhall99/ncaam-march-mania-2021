library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)

# Global Variable
minSeason <- 2021
chosenRankingSystems <- c("NET", "TRP", "POM")


# Create list of file paths for CSVs
base_path <- "/Users/ethanhall/Desktop/Data/ncaam-march-mania-2021/MDataFiles_Stage2"
base_path_len <- nchar(base_path)
files <- list.files(path = base_path, pattern = "*.csv", full.names = TRUE)

# Read all CSVs into data frames
for (i in files) {
  filename <- substr(i, base_path_len+2, nchar(i))
  name <- substr(filename, 1, nchar(filename)-4)
  assign(name, data.frame(read_csv(i, col_names = TRUE)))
} 


# Filter seasons
fMRegularSeasonDetailedResults <- MRegularSeasonDetailedResults %>%
  filter(Season >= minSeason)


fMTeamConferences <- MTeamConferences %>%
  filter(Season >= minSeason)


# Join team and conference table
TC <- left_join(fMTeamConferences, Conferences, by = c("ConfAbbrev" = "ConfAbbrev"))
TeamsConferences <- left_join(TC, MTeams, by = c("TeamID" = "TeamID")) %>%
      select(TeamID, TeamName, Description, PowerFive)


# Distinct Ranking Systems in chosen years
dRankingSystems <- MMasseyOrdinals %>%
  filter(Season >= minSeason) %>%
  distinct(SystemName)


# Filter to chosen ranking systems
RankingSystems <- MMasseyOrdinals %>%
  filter(SystemName %in% chosenRankingSystems,
         Season >= minSeason) %>%
  select(-Season) %>%
  mutate(weekRank = dense_rank(desc(-RankingDayNum)))

FinalRankings <- RankingSystems %>%
  filter(weekRank == max(weekRank)) %>%
  select(-one_of(c("weekRank", "RankingDayNum"))) %>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)


gameCols <- c("Season", "DayNum", "TeamID", "WinLoss", "PF", "PA", "NumOT", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", 
              "OR", "DR", "Ast", "TO", "Stl", "Blk", "Fouls")

# game stats for winning team
gameWinner <- fMRegularSeasonDetailedResults %>%
  select("Season", "DayNum", "WTeamID", "WScore", "LScore", "NumOT", "WFGM", "WFGA", "WFGM3", "WFGA3",
         "WFTM", "WFTA", "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF") %>%
  add_column(WinLoss = 1, .before = "WScore")
colnames(gameWinner) <- gameCols

# game stats for losing team
gameLoser <- fMRegularSeasonDetailedResults %>%
  select("Season", "DayNum", "LTeamID", "LScore", "WScore", "NumOT", "LFGM", "LFGA", "LFGM3", "LFGA3",
         "LFTM", "LFTA", "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF") %>%
  add_column(WinLoss = 0, .before = "LScore")
colnames(gameLoser) <- gameCols

# Combine Win and Loss
games <- bind_rows(gameWinner, gameLoser)

gameDetails <- left_join(TeamsConferences, games, by = c("TeamID" = "TeamID"))

seasonStats2021 <- gameDetails %>%
  group_by(TeamID, TeamName, Description, PowerFive) %>%
  summarise(Wins = sum(WinLoss == 1),
            Loss = sum(WinLoss == 0),
            PF = mean(PF),
            PA = mean(PA),
            FGPct = sum(FGM)/sum(FGA)*100,
            FGM = sum(FGM),
            FGA = sum(FGA),
            FG3Pct = sum(FGM3)/sum(FGA3)*100,
            FGM3 = sum(FGM3),
            FGA3 = sum(FGA3),
            FTPct = sum(FTM)/sum(FTA)*100,
            FTM = sum(FTM),
            FTA = sum(FTA),
            Reb = mean(OR)+mean(DR),
            OReb = mean(OR),
            DReb = mean(DR),
            Ast = mean(Ast),
            TO = mean(TO),
            Stl = mean(Stl),
            Blk = mean(Blk))

TeamStatsNRankings <- left_join(seasonStats2021, FinalRankings, by = c("TeamID" = "TeamID"))

# write.table(TeamStatsNRankings, file = "/Users/ethanhall/Desktop/Data/ncaam-march-mania-2021/teamStatsRankings.csv",
#            row.names = FALSE, sep = ",")

