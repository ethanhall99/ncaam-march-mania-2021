library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(sjmisc)

# Global Variable
minSeason <- 2021



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
# select systems with rankings every week
fullRankings2021 <- MMasseyOrdinals %>%
  filter(Season >= minSeason) %>%
  select(-Season) %>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank) %>%
  select_if(~ !any(is.na(.)))

# get system names
fullRankingSystems <- colnames(fullRankings2021)
fullRankingSystems <- fullRankingSystems[-c(1, 2)] 
print(fullRankingSystems)

# Ranking System Options
#chosenRankingSystems <- c("NET", "TRP", "POM")
chosenRankingSystems <- fullRankingSystems

RankingSystems <- MMasseyOrdinals %>%
  filter(SystemName %in% chosenRankingSystems,
         Season >= minSeason,
         RankingDayNum < max(RankingDayNum)) %>%
  select(-Season) %>%
  mutate(RankingWeekNum = ceiling(RankingDayNum/7)) %>%
  move_columns(RankingWeekNum, .before = RankingDayNum) %>%
  select(-"RankingDayNum") %>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)


# Final Rankings
FinalRankings <- MMasseyOrdinals %>%
  filter(SystemName %in% chosenRankingSystems,
         Season >= minSeason) %>%
  select(-Season) %>%
  mutate(weekRank = dense_rank(desc(-RankingDayNum)))

FinalRankings <- FinalRankings %>%
  filter(weekRank == max(weekRank)) %>%
  select(-one_of(c("weekRank", "RankingDayNum"))) %>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)


# final game columns
gameCols <- c("Season", "DayNum", "TeamID", "OppTeamID", "WinLoss", "PF", "PA", "NumOT", "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA", 
              "OR", "DR", "Ast", "TO", "Stl", "Blk", "Fouls")

# game stats for winning team
gameWinner <- fMRegularSeasonDetailedResults %>%
  select("Season", "DayNum", "WTeamID", "LTeamID", "WScore", "LScore", "NumOT", "WFGM", "WFGA", "WFGM3", "WFGA3",
         "WFTM", "WFTA", "WOR", "WDR", "WAst", "WTO", "WStl", "WBlk", "WPF") %>%
  add_column(WinLoss = 1, .before = "WScore")
colnames(gameWinner) <- gameCols

# game stats for losing team
gameLoser <- fMRegularSeasonDetailedResults %>%
  select("Season", "DayNum", "LTeamID", "WTeamID", "LScore", "WScore", "NumOT", "LFGM", "LFGA", "LFGM3", "LFGA3",
         "LFTM", "LFTA", "LOR", "LDR", "LAst", "LTO", "LStl", "LBlk", "LPF") %>%
  add_column(WinLoss = 0, .before = "LScore")
colnames(gameLoser) <- gameCols

# Combine Win and Loss
games <- bind_rows(gameWinner, gameLoser)

gameDetails <- left_join(TeamsConferences, games, by = c("TeamID" = "TeamID")) %>%
  mutate(WeekNum = ceiling(DayNum/7)) %>%
  move_columns(WeekNum, .before = DayNum)

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

# Cross Join
cross_Teams <- TeamStatsNRankings
colnames(cross_Teams) <- paste("Opp", colnames(cross_Teams), sep = "")

drop_cols <- c('TeamName', 'Description', 'OppTeamName', 'OppDescription')

allPossibleMatchups <- merge(TeamStatsNRankings, cross_Teams, all=TRUE)
allPossibleMatchups <- allPossibleMatchups %>%
  filter(TeamID != OppTeamID) %>%
  select(-drop_cols)
  



# aggregate stats from games played prior to that week
x <- min(gameDetails$WeekNum)
weekly_stats <- data.frame()
while (x <= max(gameDetails$WeekNum)) {
  holder <- gameDetails %>%
    filter(WeekNum <= x) %>%
    group_by(TeamID, TeamName, Description, PowerFive, Season) %>%
    summarise(WeekNum = x,
              StatsWeekNum = x+1,
              GP = n(),
              PF = mean(PF),
              PA = mean(PA),
              FGPct = sum(FGM)/sum(FGA)*100,
              FGM = mean(FGM),
              FGA = mean(FGA),
              FG3Pct = sum(FGM3)/sum(FGA3)*100,
              FGM3 = mean(FGM3),
              FGA3 = mean(FGA3),
              FTPct = sum(FTM)/sum(FTA)*100,
              FTM = mean(FTM),
              FTA = mean(FTA),
              Reb = mean(OR)+mean(DR),
              OReb = mean(OR),
              DReb = mean(DR),
              Ast = mean(Ast),
              TO = mean(TO),
              Stl = mean(Stl),
              Blk = mean(Blk))
  weekly_stats <- bind_rows(weekly_stats, holder)
  x = x + 1
}

weekly_stats <- weekly_stats %>%
  select(-c('TeamName', 'Description', 'WeekNum', 'Season'))

opp_weekly_stats <- weekly_stats
colnames(opp_weekly_stats) <- paste("Opp", colnames(opp_weekly_stats), sep = "")
  



# know matchups
matchups <- gameDetails %>%
  select(WinLoss, WeekNum, TeamID, OppTeamID)
  
# Join winning team data
matchups <- inner_join(matchups, weekly_stats, by = c("TeamID" = "TeamID", "WeekNum" = "StatsWeekNum"))
matchups <- inner_join(matchups, opp_weekly_stats, by = c("OppTeamID" = "OppTeamID", "WeekNum" = "OppStatsWeekNum"))





