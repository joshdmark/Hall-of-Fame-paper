############################################################################################
library(ballr)
library(tidyverse)
library(ggthemes)
melo_stats <- NBAPlayerPerGameStats('/players/a/anthoca01.html') %>% data.frame()
melo <- melo_stats[c(1:8, 11:17), ] %>%  # exclude HOU (10 games)
    mutate(year = 1:nrow(.), 
           tm = ifelse(tm == 'TOT', 'DEN/NYK (Trade)', tm))

### melo scoring graph
season_vector <- melo$season
melo <- melo %>% mutate(season = factor(season, levels = season_vector))
melo %>% 
  rename(Team = tm) %>%  
  ggplot() + 
  geom_point(aes(season, pts)) + 
  geom_line(aes(year, pts)) + 
  geom_label(aes(year,pts, label = pts, fill = Team), size = 3.5, col = 'white') + 
  scale_fill_manual(values = c("skyblue", "lightgrey", "blue", "#ff7f00")) + 
  theme_fivethirtyeight() +
  theme(legend.position = 'bottom') + 
  annotate("text", label = 'First Team \n All-Rookie \n 03-04', x = 1, y = 19.4, size = 3) + 
  annotate("text", label = "2013 NBA Scoring \n Champion (28.7 ppg)", x = 11, y = 30, size = 3.2, colour = "black") + 
  labs(title = 'Carmelo Anthony PPG', 
       subtitle = '2018-19 HOU Season excluded (10 games)') 


# /players/c/cartevi01.html ## vince carter
vc_stats <- NBAPlayerPerGameStats('/players/c/cartevi01.html') %>% data.frame()
vc_career <- vc_stats %>% filter(season == 'Career') %>% 
  mutate(player = 'Vince Carter')

# /players/p/piercpa01.html ## paul pierce
pp_stats <- NBAPlayerPerGameStats('/players/p/piercpa01.html') %>% data.frame()
pp_career <- pp_stats %>% filter(season == 'Career') %>% 
  mutate(player = 'Paul Pierce')

# /players/m/mcgratr01.html ## tracy mcgrady
tm_stats <- NBAPlayerPerGameStats('/players/m/mcgratr01.html') %>% data.frame()
tm_career <- tm_stats %>% filter(season == 'Career') %>% 
  mutate(player = 'Tracy McGrady')

# melo career
melo_career <- NBAPlayerPerGameStats('/players/a/anthoca01.html') %>% data.frame() %>% 
  filter(season == 'Career') %>% 
  mutate(player = 'Carmelo Anthony')

## compare all careers
careers <- rbind(melo_career, vc_career, pp_career, tm_career)
graph_data <- careers %>% 
  mutate(ftpercent = ftpercent * 100) %>% 
  select(player, PPG = pts, `FT%` = ftpercent, RPG = trb, MP = mp) %>% 
  gather(key = 'stat', value = 'value', -player)
graph_data %>% 
  ggplot() + 
  geom_col(aes(x = player, y = value, fill = player), alpha = .9) + 
  geom_label(aes(player, 1, label = value), size = 4, col = 'black') + 
  scale_fill_manual(values = c('darkblue', 'darkgreen', 'darkred', 'purple')) + 
  facet_wrap(.~stat, scales = 'free') + 
  labs(fill = 'Player', title = 'Carmelo Anthony vs HOF Candidates/Comparisons') + 
  theme_fivethirtyeight() + 
  theme(legend.position = 'bottom')

######## KD SHOT DATA
library(rjson)
library(tidyverse)
library(grid)
library(jpeg)
library(RCurl)
library(gridExtra)

shotData <- fromJSON(file = shotURL, method="C")
# unlist shot data, save into a data frame
fga <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# raw_data <- getURL("http://api.crunchbase.com/v/1/companies.js")

seasons <- c('2016-17', '2017-18', '2018-19')
all_shots <- data.frame()
for (season_id in seasons){
    # print(season)
    season <- season_id 
    playerID <- 201142 # KD
    shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=", 
                     season, 
                     "&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",
                     playerID,
                     "&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=", 
                     season, 
                     "&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
    shotData <- fromJSON(file = shotURL, method="C")
    # unlist shot data, save into a data frame
    fga <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))
    # fix column names
    # colnames(fga) <- shotData$resultSets[[1]][[2]] %>% data.frame()
    fga$season_nbr <- season
    all_shots <- bind_rows(all_shots, fga)
}

rm_season <- all_shots %>% select(-season_nbr)
season_col <- all_shots %>% select(season_nbr)

proper_names <- shotData$resultSets[[1]][[2]] %>% as.vector()
colnames(rm_season) <- proper_names

all_shots <- cbind(rm_season, season_col)
# rm(rm_season, season_col)

all_shots$LOC_X <- as.numeric(as.character(all_shots$LOC_X))
all_shots$LOC_Y <- as.numeric(as.character(all_shots$LOC_Y))
all_shots$SHOT_DISTANCE <- as.numeric(as.character(all_shots$SHOT_DISTANCE))

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# simple plot using EVENT_TYPE to colour the dots
ggplot(all_shots, aes(x=LOC_X, y=LOC_Y)) +
    annotation_custom(court, -250, 250, -50, 420) +
    geom_point(aes(colour = EVENT_TYPE, shape = SHOT_TYPE))


library(rjson)
library(jsonlite)
playerID <- 201142 # KD
shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID="
                 ,playerID
                 ,"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0"
                 , sep = "")
# import from JSON
shotData <- jsonlite::fromJSON(shotURL)
shotData <- rjson::fromJSON(file = shotURL, method="C")
shotData <- rjson::fromJSON(file = 'http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=201142&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0', method="C")

# unlist shot data, save into a data frame
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# shot data headers
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))


# half court image 
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg" 
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)), 
                    width=unit(1,"npc"), height=unit(1,"npc")) 

# simple plot using EVENT_TYPE to colour the dots 
ggplot(melo_2013, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) + 
  geom_hex(aes(fill = EVENT_TYPE), alpha = .7, bins = 30)  

# simple plot using EVENT_TYPE to colour the dots 
melo_2013 %>%  
  filter(GAME_DATE == 20130402) %>%  
  ggplot() +  
  geom_hex(aes(x=LOC_X, y=LOC_Y, fill = EVENT_TYPE), alpha = .7) +  
  annotation_custom(court, -250, 250, -50, 420) 

outburst <- melo_2013 %>% filter(GAME_DATE == 20130402) 

ggplot(outburst, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) + 
  geom_point(aes(col = EVENT_TYPE), alpha = .7, size = 3) 

# 
# library(tidyverse)
# 
# shots <- fread('C:/Users/jdmark/Downloads/playoff_shots.csv')
# # 
# # shots %>% 
# #     filter(PLAYER_NAME == 'Anthony Davis') %>% 
# #     ggplot(aes(LOC_X, LOC_Y, col = as.character(SHOT_MADE_FLAG))) + 
# #     geom_point() + 
# #     labs(col = "")
# 
# ## https://www.kaggle.com/dansbecker/nba-shot-logs#shot_logs.csv
# ## 2014-2015 season data 
# shots <- fread('C:/Users/jdmark/Downloads/shot_logs.csv')
# shots <- shots %>% 
#     mutate(game_month = substr(MATCHUP, 1, 3), 
#            game_day = substr(MATCHUP, 5, 6), 
#            game_year = substr(MATCHUP, 9, 12),
#            game_mo = case_when(
#                game_month == 'OCT' ~ '10',
#                game_month == 'NOV' ~ '11',
#                game_month == 'DEC' ~ '12',
#                game_month == 'JAN' ~ '01',
#                game_month == 'FEB' ~ '02',
#                game_month == 'MAR' ~ '03',
#            ), 
#            game_mo = str_pad(game_mo, 2, 'left'), 
#            game_dt = paste(game_year, game_mo, game_day, sep = '-'), 
#            game_dt = ymd(game_dt), 
#            game_dow = weekdays(game_dt),
#            away_team = substr(MATCHUP, 15, 18), 
#            home_team = substr(MATCHUP, 22, 25)) 
# shots %>% filter(home_team %in% c('LAL', 'LAC') & game_dow == 'Sunday') %>% count(game_year)
# 
# 
# ###############
# ## play by play data
# ## https://eightthirtyfour.com/data
# pbp <- fread('C:/Users/jdmark/Downloads/2015-16_pbp.csv')
# 
# 
# ## ncaahoopR
# library(ncaahoopR)
# schedule <- get_schedule('Louisville')
# game_ids <- schedule$game_id
# get_shot_locs('401082798') %>% 
#     ggplot() + 
#     geom_hex(aes(x,y,fill=team_name))

# xx <- read_rds('//S:/Apollo/melo_2013.RDS')
