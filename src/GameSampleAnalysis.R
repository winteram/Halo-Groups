# setwd("/Users/winteram/Documents/Research/Halo-Groups/src")
# require(timeDate)
require(ggplot2)

recreate.data <- FALSE

if(recreate.data)
{
  # To create processed data
  games_crawl <- read.csv("game_sample.tsv", skip=1, header=FALSE, stringsAsFactors=FALSE)
  games <- games_crawl[,1:38]
  names(games) <- c("PlayerDataIndex","gamertag","service_tag","first_active","last_active","games_total","GameId","GameVariantName","GameVariantClass","MapName","GameTimestamp","IsTeamGame","PlayerCount","Rating","Standing","Score","Team","TeamStanding","TeamScore","Kills","Deaths","Assists","Betrayals","Headshots","Suicides","AvgKillDistanceMeters","KilledMostCount","PlayerKilledByMost","KilledMostByCount","TotalMedalCount","UniqueTotalMedalCount","StyleMedalCount","UniqueStyleMedalCount","SpreeMedalCount","UniqueSpreeMedalCount","MultiMedalCount","UniqueMultiMedalCount","OtherMedalCount")
  games$first_active <- as.integer(substr(games[,"first_active"], 7, 16))
  games$last_active <- as.integer(substr(games[,"last_active"], 7, 16))
  games$GameTimestamp <- as.integer(substr(games[,"GameTimestamp"], 7, 16))
  games <- subset(games, !is.na(GameTimestamp))
  games$kdratio <- 0
  games[games$Deaths>0,]$kdratio <- games[games$Deaths>0,]$Kills / games[games$Deaths>0,]$Deaths
}
else
{
  # To load processed data
  load('../data/crawl_games.Rdata')
  games <- all_games
  rm(all_games)
}




####  GENERAL GAME PLAY STATISTICS  ####

print(paste('Number of unique games: ',length(unique(games$GameId))))
print(paste('Number of unique players: ',length(unique(games$gamertag))))

# distribution of games played
player.freq <- as.data.frame(table(as.factor(games$gamertag)))
player.k.freq <- as.data.frame(table(as.factor(player.freq$Freq)))
names(player.k.freq) <- c("games_played","Freq")
player.k.freq$games_played <- as.integer(player.k.freq$games_played)
player.k.freq$PDF <- player.k.freq$Freq / sum(player.k.freq$Freq)
player.k.freq$CCDF <- player.k.freq$PDF
for(i in 2:nrow(player.k.freq))
{
	player.k.freq[i,]$CCDF <- player.k.freq[i-1,]$CCDF + player.k.freq[i,]$CCDF
}
rm(i)
player.k.freq$CCDF <- 1 - player.k.freq$CCDF

ggplot(player.k.freq, aes(x=games_played, y=PDF)) + geom_point() + scale_x_log10() + xlab("Games Played")
ggsave("../fig/GamesPlayedPDF.png",width=5,height=5)
ggplot(player.k.freq, aes(x=games_played, y=CCDF)) + geom_point() + scale_x_log10()  + scale_y_log10() + geom_smooth(method="lm", se=FALSE) + xlab("Games Played")
ggsave("../fig/GamesPlayedCCDF.png",width=5,height=5)


# histogram of game sizes for crawled games
player.teamsizes <- with(games, table(gamertag, PlayerCount))
teamsizes <- as.data.frame(margin.table(player.teamsizes,2) / 1:16)
ggplot(teamsizes) + geom_histogram(aes(x=Var1,y=Freq)) + labs(x="Players in Game",y="Frequency")
ggsave("../fig/hist_game_sizes.png",width=5,height=5)




#####  FINDING FRIENDS #####

# Create weighted edgelist for all players
player.games <- subset(games, select=c("GameId","gamertag"))
edgelist <- merge(player.games, player.games, by="GameId", all=TRUE)
edgelist <- subset(edgelist, gamertag.x!=gamertag.y)
edgelist.wtd <- table(edgelist[,2:3])

# Distribution of games played with other people for Arrow of Doubt
games.arrow <- unique(games[games$gamertag=="Arrow of Doubt","GameId"])
games.arrow.self <- games[games$GameId %in% games.arrow,]
arrow.coplayers <- as.data.frame(table(subset(games.self, gamertag!="Arrow of Doubt")$gamertag))
arrow.coplay.tbl <- table(subset(arrow.coplayers, arrow.coplayers$Freq>0))
arrow.coplay.dist <- as.data.frame(margin.table(arrow.coplay.tbl, 2))
names(arrow.coplay.dist) <- c("games_played","Freq")
arrow.coplay.dist$games_played <- as.numeric(as.character(arrow.coplay.dist$games_played))
arrow.coplay.dist$Freq <- as.numeric(as.character(arrow.coplay.dist$Freq))
ggplot(subset(arrow.coplay.dist, games_played>2), aes(x=games_played, y=Freq)) + geom_point() + labs(x="Games Played", y="Number of Co-Players")
ggsave("../fig/ArrowCoplayerDist_gt2.png", width=5,height=5)
arrow.friends <- subset(arrow.coplayers, Freq>4)


# Distribution of games played with other people for HiDuuKeN (1k games)
games.HiDuuKeN <- unique(games[games$gamertag=="HiDuuKeN","GameId"])
games.HiDuuKeN.self <- games[games$GameId %in% games.HiDuuKeN,]
HiDuuKeN.coplayers <- as.data.frame(table(subset(games.HiDuuKeN.self, gamertag!="HiDuuKeN")$gamertag))
HiDuuKeN.coplay.tbl <- table(subset(HiDuuKeN.coplayers, HiDuuKeN.coplayers$Freq>0))
HiDuuKeN.coplay.dist <- as.data.frame(margin.table(HiDuuKeN.coplay.tbl, 2))
names(HiDuuKeN.coplay.dist) <- c("games_played","Freq")
HiDuuKeN.coplay.dist$games_played <- as.numeric(as.character(HiDuuKeN.coplay.dist$games_played))
HiDuuKeN.coplay.dist$Freq <- as.numeric(as.character(HiDuuKeN.coplay.dist$Freq))
ggplot(subset(HiDuuKeN.coplay.dist, games_played>2), aes(x=games_played, y=Freq)) + geom_point() + labs(x="Games Played", y="Number of Co-Players")
ggsave("../fig/HiDuuKeNCoplayerDist_gt2.png", width=5,height=5)
HiDuuKeN.friends <- subset(HiDuuKeN.coplayers, Freq>20)

# Distribution of games played with other people for BlackAngel97 (most games played = 2983)
games.BlackAngel97 <- unique(games[games$gamertag=="BlackAngel97","GameId"])
games.BlackAngel97.self <- games[games$GameId %in% games.BlackAngel97,]
BlackAngel97.coplayers <- as.data.frame(table(subset(games.BlackAngel97.self, gamertag!="BlackAngel97")$gamertag))
BlackAngel97.coplay.tbl <- table(subset(BlackAngel97.coplayers, BlackAngel97.coplayers$Freq>0))
BlackAngel97.coplay.dist <- as.data.frame(margin.table(BlackAngel97.coplay.tbl, 2))
names(BlackAngel97.coplay.dist) <- c("games_played","Freq")
BlackAngel97.coplay.dist$games_played <- as.numeric(as.character(BlackAngel97.coplay.dist$games_played))
BlackAngel97.coplay.dist$Freq <- as.numeric(as.character(BlackAngel97.coplay.dist$Freq))
ggplot(BlackAngel97.coplay.dist, aes(x=games_played, y=Freq)) + geom_point() + labs(x="Games Played", y="Number of Co-Players")
ggsave("../fig/BlackAngel97CoplayerDist.png", width=5,height=5)
BlackAngel97.friends <- subset(BlackAngel97.coplayers, Freq>20)


####  Learning  ####

solo.games <- subset(games, GameVariantName=="Campaign: Solo")
solo.by.plyr.map <- ddply(solo.games, .(gamertag, MapName), transform, order=rank(GameId))
solo.by.plyr.map <- transform(solo.by.plyr.map, kd.diff=Kills-Deaths)
solo.learning <- ddply(solo.by.plyr.map, .(order, MapName), summarize, kills=mean(Kills), kills.sd=sd(Kills), deaths=mean(Deaths), deaths.sd=sd(Deaths), headshots=mean(Headshots), headshots.sd=sd(Headshots), medals=mean(TotalMedalCount), medals.sd=sd(TotalMedalCount), kd.diff=mean(kd.diff), kd.diff.sd=sd(kd.diff), n=length(gamertag))

solo.learning$order <- as.numeric(as.character(solo.learning$order))
solo.learning$MapName <- as.factor(solo.learning$MapName)

ggplot(subset(solo.learning, MapName=="Winter Contingency" & order>0), aes(x=order,y=n)) + geom_point() +scale_y_log10()+labs(x="Number of Times Played", y="Number of Players")
ggsave("../fig/winter_contingency_times_played.png", width=5,height=5)


ggplot(subset(solo.learning, n>1 & MapName=="Winter Contingency")) + geom_line(aes(x=order,y=kills)) + geom_errorbar(aes(x=order, ymin=kills - (kills.sd/sqrt(n)),ymax=kills + (kills.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Kills")
ggsave("../fig/winter_contingency_kills_learning.png", width=10,height=5)

ggplot(subset(solo.learning, n>1 & MapName=="Winter Contingency")) + geom_line(aes(x=order,y=deaths)) + geom_errorbar(aes(x=order, ymin=deaths - (deaths.sd/sqrt(n)),ymax=deaths + (deaths.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Deaths")
ggsave("../fig/winter_contingency_deaths_learning.png", width=10,height=5)

ggplot(subset(solo.learning, n>1 & MapName=="Winter Contingency")) + geom_line(aes(x=order,y=kd.diff)) + geom_errorbar(aes(x=order, ymin=kd.diff - (kd.diff.sd/sqrt(n)),ymax=kd.diff + (kd.diff.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Kills - Deaths")
ggsave("../fig/winter_contingency_kd_learning.png", width=10,height=5)



####  Success  ####

# homophily of success? correlation in kdratio between teammates
