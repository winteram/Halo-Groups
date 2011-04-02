# setwd("/Users/winteram/Documents/Research/Halo-Groups/src")
# require(timeDate)
require(ggplot2)
require(SparseM)
require(igraph)
theme_set(theme_bw())

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
ggplot(player.k.freq, aes(x=games_played, y=CCDF)) + geom_point() + scale_x_log10()  + scale_y_log10() + geom_smooth(method="lm", se=FALSE)+ xlab("Games Played")
ggsave("../fig/GamesPlayedCCDF.png",width=5,height=5)
ggplot(player.k.freq, aes(x=games_played, y=PDF)) + geom_point() + scale_x_log10()  + scale_y_log10() + geom_smooth(method="lm", se=FALSE)+ xlab("Games Played")
ggsave("../fig/GamesPlayedPDF.png",width=5,height=5)


# distribution of team games played
player.team.freq <- as.data.frame(table(as.factor(subset(games, PlayerCount>1)$gamertag)))
player.team.k.freq <- as.data.frame(table(as.factor(player.team.freq$Freq)))
names(player.team.k.freq) <- c("games_played","Freq")
player.team.k.freq$games_played <- as.integer(player.team.k.freq$games_played)
player.team.k.freq$PDF <- player.team.k.freq$Freq / sum(player.team.k.freq$Freq)
player.team.k.freq$CCDF <- player.team.k.freq$PDF
for(i in 2:nrow(player.team.k.freq))
{
  player.team.k.freq[i,]$CCDF <- player.team.k.freq[i-1,]$CCDF + player.team.k.freq[i,]$CCDF
}
player.team.k.freq$CCDF <- 1 - player.team.k.freq$CCDF
ggplot(player.team.k.freq, aes(x=games_played, y=CCDF)) + geom_point() + scale_x_log10()  + scale_y_log10() + geom_smooth(method="lm", se=FALSE)+ xlab("Games Played")
ggsave("../fig/TeamGamesPlayedCCDF.png",width=5,height=5)
ggplot(player.team.k.freq, aes(x=games_played, y=PDF)) + geom_point() + scale_x_log10()  + scale_y_log10() + geom_smooth(method="lm", se=FALSE)+ xlab("Games Played")
ggsave("../fig/TeamGamesPlayedPDF.png",width=5,height=5)


# histogram of game sizes for crawled games
player.teamsizes <- with(games, table(gamertag, PlayerCount))
teamsizes <- as.data.frame(margin.table(player.teamsizes,2) / 1:16)
ggplot(teamsizes) + geom_histogram(aes(x=Var1,y=Freq)) + labs(x="Players in Game",y="Frequency")
ggsave("../fig/hist_game_sizes.png",width=5,height=5)




#####  FINDING FRIENDS #####

# Create weighted edgelist for all players
player.games <- games[games$gamertag %in% player.freq[player.freq$Freq>1,"Var1"],c("gamertag","GameId")]
edgelist <- merge(player.games, player.games, by="GameId", all=TRUE)
edgelist <- subset(edgelist, gamertag.x!=gamertag.y)
edgelist <- unique(edgelist)
duplicate.dyads <- edgelist[duplicated(edgelist[,2:3]),]
nonunique.dyads <- edgelist[paste(edgelist$gamertag.x,edgelist$gamertag.y,sep="::") %in% paste(duplicate.dyads$gamertag.x,duplicate.dyads$gamertag.y,sep="::"),]
nonunique.dyads <- nonunique.dyads[with(nonunique.dyads, order(gamertag.x, gamertag.y)),]
nonunique.dyads <- unique(nonunique.dyads)
nonunique.dyads$runs <- sequence(rle(paste(nonunique.dyads$gamertag.x,nonunique.dyads$gamertag.y,sep="::"))$lengths)
nonunique.dyads <- nonunique.dyads[with(nonunique.dyads, order(gamertag.x, gamertag.y, -runs)),]
nonunique.dyads$ID <- paste(nonunique.dyads$gamertag.x,nonunique.dyads$gamertag.y,sep="::")
edgelist.wtd <- nonunique.dyads[c(TRUE, nonunique.dyads$ID[-1] != nonunique.dyads$ID[-length(nonunique.dyads$ID)]), ]
edgelist.wtd <- subset(edgelist.wtd, select=c("gamertag.x","gamertag.y","runs"))
names(edgelist.wtd) <- c("gamertag.x","gamertag.y","wt")
edgelist.wtd <- merge(edgelist.wtd, player.freq, by.x="gamertag.x", by.y="Var1", all.x=TRUE)
edgelist.wtd$pwt <- edgelist.wtd$wt / edgelist.wtd$Freq

# define true friends, for comparison
# AngryKnife <- read.delim('../data/trueFriends/AngryKnife.true.txt', header=FALSE, col.names=c("gamertag.y","wt"))
# AngryKnife$gamertag.x <- "AngryKnife"
Arrow_of_Doubt <- read.delim('../data/trueFriends/Arrow_of_Doubt.true.txt', header=FALSE, col.names=c("gamertag.y","wt"))
Arrow_of_Doubt$gamertag.x <- "Arrow of Doubt"
FearfulSpoon <- read.delim('../data/trueFriends/FearfulSpoon.true.txt', header=FALSE, col.names=c("gamertag.y","wt"))
FearfulSpoon$gamertag.x <- "FearfulSpoon"
Stilted_Fox <- read.delim('../data/trueFriends/Stilted_Fox.true.txt', header=FALSE, col.names=c("gamertag.y","wt"))
Stilted_Fox$gamertag.x <- "Stilted Fox"
tellkeeper <- read.delim('../data/trueFriends/tellkeeper.true.txt', header=FALSE, col.names=c("gamertag.y","wt"))
tellkeeper$gamertag.x <- "tellkeeper"

true.friends <- rbind(Arrow_of_Doubt, FearfulSpoon, Stilted_Fox, tellkeeper)
rm(Arrow_of_Doubt, FearfulSpoon, Stilted_Fox, tellkeeper)

# for absolute thresholds
G <- graph.edgelist(as.matrix(subset(edgelist.wtd, select=c("gamertag.x","gamertag.y"))), directed=FALSE)
cc <- clusters(G)
dia <- diameter(G)
V <- vcount(G)
E <- ecount(G)
abs.thresh <- as.data.frame(list(1,V,E,cc$no,cc$csize[1],dia))
names(abs.thresh) <- c("threshold","V","E","cc.no","gc.size","gc.dia")

abs.friends <- data.frame()
for(name in unique(true.friends$gamertag.x))
{
    abs.friends <- merge(abs.friends,
    data.frame(1,
      name,
      length(intersect(edgelist.wtd[edgelist.wtd$gamertag.x==name,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
      length(union(edgelist.wtd[edgelist.wtd$gamertag.x==name,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
      length(edgelist.wtd[edgelist.wtd$gamertag.x==name,"gamertag.y"]),
      length(true.friends[true.friends$gamertag.x==name,"gamertag.y"]))
    )
}
names(abs.friends) <- c("threshold","gamertag.x","intsx","union","edge","true")

for(i in 2:9)
{
  # create graph from edgelist
  G <- graph.edgelist(as.matrix(subset(edgelist.wtd, wt>2^i,select=c("gamertag.x","gamertag.y"))), directed=FALSE)
  cc <- clusters(G)
  dia <- diameter(G)
  V <- vcount(G)
  E <- ecount(G)
  abs.thresh <- rbind(abs.thresh,list(i,V,E,cc$no,cc$csize[1],dia))
  for(name in unique(true.friends$gamertag.x))
  {
      tmp <- data.frame(i,
        name,
        length(intersect(edgelist.wtd[edgelist.wtd$gamertag.x==name & edgelist.wtd$wt>2^i,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
        length(union(edgelist.wtd[edgelist.wtd$gamertag.x==name & edgelist.wtd$wt>2^i,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
        length(edgelist.wtd[edgelist.wtd$gamertag.x==name & edgelist.wtd$wt>2^i,"gamertag.y"]),
        length(true.friends[true.friends$gamertag.x==name,"gamertag.y"])
      )
      names(tmp) <- c("threshold","gamertag.x","intsx","union","edge","true")
      abs.friends <- rbind(abs.friends, tmp)
  }
}
abs.thresh$threshold <- 2^(0:9)
abs.friends$threshold <- sort(2^rep(0:7,4))
abs.friends$jaccard <- abs.friends$intsx / abs.friends$union
abs.friends$precision <- 0
abs.friends[abs.friends$edge>0,]$precision <- abs.friends[abs.friends$edge>0,]$intsx / abs.friends[abs.friends$edge>0,]$edge
abs.friends$recall <- 0
abs.friends[abs.friends$true>0,]$recall <- abs.friends[abs.friends$true>0,]$intsx / abs.friends[abs.friends$true>0,]$true


ggplot(abs.thresh) + geom_line(aes(x=threshold,y=V)) + scale_x_log2() + scale_y_log10() + labs(x="Threshold",y="Number of Nodes")
ggsave("../fig/abs_thresh_V.png", width=5,height=5)
ggplot(abs.thresh) + geom_line(aes(x=threshold,y=E)) + scale_x_log2() + scale_y_log10() + labs(x="Threshold",y="Number of Edges")
ggsave("../fig/abs_thresh_E.png", width=5,height=5)
ggplot(abs.thresh) + geom_line(aes(x=threshold,y=E/V)) + scale_x_log2() + labs(x="Threshold",y="Average Degree")
ggsave("../fig/abs_thresh_deg.png", width=5,height=5)
ggplot(abs.thresh) + geom_line(aes(x=threshold,y=cc.no)) + scale_x_log2() + scale_y_log10() + labs(x="Threshold",y="Number of Connected Components")
ggsave("../fig/abs_thresh_cc.png", width=5,height=5)
ggplot(abs.thresh) + geom_line(aes(x=threshold,y=gc.size)) + scale_x_log2() + scale_y_log10() + labs(x="Threshold",y="Size of Largest Component")
ggsave("../fig/abs_thresh_gcsize.png", width=5,height=5)
ggplot(abs.thresh) + geom_line(aes(x=threshold,y=gc.dia)) + scale_x_log2() + labs(x="Threshold",y="Diameter of Largest Component")
ggsave("../fig/abs_thresh_gcdia.png", width=5,height=5)

ggplot(abs.friends) + geom_line(aes(x=threshold,y=jaccard,color=gamertag.x)) + scale_x_log2() + opts(legend.position=c(0.8,0.8))
ggsave("../fig/abs_thresh_jaccard.png", width=5,height=5)
ggplot(abs.friends) + geom_line(aes(x=threshold,y=precision,color=gamertag.x)) + scale_x_log2() + opts(legend.position=c(0.3,0.3))
ggsave("../fig/abs_thresh_precision.png", width=5,height=5)
ggplot(abs.friends) + geom_line(aes(x=threshold,y=recall,color=gamertag.x)) + scale_x_log2() + opts(legend.position=c(0.8,0.8))
ggsave("../fig/abs_thresh_recall.png", width=5,height=5)



# for relative thresholds
G <- graph.edgelist(as.matrix(subset(edgelist.wtd, select=c("gamertag.x","gamertag.y"))), directed=FALSE)
cc <- clusters(G)
dia <- diameter(G)
V <- vcount(G)
E <- ecount(G)
rel.thresh <- as.data.frame(list(0,V,E,cc$no,cc$csize[1],dia))
names(rel.thresh) <- c("threshold","V","E","cc.no","gc.size","gc.dia")

rel.friends <- data.frame()
for(name in unique(true.friends$gamertag.x))
{
    rel.friends <- merge(rel.friends,
    data.frame(0,
      name,
      length(intersect(edgelist.wtd[edgelist.wtd$gamertag.x==name,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
      length(union(edgelist.wtd[edgelist.wtd$gamertag.x==name,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
      length(edgelist.wtd[edgelist.wtd$gamertag.x==name,"gamertag.y"]),
      length(true.friends[true.friends$gamertag.x==name,"gamertag.y"]))
    )
}
names(rel.friends) <- c("threshold","gamertag.x","intsx","union","edge","true")

for(i in (1:99)/100)
{
  # create graph from edgelist
  G <- graph.edgelist(as.matrix(subset(edgelist.wtd, pwt>i,select=c("gamertag.x","gamertag.y"))), directed=FALSE)
  cc <- clusters(G)
  dia <- diameter(G)
  V <- vcount(G)
  E <- ecount(G)
  rel.thresh <- rbind(rel.thresh,list(i,V,E,cc$no,cc$csize[1],dia))
  for(name in unique(true.friends$gamertag.x))
  {
      tmp <- data.frame(i,
        name,
        length(intersect(edgelist.wtd[edgelist.wtd$gamertag.x==name & edgelist.wtd$pwt>i,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
        length(union(edgelist.wtd[edgelist.wtd$gamertag.x==name & edgelist.wtd$pwt>i,"gamertag.y"],true.friends[true.friends$gamertag.x==name,"gamertag.y"])),
        length(edgelist.wtd[edgelist.wtd$gamertag.x==name & edgelist.wtd$pwt>i,"gamertag.y"]),
        length(true.friends[true.friends$gamertag.x==name,"gamertag.y"])
      )
      names(tmp) <- c("threshold","gamertag.x","intsx","union","edge","true")
      rel.friends <- rbind(rel.friends, tmp)
  }
}

rel.friends$jaccard <- rel.friends$intsx / rel.friends$union
rel.friends$precision <- 0
rel.friends[rel.friends$edge>0,]$precision <- rel.friends[rel.friends$edge>0,]$intsx / rel.friends[rel.friends$edge>0,]$edge
rel.friends$recall <- 0
rel.friends[rel.friends$true>0,]$recall <- rel.friends[rel.friends$true>0,]$intsx / rel.friends[rel.friends$true>0,]$true

ggplot(rel.thresh) + geom_line(aes(x=threshold,y=V)) + labs(x="Threshold",y="Number of Nodes")
ggsave("../fig/rel_thresh_V.png", width=5,height=5)
ggplot(rel.thresh) + geom_line(aes(x=threshold,y=E)) + labs(x="Threshold",y="Number of Edges")
ggsave("../fig/rel_thresh_E.png", width=5,height=5)
ggplot(rel.thresh) + geom_line(aes(x=threshold,y=E/V)) + labs(x="Threshold",y="Average Degree")
ggsave("../fig/rel_thresh_deg.png", width=5,height=5)
ggplot(rel.thresh) + geom_line(aes(x=threshold,y=cc.no)) + labs(x="Threshold",y="Number of Connected Components")
ggsave("../fig/rel_thresh_cc.png", width=5,height=5)
ggplot(rel.thresh) + geom_line(aes(x=threshold,y=gc.size)) + labs(x="Threshold",y="Size of Largest Component")
ggsave("../fig/rel_thresh_gcsize.png", width=5,height=5)
ggplot(rel.thresh) + geom_line(aes(x=threshold,y=gc.dia)) + labs(x="Threshold",y="Diameter of Largest Component")
ggsave("../fig/rel_thresh_gcdia.png", width=5,height=5)

ggplot(rel.friends) + geom_line(aes(x=threshold,y=jaccard,color=gamertag.x)) + opts(legend.position=c(0.8,0.8))
ggsave("../fig/rel_thresh_jaccard.png", width=5,height=5)
ggplot(rel.friends) + geom_line(aes(x=threshold,y=precision,color=gamertag.x)) + opts(legend.position=c(0.8,0.8))
ggsave("../fig/rel_thresh_precision.png", width=5,height=5)
ggplot(rel.friends) + geom_line(aes(x=threshold,y=recall,color=gamertag.x)) + opts(legend.position=c(0.8,0.8))
ggsave("../fig/rel_thresh_recall.png", width=5,height=5)




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
