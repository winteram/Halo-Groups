# setwd("/Users/winteram/Documents/Research/HaloGroups")
# require(timeDate)
require(ggplot2)
require(Matrix)
require(igraph)
theme_set(theme_bw())

# correlation in kdratio amongst co-players
with(kd, cor(kdratio.x,kdratio.y))
strong.ties <- merge(strong.ties, kd_mean, by.x="player1", by.y="gamerid", all.x=TRUE)
strong.ties <- merge(strong.ties, kd_mean, by.x="player2", by.y="gamerid", all.x=TRUE)
with(strong.ties, cor(kdratio.x,kdratio.y))
# low for both: 0.124 for all ties, 0.126 for strong ties

# split games into: solo, campaign:co-op / custom, competitive matchmaking, cooperative matchmaking


# histogram of game sizes for crawled games
player.teamsizes <- with(games, table(gamertag, PlayerCount))
teamsizes <- as.data.frame(margin.table(player.teamsizes,2) / 1:16)
ggplot(teamsizes) + geom_histogram(aes(x=Var1,y=Freq)) + labs(x="Players in Game",y="Frequency")
ggsave("../fig/hist_game_sizes.pdf",width=5,height=5)

# Learning: time series of kd ratio (or just # deaths) for each player (segmented by game type?)
tmp <- subset(games, GameVariantClass==4 & gamerid %in% crawled_players$gamerid, select=c("gamerid","gamertag","GameId","PlayerCount","GameVariantName","Kills","Deaths","kdratio","Headshots","TotalMedalCount"))
cmpgn.by.plyr <- ddply(tmp, .(gamerid), transform, order=rank(GameId))
cmpgn.by.plyr <- transform(cmpgn.by.plyr, kd.diff=Kills-Deaths)

cmpgn.learning <- ddply(cmpgn.by.plyr, .(order), summarize, kills=mean(Kills), kills.sd=sd(Kills), deaths=mean(Deaths), deaths.sd=sd(Deaths), headshots=mean(Headshots), headshots.sd=sd(Headshots), medals=mean(TotalMedalCount), medals.sd=sd(TotalMedalCount), kd.diff=mean(kd.diff), kd.diff.sd=sd(kd.diff), n=length(gamertag))

# Full time series
ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=kills)) + geom_errorbar(aes(x=order, ymin=kills - (kills.sd/sqrt(n)),ymax=kills + (kills.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Kills")
ggsave("../fig/campaign_kills_learning.pdf", width=10,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=deaths)) + geom_errorbar(aes(x=order, ymin=deaths - (deaths.sd/sqrt(n)),ymax=deaths + (deaths.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Deaths")
ggsave("../fig/campaign_deaths_learning.pdf", width=10,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=kd.diff)) + geom_errorbar(aes(x=order, ymin=kd.diff - (kd.diff.sd/sqrt(n)),ymax=kd.diff + (kd.diff.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Kills - Deaths")
ggsave("../fig/campaign_kd_learning.pdf", width=10,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=headshots)) + geom_errorbar(aes(x=order, ymin=headshots - (headshots.sd/sqrt(n)),ymax=headshots + (headshots.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Headshots")
ggsave("../fig/campaign_headshots_learning.pdf", width=10,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=medals)) + geom_errorbar(aes(x=order, ymin=medals - (medals.sd/sqrt(n)),ymax=medals + (medals.sd/sqrt(n))),colour="gray60") + labs(x="Campaign Game Ordinality",y="Number of Medals")
ggsave("../fig/campaign_medals_learning.pdf", width=10,height=5)

# First 150 games
ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=kills)) + geom_errorbar(aes(x=order, ymin=kills - (kills.sd/sqrt(n)),ymax=kills + (kills.sd/sqrt(n))),colour="gray60") + xlim(0,150) + ylim(0,300) + labs(x="Campaign Game Ordinality",y="Number of Kills")
ggsave("../fig/campaign_kills_learning_sub.pdf", width=8,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=deaths)) + geom_errorbar(aes(x=order, ymin=deaths - (deaths.sd/sqrt(n)),ymax=deaths + (deaths.sd/sqrt(n))),colour="gray60") + xlim(0,150) + ylim(0,30) + labs(x="Campaign Game Ordinality",y="Number of Deaths")
ggsave("../fig/campaign_deaths_learning_sub.pdf", width=8,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=kd.diff)) + geom_errorbar(aes(x=order, ymin=kd.diff - (kd.diff.sd/sqrt(n)),ymax=kd.diff + (kd.diff.sd/sqrt(n))),colour="gray60") + xlim(0,150) + ylim(0,300) + labs(x="Campaign Game Ordinality",y="Number of Kills - Deaths")
ggsave("../fig/campaign_kd_learning_sub.pdf", width=8,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=headshots)) + geom_errorbar(aes(x=order, ymin=headshots - (headshots.sd/sqrt(n)),ymax=headshots + (headshots.sd/sqrt(n))),colour="gray60") + xlim(0,150) + ylim(0,50) + labs(x="Campaign Game Ordinality",y="Number of Headshots")
ggsave("../fig/campaign_headshots_learning_sub.pdf", width=8,height=5)

ggplot(subset(cmpgn.learning, n>1)) + geom_line(aes(x=order,y=medals)) + geom_errorbar(aes(x=order, ymin=medals - (medals.sd/sqrt(n)),ymax=medals + (medals.sd/sqrt(n))),colour="gray60") + xlim(0,150) + ylim(0,200) + labs(x="Campaign Game Ordinality",y="Number of Medals")
ggsave("../fig/campaign_medals_learning_sub.pdf", width=8,height=5)

## POTENTIAL ANALYSES

# Specialization: entropy across weapon types over games (increases = specialization)

# Player contribution to dynamics: calculate change in average score for games played with / without each teammate

# 
