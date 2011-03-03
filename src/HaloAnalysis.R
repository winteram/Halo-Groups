# setwd("/Users/winteram/Documents/Research/Halo-Groups/src")
# require(timeDate)
require(ggplot2)
require(SparseM)
require(igraph)
theme_set(theme_bw())

# To load processed data
#load('../data/games_new.Rdata')

# To create processed data
games_full <- read.csv("../data/allgames.tsv", skip=1, header=FALSE)
names(games_full) <- c("PlayerDataIndex","gamertag","service_tag","first_active","last_active","games_total","GameId","GameVariantName","GameVariantClass","MapName","GameTimestamp","IsTeamGame","PlayerCount","Rating","Standing","Score","Team","TeamStanding","TeamScore","Kills","Deaths","Assists","Betrayals","Headshots","Suicides","AvgKillDistanceMeters","KilledMostCount","PlayerKilledByMost","KilledMostByCount","TotalMedalCount","UniqueTotalMedalCount","StyleMedalCount","UniqueStyleMedalCount","SpreeMedalCount","UniqueSpreeMedalCount","MultiMedalCount","UniqueMultiMedalCount","OtherMedalCount","Kills.0","Headshots.0","Deaths.0","Penalties.0","Kills.1","Headshots.1","Deaths.1","Penalties.1","Kills.2","Headshots.2","Deaths.2","Penalties.2","Kills.3","Headshots.3","Deaths.3","Penalties.3","Kills.4","Headshots.4","Deaths.4","Penalties.4","Kills.5","Headshots.5","Deaths.5","Penalties.5","Kills.6","Headshots.6","Deaths.6","Penalties.6","Kills.7","Headshots.7","Deaths.7","Penalties.7","Kills.8","Headshots.8","Deaths.8","Penalties.8","Kills.9","Headshots.9","Deaths.9","Penalties.9","Kills.10","Headshots.10","Deaths.10","Penalties.10","Kills.11","Headshots.11","Deaths.11","Penalties.11","Kills.12","Headshots.12","Deaths.12","Penalties.12","Kills.13","Headshots.13","Deaths.13","Penalties.13","Kills.14","Headshots.14","Deaths.14","Penalties.14","Kills.15","Headshots.15","Deaths.15","Penalties.15","Kills.16","Headshots.16","Deaths.16","Penalties.16","Kills.17","Headshots.17","Deaths.17","Penalties.17","Kills.18","Headshots.18","Deaths.18","Penalties.18","Kills.19","Headshots.19","Deaths.19","Penalties.19","Kills.20","Headshots.20","Deaths.20","Penalties.20","Kills.21","Headshots.21","Deaths.21","Penalties.21","Kills.22","Headshots.22","Deaths.22","Penalties.22","Kills.23","Headshots.23","Deaths.23","Penalties.23","Kills.24","Headshots.24","Deaths.24","Penalties.24","Kills.25","Headshots.25","Deaths.25","Penalties.25","Kills.26","Headshots.26","Deaths.26","Penalties.26","Kills.27","Headshots.27","Deaths.27","Penalties.27","Kills.28","Headshots.28","Deaths.28","Penalties.28","Kills.29","Headshots.29","Deaths.29","Penalties.29","Kills.30","Headshots.30","Deaths.30","Penalties.30","Kills.31","Headshots.31","Deaths.31","Penalties.31","Kills.32","Headshots.32","Deaths.32","Penalties.32","Kills.33","Headshots.33","Deaths.33","Penalties.33","Kills.34","Headshots.34","Deaths.34","Penalties.34","Kills.35","Headshots.35","Deaths.35","Penalties.35","Kills.36","Headshots.36","Deaths.36","Penalties.36","Kills.37","Headshots.37","Deaths.37","Penalties.37","Kills.38","Headshots.38","Deaths.38","Penalties.38","Kills.39","Headshots.39","Deaths.39","Penalties.39","Kills.40","Headshots.40","Deaths.40","Penalties.40","Kills.41","Headshots.41","Deaths.41","Penalties.41","Kills.42","Headshots.42","Deaths.42","Penalties.42","Kills.43","Headshots.43","Deaths.43","Penalties.43","Kills.44","Headshots.44","Deaths.44","Penalties.44","Kills.45","Headshots.45","Deaths.45","Penalties.45","Kills.46","Headshots.46","Deaths.46","Penalties.46","Kills.47","Headshots.47","Deaths.47","Penalties.47","Kills.48","Headshots.48","Deaths.48","Penalties.48","Kills.49","Headshots.49","Deaths.49","Penalties.49","Kills.50","Headshots.50","Deaths.50","Penalties.50","Kills.51","Headshots.51","Deaths.51","Penalties.51","Kills.52","Headshots.52","Deaths.52","Penalties.52","Kills.53","Headshots.53","Deaths.53","Penalties.53","Kills.54","Headshots.54","Deaths.54","Penalties.54","Kills.55","Headshots.55","Deaths.55","Penalties.55","Kills.56","Headshots.56","Deaths.56","Penalties.56","Kills.57","Headshots.57","Deaths.57","Penalties.57","Kills.58","Headshots.58","Deaths.58","Penalties.58","Kills.59","Headshots.59","Deaths.59","Penalties.59","Kills.63","Headshots.63","Deaths.63","Penalties.63","Kills.64","Headshots.64","Deaths.64","Penalties.64","Kills.74","Headshots.74","Deaths.74","Penalties.74")
games_full <- unique(games_full)
games <- games_full[,1:38]
games$first_active <- as.integer(substr(games[,"first_active"], 7, 16))
games$last_active <- as.integer(substr(games[,"last_active"], 7, 16))
games$GameTimestamp <- as.integer(substr(games[,"GameTimestamp"], 7, 16))
games$kdratio <- 0
games[games$Deaths>0,]$kdratio <- games[games$Deaths>0,]$Kills / games[games$Deaths>0,]$Deaths

crawled_players <- read.csv("../players/playerList.csv")
names(crawled_players) <- "gamertag"

# Draw connections between players
name.to.int <- data.frame(unique(games$gamertag), 1:length(unique(games$gamertag)))
names(name.to.int) <- c("gamertag","gamerid")
games <- merge(games, name.to.int, by="gamertag", all.x=TRUE)
crawled_players <- merge(name.to.int, crawled_players, by="gamertag")


# Create affiliation graph
p.g.bipartite <- subset(games[grep("(G)", games$gamertag,invert=TRUE,fixed=TRUE),], select=c("gamerid","GameId"))
p.g.bipartite <- merge(p.g.bipartite, p.g.bipartite, by="GameId", all=TRUE)
p.g.bipartite <- subset(p.g.bipartite, gamerid.x != gamerid.y)
adj.ids <- ddply(subset(p.g.bipartite, gamerid.x<20), .(gamerid.x, gamerid.y), function(x) nrow(x))
for(i in seq(20,nrow(p.g.bipartite),20))
{
	tmp <- ddply(subset(p.g.bipartite, gamerid.x>=i & gamerid.x<i+20), .(gamerid.x, gamerid.y), function(x) nrow(x))
	adj.ids <- rbind(adj.ids, tmp)
}
names(adj.ids) <- c("player1","player2","weight")
adj.ids <- subset(adj.ids, player1 < player2)


players.ngames <- as.data.frame(table(games$gamerid))
players.nedges <- as.data.frame(table(adj.ids$player1))
adj.ids <- merge(adj.ids, players.nedges, by.x="player1", by.y="Var1", all.x=TRUE)
adj.ids <- merge(adj.ids, players.ngames, by.x="player1", by.y="Var1", all.x=TRUE)
names(adj.ids) <- c("player1","player2","weight","nedges","ngames")

all.strong.ties <- merge(subset(adj.ids, weight > nedges / ngames), name.to.int, by.x="player1", by.y="gamerid")
all.strong.ties <- merge(all.strong.ties, name.to.int, by.x="player2", by.y="gamerid")
strong.ties <- subset(all.strong.ties, player1 %in% crawled_players$gamerid & player2 %in% crawled_players$gamerid)
#strong.ties <- strong.ties[intersect(grep("(G)", strong.ties$gamertag.x,invert=TRUE,fixed=TRUE),grep("(G)", strong.ties$gamertag.y,invert=TRUE,fixed=TRUE)),]

G <- graph.edgelist(as.matrix(subset(strong.ties, select=c("gamertag.x","gamertag.y"))), directed=FALSE)
strong.tie.sizes <- as.data.frame(get.vertex.attribute(G, "name"))
names(strong.tie.sizes) <- "gamertag"
strong.tie.sizes <- transform(strong.tie.sizes, index=0:(nrow(strong.tie.sizes)-1))
strong.tie.sizes <- merge(strong.tie.sizes, name.to.int, by="gamertag",all.x=TRUE)
strong.tie.sizes <- merge(strong.tie.sizes, players.ngames, by.x="gamerid",by.y="Var1", all.x=TRUE)

G <- set.edge.attribute(G, "weight", value=log(strong.ties$weight))
G <- set.vertex.attribute(G, "ngames", value=log(strong.tie.sizes$Freq), index=strong.tie.sizes$index)

tmp.layout <- layout.fruchterman.reingold(G, weights=get.edge.attribute(G, "weight"))
#tmp.layout <- layout.kamada.kawai(G)
#png("gameGraph.png",width=480,height=480)
plot(G, layout=tmp.layout, edge.width=get.edge.attribute(G, "weight"), vertex.size=get.vertex.attribute(G, "ngames"), vertex.label=get.vertex.attribute(G, "name"), vertex.label.cex=0.6, vertex.label.family="Arial", vertex.label.color="black")
#dev.off()

#games.ties <- subset(strong.ties, player1==1)
#games.ties <- games.ties[with(games.ties, order(weight)),]

#ggplot(games.ties) + geom_bar(aes(x=name.y,y=weight),fill="blue") + scale_x_discrete(limits=games.ties$name.y) + opts(legend.position="none") + theme_bw() + coord_flip()
#ggsave("gamesTopCoplayers.pdf")




#possible analyses:
games.arrow <- unique(games[games$gamertag=="Arrow of Doubt","GameId"])
games.self <- games[games$GameId %in% games.arrow,]
arrow.coplayers <- as.data.frame(table(subset(games.self, gamertag!="Arrow of Doubt")$gamertag))
names(arrow.coplayers) <- c("gamertag","Freq")
arrow.coplay.dist <- as.data.frame(table(subset(arrow.coplayers, arrow.coplayers$Freq>0, select="Freq")))
names(arrow.coplay.dist) <- c("Games_Played","Count")
arrow.coplay.dist$Games_Played <- as.numeric(arrow.coplay.dist$Games_Played)
arrow.coplay.dist$Count <- as.numeric(arrow.coplay.dist$Count)
ggplot(arrow.coplay.dist, aes(x=Games_Played, y=Count)) + geom_point()
ggsave("../fig/ArrowCoplayerDist.pdf", width=5,height=5)
ggplot(subset(arrow.coplay.dist, Games_Played>1), aes(x=Games_Played, y=Count)) + geom_point()

arrow.coplay.ccdf <- arrow.coplay.dist
arrow.coplay.ccdf$PDF <- arrow.coplay.ccdf$Count / sum(arrow.coplay.ccdf$Count)
arrow.coplay.ccdf$CCDF <- arrow.coplay.ccdf$PDF
for(i in 2:nrow(arrow.coplay.ccdf))
{
	arrow.coplay.ccdf[i,]$CCDF <- arrow.coplay.ccdf[i-1,]$CCDF + arrow.coplay.ccdf[i,]$CCDF
}
arrow.coplay.ccdf$CCDF <- 1 - arrow.coplay.ccdf$CCDF
ggplot(arrow.coplay.ccdf, aes(x=Games_Played, y=CCDF)) + geom_point()
 
ggsave("../fig/ArrowCoplayerCCDF.pdf")

# select players who have played with arrow more than X games
arrow.friends <- subset(arrow.coplayers, Freq>5)
num.friends <- ddply(games.arrow, .(GameId), summarize, )

# calculate change in average score with / without each teammate

# homophily of success? correlation in kdratio between teammates
kd_mean <- ddply(games, .(gamerid), summarize, kdratio=mean(kdratio))
kd <- merge(adj.ids, kd_mean, by.x="player1", by.y="gamerid", all.x=TRUE)
kd <- merge(kd, kd_mean, by.x="player2", by.y="gamerid", all.x=TRUE)
# low --- ~0.12

