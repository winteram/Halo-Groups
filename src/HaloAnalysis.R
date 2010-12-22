# setwd("/Users/winteram/Documents/Research/HaloGroups")
# require(timeDate)
require(ggplot2)
require(Matrix)
require(igraph)

arrow_all <- read.csv("Arrow_of_Doubt_games.tsv")
arrow_all <- unique(arrow_all)
arrow <- arrow_all[,1:38]
arrow$first_active <- as.integer(substr(arrow[,"first_active"], 7, 16))
arrow$last_active <- as.integer(substr(arrow[,"last_active"], 7, 16))
arrow$GameTimestamp <- as.integer(substr(arrow[,"GameTimestamp"], 7, 16))
arrow$kdratio <- 0
arrow[arrow$Deaths>0,]$kdratio <- arrow[arrow$Deaths>0,]$Kills / arrow[arrow$Deaths>0,]$Deaths

# Draw connections between players
name.to.int <- data.frame(unique(arrow$gamertag), 1:length(unique(arrow$gamertag)))
names(name.to.int) <- c("gamertag","gamerid")
arrow <- merge(arrow, name.to.int, by="gamertag", all.x=TRUE)

player.game.bipartite <- sparseMatrix(i=arrow$gamerid, j=arrow$GameId)

n.players <- nrow(name.to.int)
adj.ids <- data.frame(cbind(rep(1:n.players,each=n.players),rep(1:n.players,n.players),rep(0,n.players*n.players)))
names(adj.ids) <- c("player1","player2","weight")
adj.ids <- subset(adj.ids, player1 < player2)


freq.players.all <- as.data.frame(table(arrow$gamertag))
strong.ties <- merge(adj.ids[adj.ids$weight > 3,], name.to.int, by.x="player1", by.y="id")
strong.ties <- merge(strong.ties, name.to.int, by.x="player2", by.y="id")
strong.tie.names <- name.to.int[name.to.int$id %in% strong.ties$player1 | name.to.int$id %in% strong.ties$player2,]
strong.tie.sizes <- freq.players.all[freq.players.all$Var1 %in% strong.ties$name.x | freq.players.all$Var1 %in% strong.ties$name.y,]

G <- graph.edgelist(as.matrix(strong.ties[,4:5]), directed=FALSE)
G <- set.edge.attribute(G, "weight", value=log(strong.ties$weight))

tmp.layout <- layout.kamada.kawai(G)
png("gameGraph.png",width=480,height=480)
plot(G, layout=tmp.layout, edge.width = get.edge.attribute(G, "weight"), vertex.label=strong.tie.names$name, vertex.size=log(strong.tie.sizes$Freq))
dev.off()

arrow.ties <- subset(strong.ties, player1==1)
arrow.ties <- arrow.ties[with(arrow.ties, order(weight)),]

ggplot(arrow.ties) + geom_bar(aes(x=name.y,y=weight),fill="blue") + scale_x_discrete(limits=arrow.ties$name.y) + opts(legend.position="none") + theme_bw() + coord_flip()
ggsave("ArrowTopCoplayers.pdf")


#arrow.self <- arrow[arrow$gamertag=="Arrow of Doubt",]
#arrow.self <- arrow.self[order(arrow.self$GameVariantName, arrow.self$GameTimestamp),]

#possible analyses:

# calculate change in average score with / without each teammate

# 
