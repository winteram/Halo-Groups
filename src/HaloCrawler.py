##Python script for crawling Halo Data
import simplejson
import operator
import sys, urllib, urllib2, httplib, re, time
import logging

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s")
logger=logging.getLogger('HaloCrawler')

api_key = "SgdO4MSJ4DQcUofL3bUEJ9mdDmZf$r5G4LW1IdpFodk="

# write Header info on output file (column names)
def writeHeader(outputFile):
    header_info = "PlayerDataIndex,"
    header_info += "gamertag,"
    header_info += "service_tag,"
    header_info += "first_active,"
    header_info += "last_active,"
    header_info += "games_total,"
    header_info += "GameId,"
    header_info += "GameVariantName,"
    header_info += "GameVariantClass,"
    header_info += "MapName,"
    header_info += "GameTimestamp,"
    header_info += "IsTeamGame,"
    header_info += "PlayerCount,"
    header_info += "Rating,"
    header_info += "Standing,"
    header_info += "Score,"
    header_info += "Team,"
    header_info += "TeamStanding,"
    header_info += "TeamScore,"
    header_info += "Kills,"
    header_info += "Deaths,"
    header_info += "Assists,"
    header_info += "Betrayals,"
    header_info += "Headshots,"
    header_info += "Suicides,"
    header_info += "AvgKillDistanceMeters,"
    header_info += "KilledMostCount,"
    header_info += "PlayerKilledByMost,"
    header_info += "KilledMostByCount,"
    header_info += "TotalMedalCount,"
    header_info += "UniqueTotalMedalCount,"
    header_info += "StyleMedalCount,"
    header_info += "UniqueStyleMedalCount,"
    header_info += "SpreeMedalCount,"
    header_info += "UniqueSpreeMedalCount,"
    header_info += "MultiMedalCount,"
    header_info += "UniqueMultiMedalCount,"
    header_info += "OtherMedalCount,"
    for i in range(0,60):
        header_info += "Kills." + str(i) + ","
        header_info += "Headshots." + str(i) + ","
        header_info += "Deaths." + str(i) + ","
        header_info += "Penalties." + str(i) + ","
    header_info += "Kills.63,"
    header_info += "Headshots.63,"
    header_info += "Deaths.63,"
    header_info += "Penalties.63,"
    header_info += "Kills.64,"
    header_info += "Headshots.64,"
    header_info += "Deaths.64,"
    header_info += "Penalties.64,"
    header_info += "Kills.74,"
    header_info += "Headshots.74,"
    header_info += "Deaths.74,"
    header_info += "Penalties.74"

    outputFile.write(header_info + "\n")


# get Player's game history
def getGameHistory(player, gameIDs):
    gameHistBaseURL = "http://www.bungie.net/api/reach/reachapijson.svc/player/gamehistory/"
    pagenum = 0
    allpages = False
    
    while not allpages:
        #logger.info("Retrieving game history page " + str(pagenum+1))
        goodpage = False
        gameHistURL = gameHistBaseURL + api_key + "/" + urllib.quote(player) + "/Unknown/" + str(pagenum)
        # logger.info(gameHistURL)
        req = urllib2.Request(gameHistURL)
        req.add_header('User-agent','http://research.yahoo.com/; winteram@yahoo-inc.com')
        startcall = time.time()
        try:
        	response = urllib2.urlopen(req)	
        except httplib.HTTPException, h:
        	logger.info("HTTP Exception %s", h)
        	pass
        except IOError, e:
        	if hasattr(e, 'reason'):
        		logger.info("Server Error %s", e.reason)
        	elif hasattr(e, 'code'):
        		logger.info("HTTP error code: %s", e.code)
        		if e.code < 400:
        			goodpage = True
        else:
        	goodpage = True
	
        if (goodpage):	
            logger.info("Successfully retrieved page " + str(pagenum+1))
            gameHistPage = response.read()

            try:
                parsedPage = simplejson.loads(gameHistPage)
            except:
                logger.info("Failure to load JSON")
                break
                    
            if type(parsedPage) != dict:
                logger.info("parsedPage not a dict")
                break

            #logger.info(parsedPage["HasMorePages"])
            if (parsedPage["HasMorePages"]==True):
                pagenum += 1
            else:
                allpages = True
                
            for game in parsedPage["RecentGames"]:
                # logger.info("GameID: " + str(game["GameId"]) + " on " + game["GameTimestamp"])
                if not gameIDs.has_key(game["GameId"]):
                    gameIDs[game["GameId"]] = 0
        
        time.sleep(0.2 - (time.time() - startcall))
    return gameIDs

# Get details for each game
def getGameDetails(gameID,players,outputFile):
    gameDetailBaseURL = "http://www.bungie.net/api/reach/reachapijson.svc/game/details/"
     
    logger.info("Retrieving game details for " + str(gameID))
    goodpage = False
    gameDetailURL = gameDetailBaseURL + api_key + "/" + str(gameID)
    req = urllib2.Request(gameDetailURL)
    req.add_header('User-agent','http://research.yahoo.com/; winteram@yahoo-inc.com')
    
    try:
    	response = urllib2.urlopen(req)	
    except httplib.HTTPException, h:
    	logger.info("HTTP Exception %s", h)
    	pass
    except IOError, e:
    	if hasattr(e, 'reason'):
    		logger.info("Server Error %s", e.reason)
    	elif hasattr(e, 'code'):
    		logger.info("HTTP error code: %s", e.code)
    		if e.code < 400:
    			goodpage = True
    else:
    	goodpage = True

    if (goodpage):	
	gameDetailPage = response.read()

        try:
            parsedPage = simplejson.loads(gameDetailPage)
        except:
            logger.info("Failure to load JSON")
            return
                
        if type(parsedPage) != dict:
            logger.info("parsedPage not a dict")
            return

        gameDetails = parsedPage["GameDetails"]
        #logger.info(gameDetails["GameTimestamp"][6:16])
        for player in gameDetails["Players"]:
            playerInfo = player["PlayerDetail"]
            #logger.info("Writing info for Player " + playerInfo["gamertag"])
            if players.has_key(playerInfo["gamertag"]):
                if players[playerInfo["gamertag"]] > 0:
                    players[playerInfo["gamertag"]] += 1
            else:
                players[playerInfo["gamertag"]] = 1
                
            gameString = str(player["PlayerDataIndex"]) + ","
            gameString += playerInfo["gamertag"] + ","
            gameString += playerInfo["service_tag"] + ","
            gameString += playerInfo["first_active"] + ","
            gameString += playerInfo["last_active"] + ","
            gameString += str(playerInfo["games_total"]) + ","
            gameString += str(gameDetails["GameId"]) + ","
            gameString += re.sub(',',':',gameDetails["GameVariantName"]) + ","
            gameString += str(gameDetails["GameVariantClass"]) + ","
            gameString += gameDetails["MapName"] + ","
            gameString += gameDetails["GameTimestamp"] + ","
            gameString += str(gameDetails["IsTeamGame"]) + ","
            gameString += str(gameDetails["PlayerCount"]) + ","
            gameString += str(player["Rating"]) + ","
            gameString += str(player["Standing"]) + ","
            gameString += str(player["Score"]) + ","
            gameString += str(player["Team"]) + ","
            if player.has_key("TeamStanding"):
                gameString += str(player["TeamStanding"]) + ","
            else:
                gameString += "NA,"
            if player.has_key("TeamScore"):
                gameString += str(player["TeamScore"]) + ","
            else:
                gameString += "NA,"
            gameString += str(player["Kills"]) + ","
            gameString += str(player["Deaths"]) + ","
            gameString += str(player["Assists"]) + ","
            gameString += str(player["Betrayals"]) + ","
            gameString += str(player["Headshots"]) + ","
            gameString += str(player["Suicides"]) + ","
            gameString += str(player["AvgKillDistanceMeters"]) + ","
            gameString += str(player["KilledMostCount"]) + ","
            gameString += str(player["PlayerKilledByMost"]) + ","
            gameString += str(player["KilledMostByCount"]) + ","
            gameString += str(player["TotalMedalCount"]) + ","
            gameString += str(player["UniqueTotalMedalCount"]) + ","
            gameString += str(player["StyleMedalCount"]) + ","
            gameString += str(player["UniqueStyleMedalCount"]) + ","
            gameString += str(player["SpreeMedalCount"]) + ","
            gameString += str(player["UniqueSpreeMedalCount"]) + ","
            gameString += str(player["MultiMedalCount"]) + ","
            gameString += str(player["UniqueMultiMedalCount"]) + ","
            gameString += str(player["OtherMedalCount"]) + ","

            # Get weapon use info
            weaponInfo = player["WeaponCarnageReport"]
            weaponList = ["0"]*252
            for weapon in weaponInfo:
                weaponIndex = weapon["WeaponId"]
                if weaponIndex == 63 or weaponIndex == 64:
                    weaponIndex -= 3
                if weaponIndex == 74:
                    weaponIndex = 62

                weaponIndex *= 4
                weaponList[weaponIndex] = str(weapon["Kills"])
                weaponList[weaponIndex+1] = str(weapon["Headshots"])
                weaponList[weaponIndex+2] = str(weapon["Deaths"])
                weaponList[weaponIndex+3] = str(weapon["Penalties"])

            gameString += ','.join(weaponList)

            outputFile.write(gameString + "\n")
    return players

# If running from command line, take first argument as seed, or if none, use Arrow of Doubt
if __name__ == "__main__":
	if len(sys.argv) == 1:
		seed = "Arrow of Doubt"
	else:
		seed = sys.argv[1]
    
        begin_time = time.time()
        logger.info("Started: " + str(begin_time))    
	
	outputFile = open(re.sub('\s','_',seed) + "_games.tsv",'w')
        
        writeHeader(outputFile)

        gameIDs = dict()
        players = dict()
        players[seed] = 1
        for i in range(0,10):
            sortedPlayers = sorted(players.iteritems(), key=operator.itemgetter(1))
            player = sortedPlayers.pop()[0]
            players[player] = 0

            logger.info("Getting game history for " + player)
            gameIDs = getGameHistory(player, gameIDs)
		
            for gameID, checked in gameIDs.iteritems():
		if checked==0:
                    players = getGameDetails(gameID,players,outputFile)
                    gameIDs[gameID] = 1
                else:
                    logger.info("Skipping " + str(gameID))
	
	outputFile.close()
	
        end_time = time.time()
        logger.info("Finished: " + str(end_time))
        logger.info("Total time: " + str(end_time - begin_time))
