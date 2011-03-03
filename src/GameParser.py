##Python script for parsing Halo Data
import simplejson
import operator, pickle
import os.path, sys, re, time
import logging

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s")
logger=logging.getLogger('GameParser')

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

# Extract details for each game
def getGameDetails(gameDetail,outputFile):
     
    try:
        parsedPage = simplejson.loads(gameDetail)
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
        weaponList = ["0"]*260
        for weapon in weaponInfo:
            weaponIndex = weapon["WeaponId"]
            if weaponIndex == 63 or weaponIndex == 64:
                weaponIndex -= 3
            if weaponIndex == 74:
                weaponIndex = 62
            elif weaponIndex > 64 or weaponIndex<0:
                weaponIndex = 63

            weaponIndex *= 4
            weaponList[weaponIndex] = str(weapon["Kills"])
            weaponList[weaponIndex+1] = str(weapon["Headshots"])
            weaponList[weaponIndex+2] = str(weapon["Deaths"])
            weaponList[weaponIndex+3] = str(weapon["Penalties"])

        gameString += ','.join(weaponList)
        gameString = ''.join(c for c in gameString if ord(c) in range(128))

        outputFile.write(gameString + "\n")


if __name__ == "__main__":
	
    # define how often data is written
    BLOCK_SIZE = 50
    i = 1

    begin_time = time.time()
    logger.info("Started: " + str(begin_time))    

#    if os.path.exists('../data/sample_gameList.pkl'):
#        try:
#            f_gamelist = open("../data/sample_gameList.pkl",'r')
#            gamelist = pickle.load(f_gamelist)
#            f_gamelist.close()
#        except:
#            logger.info("Couldn't find or unpickle sample_gameList.pkl")
    if os.path.exists('../data/sample/'):
        try:
            gamelist = os.listdir("../data/sample/")
        except:
            logger.info("Couldn't find sample directory")

    # final, parsed output file
    if os.path.exists('../data/game_sample.tsv'):
        outputFile = open("../data/game_sample.tsv",'a')
    else:
        outputFile = open("../data/game_sample.tsv",'w')
        writeHeader(outputFile)

    while len(gamelist) > 0:
		
        # open file
        game = gamelist.pop()
        try:
            f_gameDetail = open("../data/sample/"+str(game),'r')
            gameDetail = f_gameDetail.read()
            f_gameDetail.close()
        except:
            logger.info("Couldn't open game "+str(game))
	
        # pass contents to write output
        logger.info("Retrieving game details for " + str(game))
        getGameDetails(gameDetail,outputFile)
		
		
        i += 1
		
        # if done BLOCK_SIZE files,
        if i % BLOCK_SIZE == 0:
#            f_gameList = open("../data/sample_gameList.pkl",'w')
#            pickle.dump(gamelist, f_gameList)
#            f_gameList.close()
            outputFile.close()
            outputFile = open("../data/game_sample.tsv",'a')

    end_time = time.time()
    logger.info("Finished: " + str(end_time))
    logger.info("Total time: " + str(end_time - begin_time))
