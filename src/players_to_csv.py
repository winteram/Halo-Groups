import pickle

f_playerList = open("../players/playerList.pkl",'r')
playerList = pickle.load(f_playerList)
f_playerList.close()
outputFile = open("../players/playerList.csv","w")
for player in playerList:
     if playerList[player]==0:
             outputFile.write(player + "\n")

outputFile.close()

