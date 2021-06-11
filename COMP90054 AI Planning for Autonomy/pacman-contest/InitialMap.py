import copy
def searchDeadEnd(map):
    # 0: Walls
    # 1: Available movements
    # 2: RedFood
    # 3: RedCapsule
    # 4: BlueFood
    # 5: BlueCapsule
    #
    # blueCapsule = {}
    # redCapsule = {}
    deadEndDist = {}
    deadEnd = []
    for i in range(1,len(map)-1):
        for j in range(1,len(map[i])-1):
            if map[i][j] != 0:
                if moveNum(map,i,j,deadEnd) == 1:
                    deadEnd = moveEnd(map,i,j,deadEnd)
    deadEnd = list(set(deadEnd))
    # for i in range(1,len(map)-1):
    #     for j in range(1,len(map[i])-1):
    #         if map[i,j] == 3:
    #             blueCapsule[(i,j)] = capsuleMove(map,i,j,deadEnd,[])
    #         if map[i,j] == 5:
    #             redCapsule[(i,j)] = capsuleMove(map,i,j,deadEnd,[])
    deadEndDist = getDepth(deadEnd,map)
    deadEndDistInver = {}
    for i in deadEndDist:
        for j in deadEndDist[i]:
            x = j[1]
            y = len(map) - 1 - j[0]
            deadEndDistInver[(x,y)] = i
    # print(deadEndDistInver)
    # for i in range(0,len(map)):
    #     for j in range(0,len(map[i])):
    #         if map[i][j] == 0:
    #             print("%",end='')
    #         else:
    #             if (i,j) in deadEnd:
    #                 print(str(deadEndDistInver[(i,j)]),end = "")
    #             else:
    #                 print("#",end = "")
    #     print()
    return deadEndDistInver

def getDepth(deadEnd,map):
    deadEndDist = {}
    deadEndDist[1] = []
    deadEndRemove = copy.deepcopy(deadEnd)
    addDist = []
    moves = [(-1,0),(+1,0),(0,-1),(0,+1)]
    for i in range(1,len(map)-1):
        for j in range(1,len(map[i])-1):
            if map[i][j] != 0 and (not (i,j) in deadEnd):
                for move in moves:
                    if (i+move[0],j+move[1]) in deadEnd:
                        deadEndDist[1].append((i+move[0],j+move[1]))
                        deadEndRemove.remove((i+move[0],j+move[1]))
                        addDist.append((i+move[0],j+move[1]))

    depth = 1
    while set(addDist) != set(deadEnd):
        deadEndDist[depth+1] = []
        for i in deadEndDist[depth]:
            for move in moves:
                if ((i[0]+move[0],i[1]+move[1]) in deadEnd) and (not (i[0]+move[0],i[1]+move[1]) in addDist):
                    deadEndDist[depth+1].append((i[0]+move[0],i[1]+move[1]))
                    # print((i[0]+move[0],i[1]+move[1]))
                    deadEndRemove.remove((i[0]+move[0],i[1]+move[1]))
                    addDist.append((i[0]+move[0],i[1]+move[1]))
        depth +=1
    return deadEndDist



def capsuleMove(map,x,y,deadEnd,caList):
    if (x,y) in deadEnd:
        caList.append((x,y))
        moves = [(-1,0),(+1,0),(0,-1),(0,+1)]
        for i in moves:
            if (x+i[0],y+i[1]) in deadEnd:
                if moveNum(map,x,y,deadEnd) >0:
                    caList = capsuleMove(map,x,y,deadEnd,caList)
    return caList

def moveNum(map,x,y,deadEnd):
    numDead = ((x-1,y) in deadEnd) + ((x+1,y) in deadEnd) + ((x,y-1) in deadEnd)+((x,y+1) in deadEnd)
    return (map[x-1][y] != 0) + (map[x+1][y] != 0 ) + (map[x][y-1] !=0) + (map[x][y+1] !=0) - numDead

def checkMid(pos1,pos2,midx):
    return not ((pos1 - midx) * (pos2 - midx) < 0)

def moveEnd(map,x,y,deadEnd):
    deadEnd.append((x,y))
    moves = [(-1,0),(+1,0),(0,-1),(0,+1)]
    for i in moves:
        if map[x+i[0]][y+i[1]] != 0:
            if (moveNum(map,x+i[0],y+i[1],deadEnd) == 1) and checkMid(y + i[1],y,(len(map[0])+1)/2):
                # deadEnd.append((x+i[0],y+i[1]))
                deadEnd = moveEnd(map,x+i[0],y+i[1],deadEnd)
    return deadEnd

def getFoodRegion(agent,gameState):
    meanDist = getFoodMeanDistance(agent,gameState)
    foodList = agent.getFood(gameState).asList()
    foodRegion = {}
    # print("meanDist",meanDist)
    foodInRegion = {}
    for food in foodList:
        foodRegion[food] = []
        for food2 in foodList:
            dist = agent.distancer.getDistance(food,food2)
            if dist < 8:
                foodInRegion[food2] = dist
        FList = sorted(foodInRegion.values(),key=lambda item:item)
        maxDist = FList[min(3,len(FList)-1)]
        FList2 = sorted(foodInRegion.items(),key=lambda item:item[1])
        for i in FList2:
            if i[1] <= maxDist:
                foodRegion[food].append(i[0])
    # print("food Region",foodRegion)
    return foodRegion

def getFoodMeanDistance(agent,gameState):
    foodList = agent.getFood(gameState).asList()
    foodNumber = len(foodList)
    foodDist = 0
    for food in foodList:
        minDist = 999
        maxDist = 0
        for food2 in foodList:
            dist = agent.distancer.getDistance(food,food2)
            if dist != 0 :
                minDist = min(minDist,dist)
                maxDist = max(maxDist,dist)
        foodDist += (maxDist - minDist) / 2
    foodDist = foodDist / foodNumber
    return foodDist

def cluster1(gameState,food,agent):
    foodList = agent.getFood(gameState).asList()
    foodNumber = len(foodList)
    cluster = []
    open = [food]
    while (open != []) and len(cluster) < min(5,foodNumber / 2):
        newOpen = []
        for i in open:
            for j in foodList:
                dist = agent.distancer.getDistance(i,j)
                if dist < 8:
                    cluster.append(j)
                    newOpen.append(j)
        open = newOpen
    return cluster


def cluster(food,foodRegion,foodNumber):
    foodCluster = []
    region = copy.deepcopy(foodRegion[food])
    while region != [] and (len(foodCluster) <= foodNumber/2):
        newRegion = []
        for food1 in region:
            for food2 in foodRegion[food1]:
                if not food2 in foodCluster:
                    foodCluster.append(food2)
                    newRegion.append(food2)
        region = newRegion
    # print("food Region",region)
    return foodCluster





# def searchDeadEnd(map):
#     # 0: Walls
#     # 1: Available movements
#     # 2: RedFood
#     # 3: RedCapsule
#     # 4: BlueFood
#     # 5: BlueCapsule
#     #
#     # blueCapsule = {}
#     # redCapsule = {}
#     deadEnd = {}
#     for i in range(1,len(map)-1):
#         for j in range(1,len(map[i])-1):
#             if map[i][j] != 0:
#                 if moveNum(map,i,j,deadEnd) == 1:
#                     deadEnd,depth = moveEnd(map,i,j,deadEnd,1)
#     for i in range(0,len(map)):
#         for j in range(0,len(map[i])):
#             if map[i][j] == 0:
#                 print("%",end='')
#             else:
#                 if (i,j) in deadEnd:
#                     print(deadEnd[(i,j)],end = "")
#                 else:
#                     print("#",end = "")
#         print()
#     print("END")
#     return deadEnd
#
# def moveNum(map,x,y,deadEnd):
#     numDead = ((x-1,y) in deadEnd) + ((x+1,y) in deadEnd) + ((x,y-1) in deadEnd)+((x,y+1) in deadEnd)
#     return (map[x-1][y] != 0) + (map[x+1][y] != 0 ) + (map[x][y-1] !=0) + (map[x][y+1] !=0) - numDead
#
# def moveEnd(map,x,y,deadEnd,depth):
#     newdepth = depth
#     deadEnd[(x,y)] = newdepth - depth +1
#     moves = [(-1,0),(+1,0),(0,-1),(0,+1)]
#     for i in moves:
#         if map[x+i[0]][y+i[1]] != 0:
#             if moveNum(map,x+i[0],y+i[1],deadEnd) == 1:
#                 deadEnd[(x,y)] = depth
#                 deadEnd,newdepth = moveEnd(map,x+i[0],y+i[1],deadEnd,depth+1)
#                 deadEnd[(x,y)] = newdepth - deadEnd[(x,y)]+1
#     return deadEnd,newdepth

def getMiddleX(self, gameState):
    mapWidth = gameState.data.layout.width
    if self.red:
      x = int((mapWidth - 2) / 2)
    else:
      x = int((mapWidth - 2) / 2 + 1)
    return x

def getEnemyMiddleX(self, gameState): # x of middle line on enemy's side
    mapWidth = gameState.data.layout.width
    if self.red:
      enemyX = int((mapWidth - 2) / 2 + 1)
    else:
      enemyX = int((mapWidth - 2) / 2)
    return enemyX

def getMiddleLine(self, gameState):
    middleLine = []
    mapHeight = gameState.data.layout.height
    x = self.middleX
    wallList = gameState.getWalls().asList()
    for y in range(1, mapHeight):
      if (x, y) not in wallList:
        middleLine.append((x,y))
    return middleLine

def getEnemyMiddleLine(self, gameState):
    enemyMiddleLine = []
    mapHeight = gameState.data.layout.height
    x = self.enemyMiddleX
    wallList = gameState.getWalls().asList()
    for y in range(1, mapHeight):
      if (x, y) not in wallList:
        enemyMiddleLine.append((x, y))
    return enemyMiddleLine

def getMapMatrix(gameState):
  """
  Start counting from the top-left corner

  0 1 2 ➡
  1
  2
  ⬇

  0: Walls
  1: Available movements
  2: RedFood
  3: RedCapsule
  4: BlueFood
  5: BlueCapsule
  """
  mapMatrix = gameState.deepCopy().data.layout.layoutText
  mapHeight = len(mapMatrix)
  for i in range(mapHeight):
    mapMatrix[i] = mapMatrix[i].replace('%', '0')
    mapMatrix[i] = mapMatrix[i].replace(' ', '1')
    mapMatrix[i] = mapMatrix[i].replace('.', '1')
    mapMatrix[i] = mapMatrix[i].replace('o', '1')
    mapMatrix[i] = mapMatrix[i].replace('1', '1')
    mapMatrix[i] = mapMatrix[i].replace('2', '1')
    mapMatrix[i] = mapMatrix[i].replace('3', '1')
    mapMatrix[i] = mapMatrix[i].replace('4', '1')
    mapMatrix[i] = list(mapMatrix[i])
    mapMatrix[i] = list(map(float, mapMatrix[i]))
  for redFood in gameState.getRedFood().asList():
    x = redFood[0]
    y = mapHeight - 1 - redFood[1]
    mapMatrix[y][x] = 2.0
  for redCapsule in gameState.getRedCapsules():
    if not redCapsule:
      continue
    x = redCapsule[0]
    y = mapHeight - 1 - redCapsule[1]
    mapMatrix[y][x] = 3.0
  for blueFood in gameState.getBlueFood().asList():
    x = blueFood[0]
    y = mapHeight - 1 - blueFood[1]
    mapMatrix[y][x] = 4.0
  for blueCapsule in gameState.getBlueCapsules():
    if not blueCapsule:
      continue
    x = blueCapsule[0]
    y = mapHeight - 1 - blueCapsule[1]
    mapMatrix[y][x] = 5.0
  return mapMatrix
