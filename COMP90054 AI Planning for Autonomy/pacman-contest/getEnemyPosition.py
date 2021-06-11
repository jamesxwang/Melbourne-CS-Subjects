        # ['agentDistances', 'blueTeam', 'data', 'deepCopy', 'generateSuccessor', 'getAgentDistances', 'getAgentPosition',
        #  'getAgentState', 'getBlueCapsules', 'getBlueFood', 'getBlueTeamIndices', 'getCapsules', 'getDistanceProb',
        #  'getInitialAgentPosition', 'getLegalActions', 'getNumAgents', 'getRedCapsules', 'getRedFood', 'getRedTeamIndices',
        #  'getScore', 'getWalls', 'hasFood', 'hasWall', 'initialize', 'isOnRedTeam', 'isOver', 'isRed', 'makeObservation',
        #  'redTeam', 'teams']
debug = False

class enemyPosition:
    def __init__(self):
        self.firstEnemy = []
        self.secondEnemy = []
        self.start = True
        self.moves = [(+1,0),(-1,0),(0,+1),(0,-1),(0,0)]
        self.death = {}
    def initial(self,gameState,isRed,map):
        if self.start:
            self.validPosition = self.getValidPosition(map)
            if isRed:
                self.firstEnemyIndex = 1
                self.secondEnemyIndex = 3
            else:
                self.firstEnemyIndex = 0
                self.secondEnemyIndex = 2
            self.enemyInitial = {}
            self.enemyPosition = {}
            self.enemyInitial[self.firstEnemyIndex] = gameState.getInitialAgentPosition(self.firstEnemyIndex)
            self.enemyInitial[self.secondEnemyIndex] = gameState.getInitialAgentPosition(self.secondEnemyIndex)
            self.enemyPosition[self.firstEnemyIndex] = []
            self.enemyPosition[self.secondEnemyIndex] = []
            self.enemyPosition[self.firstEnemyIndex].append(self.enemyInitial[self.firstEnemyIndex])
            self.enemyPosition[self.secondEnemyIndex].append(self.enemyInitial[self.secondEnemyIndex])
            self.death[self.firstEnemyIndex] = 0
            self.death[self.secondEnemyIndex] = 0
        self.start = False
        # print(self.enemyPosition)

    def getValidPosition(self,map):
        validPosition = []
        # print(map)
        for x in range(0,len(map)):
            for y in range(0,len(map[x])):
                if map[x][y] != 0:
                    validPosition.append((y,len(map) - x  -1))
        return validPosition

    def updateWithDeath(self,index):

        if self.death[index] == 0:
            self.enemyPosition[index] = [self.enemyInitial[index]]
            self.death[index] = 4
        # if myTeam.debug:
        # if debug:
        #     print("update with death",self.enemyPosition)

    def updateWithVision(self,index,pos):
        self.enemyPosition[index] = []
        self.enemyPosition[index].append(pos)

    def updateWithEatenFood(self,foodPosition):
        if foodPosition in self.enemyPosition[self.firstEnemyIndex] and (not foodPosition in self.enemyPosition[self.secondEnemyIndex]):
            self.enemyPosition[self.firstEnemyIndex] = []
            self.enemyPosition[self.firstEnemyIndex].append(foodPosition)
        if foodPosition in self.enemyPosition[self.secondEnemyIndex] and (not foodPosition in self.enemyPosition[self.firstEnemyIndex]):
            self.enemyPosition[self.secondEnemyIndex] = []
            self.enemyPosition[self.secondEnemyIndex].append(foodPosition)

    def calculateDistance(self,pos1,pos2):
        return abs(pos1[0] - pos2[0]) + abs(pos1[1] - pos2[1])

    def getNoiseDistance(self,noise,position):
        noiseRange = []
        for pos in self.validPosition:
            if self.calculateDistance(pos,position) >= (noise - 6) and self.calculateDistance(pos,position) <= (noise + 6):
                noiseRange.append(pos)
        return noiseRange

    def checkNoiseDistance(self,noise,pos1,pos2):
        return (self.calculateDistance(pos1,pos2)>= noise - 6) and (self.calculateDistance(pos1,pos2) <= noise + 6)

    def updateWithNoise(self,noiseDistance,index,position,enemyIndex):
        hasMoved = index - 1
        notMoved = index + 1
        if hasMoved< 0:
            hasMoved = 3
        if notMoved > 3:
            notMoved = 0
        before1 = self.enemyPosition[hasMoved]
        before2 = self.enemyPosition[notMoved]
        newEnemyPosition = []
        if enemyIndex == hasMoved:
            for pos in self.enemyPosition[hasMoved]:
                for move in self.moves:
                    newPos = (pos[0] + move[0],pos[1] + move[1])
                    if newPos in self.validPosition and self.checkNoiseDistance(noiseDistance[hasMoved],newPos,position):
                        newEnemyPosition.append(newPos)
            self.enemyPosition[hasMoved] = list(set(newEnemyPosition))
        else:
            for pos in self.enemyPosition[notMoved]:
                if self.checkNoiseDistance(noiseDistance[notMoved],pos,position):
                    newEnemyPosition.append(pos)
            self.enemyPosition[notMoved] = list(set(newEnemyPosition))
        # if myTeam.debug:
        # if debug:
            # print("index:",index,"position",position,"enemyIndex,",enemyIndex,"noise distance",noiseDistance,"enemyPosition:",self.enemyPosition,"before:",before1,before2)
        if len(self.enemyPosition[notMoved]) ==0 or len(self.enemyPosition[hasMoved]) ==0:
            print("????")
        for i in self.death:
            if self.death[i] >0:
                self.death[i] += -1
def getCapsulesRegion(gameState,capsules):
    foodList = self.getFood().asList









