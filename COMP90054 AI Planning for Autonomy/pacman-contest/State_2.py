class State_2:

    def __init__(self,
                 gameState,
                 score,
                 foodCarrying,
                 index,
                 isRed,
                 enemyFoodList,
                 getDistance,
                 middleLineX,
                 ourMiddleLine):
        self.gameState = gameState
        self.myIndex = index
        self.isRed = isRed
        self.getDistance = getDistance
        self.firstEnemyIndex = self.getIndex(1)
        self.secondEnemyIndex = self.getIndex(3)
        # 自己的位置 (左下角开始)
        self.myCurPosition = gameState.getAgentPosition(self.myIndex)
        # 豆子位置 (左下角)
        self.enemyFoodList = enemyFoodList
        # 中线x位置
        self.middleLineX = middleLineX
        self.ourMiddleLine = ourMiddleLine
        # 分数
        self.prevScore = score
        self.curScore = gameState.getScore()
        # 自己吃了多少豆子
        self.prevFoodCarrying = foodCarrying
        self.curFoodCarrying = gameState.data.agentStates[self.myIndex].numCarrying

    # def isTerminal(self):
    #     return False

    def getPossibleActions(self):
        actions = self.gameState.getLegalActions(self.myIndex)
        actions.remove('Stop')
        return actions

    def takeAction(self, action):
        gameState = self.gameState.generateSuccessor(self.myIndex, action)
        gameStateAfterEnemyMove = self.getGameStateAfterEnemyWalkTowardsMe(gameState)
        return State_2(gameStateAfterEnemyMove,
                       self.curScore,
                       self.curFoodCarrying,
                       self.myIndex,
                       self.isRed,
                       self.enemyFoodList,
                       self.getDistance,
                       self.middleLineX,
                       self.ourMiddleLine)

    def getGameStateAfterEnemyWalkTowardsMe(self, gameState):
        return gameState
        firstEnemyPos = self.gameState.getAgentPosition(self.firstEnemyIndex)
        secondEnemyPos = self.gameState.getAgentPosition(self.secondEnemyIndex)
        if not firstEnemyPos and not secondEnemyPos:
            return gameState
        else:
            newState = gameState
            if firstEnemyPos:
                enemyNewPos = []
                firstActions = newState.getLegalActions(self.firstEnemyIndex)
                for action in firstActions:
                    firstEnemyNewPos = self._getPositionFromAction(firstEnemyPos, action)
                    print(firstEnemyNewPos, action)
                    dis = self.getDistance(self.myCurPosition, firstEnemyNewPos)
                    enemyNewPos.append((firstEnemyNewPos, action, dis))
                minD = 999999
                act = None
                for i, item in enumerate(enemyNewPos):
                    if item[2] < minD:
                        minD = item[2]
                        act = item[1]
                newState = newState.generateSuccessor(self.firstEnemyIndex, act)
            if secondEnemyPos:
                enemyNewPos = []
                secondActions = newState.getLegalActions(self.secondEnemyIndex)
                for action in secondActions:
                    secondEnemyNewPos = self._getPositionFromAction(secondEnemyPos, action)
                    dis = self.getDistance(self.myCurPosition, secondEnemyNewPos)
                    enemyNewPos.append((secondEnemyNewPos, action, dis))
                minD = 999999
                act = None
                for i, item in enumerate(enemyNewPos):
                    if item[2] < minD:
                        minD = item[2]
                        act = item[1]
                newState = newState.generateSuccessor(self.secondEnemyIndex, act)
        return newState

    def _getPositionFromAction(self, pos, action):
        x,y = pos
        if action == 'North':
            newPos = (x,y+1)
        elif action == 'South':
            newPos = (x,y-1)
        elif action == 'East':
            newPos = (x+1,y)
        elif action == 'West':
            newPos = (x-1,y)
        else:
            newPos = pos
        return newPos

    def getReward(self):
        discountFactor = 0.001
        reward = 0
        distMid = 0
        minDist = 100
        for i in self.ourMiddleLine:
            minDist = min(minDist,self.getDistance(self.myCurPosition,i))
        distMid = discountFactor * (100 - minDist)*0.1
        minDist = 1000
        for i in self.enemyFoodList:
            minDist = min(minDist,self.getDistance(self.myCurPosition,i))
        distFood = discountFactor* (100 - minDist)
        carry = self.curFoodCarrying*0.3
        score = self.gameState.getScore()
        reward = distFood + carry + score + distMid

        # for i,row in enumerate(self.mapMatrix):
        #     print(row)
        # print('-'*40)

        # reward = score / 100
        # print("index",self.myIndex,"Total:", reward, "Position:",self.myCurPosition," mid: ", distMid," food:", distFood, " carry:",carry," score:",score)
        return reward

    def getIndex(self, index):
        new_index = index + self.myIndex
        if new_index > 3:
            new_index = new_index - 4
        return new_index

    def returnedHome(self, x):
        return x <= self.middleLineX if self.isRed else x >= self.middleLineX

    def isTerminal(self):
        if self.curScore - self.prevScore > 0:
            return True
        return False

    def getOurMiddleLine(self):
        middleLine = []
        mapWidth = self.gameState.data.layout.width
        mapHeight = self.gameState.data.layout.height
        if self.isRed:
          x = int((mapWidth - 2) / 2)
        else:
          x = int((mapWidth - 2) / 2 + 1)
        wallList = self.gameState.getWalls().asList()
        for y in range(1, mapHeight):
          if (x, y) not in wallList:
            middleLine.append((x,y))
        return middleLine
