class State_2:

    def __init__(self, Agent, gameState, score, foodCarrying):
        self.Agent = Agent
        self.gameState = gameState
        self.myIndex = Agent.index
        # 自己的位置 (左下角开始)
        self.myCurPosition = gameState.getAgentPosition(self.myIndex)
        # 豆子位置 (左上角)
        self.enemyFood = self.Agent.getFood(self.gameState).asList()
        # 中线位置
        self.middleLineX = int((gameState.data.layout.width - 2) / 2 ) if Agent.red else int((gameState.data.layout.width - 2) / 2 + 1)
        # 分数
        self.prevScore = score
        self.curScore = gameState.getScore()
        # 自己吃了多少豆子
        self.prevFoodCarrying = foodCarrying
        self.curFoodCarrying = gameState.data.agentStates[self.myIndex].numCarrying

    def isTerminal(self):

        return False
            # (self.prevFoodCarrying > 0) and (self.myCurPosition[0] == self.middleLineX)
            # self.curScore != self.prevScore

    def getPossibleActions(self):
        actions = self.gameState.getLegalActions(self.Agent.index)
        actions.remove('Stop')
        # print(self.Agent.index, self.myCurPosition, actions) if not self.Agent.index & 1 else None
        return actions

    def takeAction(self, action):
        return State_2(self.Agent, self.gameState.generateSuccessor(self.myIndex, action), self.curScore, self.curFoodCarrying)

    def getReward(self):
        discountFactor = 0.001
        reward = 0
        distMid = 0
        minDist = 100
        for i in self.getOurMiddleLine():
            minDist = min(minDist,self.Agent.distancer.getDistance(self.myCurPosition,i))
        distMid = discountFactor * (100 - minDist)*0.1
        # distMid = discountFactor * (100-min(map(lambda x: self.Agent.distancer.getDistance(self.myCurPosition, x), self.getOurMiddleLine())))
        # carryChange = abs(self.curFoodCarrying - self.prevFoodCarrying)
        # minDist = 100
        # visted = []
        # visted.append(self.myCurPosition)
        # dist = 0
        # for i in self.enemyFood:
        #     minDist = 999
        #     for j in visted:
        #         minDist = min(minDist,self.Agent.distancer.getDistance(i,j))
        #     dist += minDist
        #     visted.append(i)
        #     self.enemyFood.remove(i)
        minDist = 1000
        for i in self.enemyFood:
            minDist = min(minDist,self.Agent.distancer.getDistance(self.myCurPosition,i))
        distFood = discountFactor* (100 - minDist)
        # if self.enemyFood:
        #     distFood = 10*discountFactor * (100-min(map(lambda x: self.Agent.distancer.getDistance(self.myCurPosition, x), self.enemyFood)))
        carry = self.curFoodCarrying*0.3
        score = self.gameState.getScore()
        reward = distFood + carry + score + distMid
        # reward = score / 100
        # print("index",self.myIndex,"Total:", reward, "Position:",self.myCurPosition," mid: ", distMid," food:", distFood, " carry:",carry," score:",score,"midline",self.getOurMiddleLine())
        # if (self.myCurPosition[0] == self.middleLineX) and (self.prevFoodCarrying > 0 ):
        #     print(self.gameState.getScore())
        #     print(self.myCurPosition[0],self.middleLineX)
        #     reward += self.prevFoodCarrying*5
        #     print(reward,self.prevFoodCarrying,self.myCurPosition)
        # reward += (self.curScore - self.prevScore)*10
        # print("get score:",self.curScore-self.prevScore)

        return reward

    def getIndex(self, index):
        new_index = index + self.Agent.index
        if new_index > 3:
            new_index = new_index - 4
        return new_index

    def returnedHome(self, x):
        return x <= self.middleLineX if self.Agent.red else x >= self.middleLineX

    def getOurMiddleLine(self):
        middleLine = []
        mapWidth = self.gameState.data.layout.width
        mapHeight = self.gameState.data.layout.height
        if self.Agent.red:
          x = int((mapWidth - 2) / 2 )
        else:
          x = int((mapWidth - 2) / 2 + 1)
        wallList = self.gameState.getWalls().asList()
        for y in range(1, mapHeight):
          if (x, y) not in wallList:
            middleLine.append((x,y))
        return middleLine
