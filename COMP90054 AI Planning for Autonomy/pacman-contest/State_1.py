class State_1:

    def __init__(self, Agent, gameState):
        self.Agent = Agent
        self.gameState = gameState
        self.ourMiddleLine = self.getOurMiddleLine(Agent, gameState)
        self.enemyMiddleLine = self.getEnemyMiddleLine(Agent, gameState)
        self.initEnemyFood = gameState.getRedFood() if Agent.index & 1 else gameState.getBlueFood()
        self.initOurFood =  gameState.getBlueFood() if Agent.index & 1 else gameState.getRedFood()
        self.initEnemyCapsules = gameState.getRedCapsules() if Agent.index & 1 else gameState.getBlueCapsules()
        self.initOurCapsules =  gameState.getBlueCapsules() if Agent.index & 1 else gameState.getRedCapsules()

        # ['agentDistances', 'blueTeam', 'data', 'deepCopy', 'generateSuccessor', 'getAgentDistances', 'getAgentPosition',
        #  'getAgentState', 'getBlueCapsules', 'getBlueFood', 'getBlueTeamIndices', 'getCapsules', 'getDistanceProb',
        #  'getInitialAgentPosition', 'getLegalActions', 'getNumAgents', 'getRedCapsules', 'getRedFood', 'getRedTeamIndices',
        #  'getScore', 'getWalls', 'hasFood', 'hasWall', 'initialize', 'isOnRedTeam', 'isOver', 'isRed', 'makeObservation',
        #  'redTeam', 'teams']

    def getCurrentState(self, initState):
        myIndex = self.Agent.index
        teammateIndex = self.getIndex(2)
        firstEnemyIndex = self.getIndex(1)
        secondEnemyIndex = self.getIndex(3)

        myAgentState = self.gameState.getAgentState(myIndex)
        teammateAgentState = self.gameState.getAgentState(teammateIndex)
        firstEnemyAgentState = self.gameState.getAgentState(firstEnemyIndex)
        secondEnemyAgentState = self.gameState.getAgentState(secondEnemyIndex)

        myInitialAgentPosition = self.gameState.getInitialAgentPosition(myIndex)
        teammateInitialAgentPosition = self.gameState.getInitialAgentPosition(teammateIndex)
        firstEnemyInitialAgentPosition = self.gameState.getInitialAgentPosition(firstEnemyIndex)
        secondEnemyInitialAgentPosition = self.gameState.getInitialAgentPosition(secondEnemyIndex)

        myCurPosition = self.gameState.getAgentPosition(myIndex)
        teammateCurPosition = self.gameState.getAgentPosition(teammateIndex)
        firstEnemyCurPosition = self.gameState.getAgentPosition(firstEnemyIndex)
        secondEnemyCurPosition = self.gameState.getAgentPosition(secondEnemyIndex)

        enemyFood = self.gameState.getRedFood() if self.Agent.index & 1 else self.gameState.getBlueFood()
        ourFood =  self.gameState.getBlueFood() if self.Agent.index & 1 else self.gameState.getRedFood()
        enemyCapsules = self.gameState.getRedCapsules() if self.Agent.index & 1 else self.gameState.getBlueCapsules()
        ourCapsules =  self.gameState.getBlueCapsules() if self.Agent.index & 1 else self.gameState.getRedCapsules()

        ################################################################################################################
        # 分数
        score = self.gameState.data.score

        # 剩余时间
        timeleft = self.gameState.data.timeleft

        # 是不是红方
        isRed = self.gameState.isOnRedTeam(myIndex)
        teammateIsRed = self.gameState.isOnRedTeam(teammateIndex)
        firstEnemyIsRed = self.gameState.isOnRedTeam(firstEnemyIndex)
        secondEnemyIsRed = self.gameState.isOnRedTeam(secondEnemyIndex)

        # 自己是不是鬼
        isGhost = str(myAgentState).startswith('G')
        # 队友是不是鬼
        teammateIsGhost = str(teammateAgentState).startswith('G')
        # 第一个敌人是不是鬼
        firstEnemyIsGhost = str(firstEnemyAgentState).startswith('G')
        # 第二个敌人是不是鬼
        secondEnemyIsGhost = str(secondEnemyAgentState).startswith('G')

        # 自己是不是Pacman
        isPacman = str(myAgentState).startswith('P')
        # 队友是不是Pacman
        teammateIsPacman = str(teammateAgentState).startswith('P')
        # 第一个敌人是不是Pacman
        firstEnemyIsPacman = str(firstEnemyAgentState).startswith('P')
        # 第二个敌人是不是Pacman
        secondEnemyIsPacman = str(secondEnemyAgentState).startswith('P')

        # 自己是不是白鬼
        isScaredGhost = isGhost and (myAgentState.scaredTimer != 0)
        # 队友是不是白鬼
        teammateIsScaredGhost = teammateIsGhost and (teammateAgentState.scaredTimer != 0)
        # 第一个敌人是不是白鬼
        firstEnemyIsScaredGhost = firstEnemyIsGhost and (firstEnemyAgentState.scaredTimer != 0)
        # 第二个敌人是不是白鬼
        secondEnemyIsScaredGhost = secondEnemyIsGhost and (secondEnemyAgentState.scaredTimer != 0)

        # 自己离我方出生地距离
        myDistanceFromHome = self.Agent.getMazeDistance(myInitialAgentPosition, myCurPosition)
        # 队友离我方出生地距离
        teammateDistanceFromHome = self.Agent.getMazeDistance(teammateInitialAgentPosition, teammateCurPosition)
        # 第一个敌人离我方出生地距离
        firstEnemyDistanceFromHome = self.Agent.getMazeDistance(firstEnemyInitialAgentPosition, firstEnemyCurPosition) if firstEnemyCurPosition else -1
        # 第二个敌人离我方出生地距离
        secondEnemyDistanceFromHome = self.Agent.getMazeDistance(secondEnemyInitialAgentPosition, secondEnemyCurPosition) if secondEnemyCurPosition else -1

        # 自己离第一个敌人的真实距离
        distanceBetweenMeAndFirstEnemy = self.Agent.getMazeDistance(myCurPosition, firstEnemyCurPosition) if firstEnemyCurPosition else -1
        # 自己离第二个敌人的真实距离
        distanceBetweenMeAndSecondEnemy = self.Agent.getMazeDistance(myCurPosition, secondEnemyCurPosition) if secondEnemyCurPosition else -1
        # 自己离队友的真实距离
        distanceBetweenMeAndTeammate = self.Agent.getMazeDistance(myCurPosition, teammateCurPosition)

        # 自己与第一个敌人的noisy distance
        noisyDistanceBetweenMeAndFirstEnemy = self.gameState.agentDistances[firstEnemyIndex]
        # 自己与第一个敌人的noisy distance
        noisyDistanceBetweenMeAndSecondEnemy = self.gameState.agentDistances[secondEnemyIndex]

        # 自己距离我方中线的最近点距离
        distanceToOurMiddleLine = min(map(lambda x: self.Agent.getMazeDistance(myCurPosition, x), initState['ourMiddleLine']))
        # 自己距离敌方中线的最近点距离
        distanceToEnemyMiddleLine = min(map(lambda x: self.Agent.getMazeDistance(myCurPosition, x), initState['enemyMiddleLine']))

        # 自己距离敌方最近capsule的距离
        distanceToEnemyCapsule = min(map(lambda x: self.Agent.getMazeDistance(myCurPosition, x), enemyCapsules)) if enemyCapsules else 100
        # 第一个敌人距离我方capsule的最近距离
        distanceFromFirstEnemyToOurCapsule = min(map(lambda x: self.Agent.getMazeDistance(firstEnemyCurPosition, x), ourCapsules)) if firstEnemyCurPosition and ourCapsules else 100
        # 第二个敌人距离我方capsule的最近距离
        distanceFromSecondEnemyToOurCapsule = min(map(lambda x: self.Agent.getMazeDistance(secondEnemyCurPosition, x), ourCapsules)) if secondEnemyCurPosition and ourCapsules else 100

        # 我方剩余豆子个数
        ourRestFoodCount = ourFood.count()
        # 敌方剩余豆子个数
        enemyRestFoodCount = enemyFood.count()
        # 我方豆子总数
        ourFoodTotal = initState['initOurFood'].count()
        # 敌方豆子总数
        enemyFoodTotal = initState['initEnemyFood'].count()

        # 自己身上吃了几个豆子
        foodCarrying = self.gameState.data.agentStates[myIndex].numCarrying

        # 敌人无敌时间
        myScaredTimer = self.gameState.data.agentStates[myIndex].scaredTimer
        # 自己无敌时间
        myInvincibleTimer = min(self.gameState.data.agentStates[firstEnemyIndex].scaredTimer, self.gameState.data.agentStates[secondEnemyIndex].scaredTimer)

        __enemyNearestFoodAndDistance = self.getEnemyNearestFood(self.Agent.getMazeDistance, ourFood, firstEnemyCurPosition, secondEnemyCurPosition)
        # 敌人到我方豆子最近到豆子(如果可见到话), 坐标从左上角开始
        distanceFromEnemyToOurFood = __enemyNearestFoodAndDistance[0]
        # 我方到离敌人最近豆子到距离
        distanceToEnemyNearestFood = min(self.Agent.getMazeDistance(myCurPosition, __enemyNearestFoodAndDistance[1]), self.Agent.getMazeDistance(teammateCurPosition, __enemyNearestFoodAndDistance[1])) if __enemyNearestFoodAndDistance[1] else 100

        currentState = [
            score,
            timeleft,
            isRed,
            teammateIsRed,
            firstEnemyIsRed,
            secondEnemyIsRed,
            isGhost,
            teammateIsGhost,
            firstEnemyIsGhost,
            secondEnemyIsGhost,
            isPacman,
            teammateIsPacman,
            firstEnemyIsPacman,
            secondEnemyIsPacman,
            isScaredGhost,
            teammateIsScaredGhost,
            firstEnemyIsScaredGhost,
            secondEnemyIsScaredGhost,
            myDistanceFromHome,
            teammateDistanceFromHome,
            firstEnemyDistanceFromHome,
            secondEnemyDistanceFromHome,
            distanceBetweenMeAndFirstEnemy,
            distanceBetweenMeAndSecondEnemy,
            distanceBetweenMeAndTeammate,
            noisyDistanceBetweenMeAndFirstEnemy,
            noisyDistanceBetweenMeAndSecondEnemy,
            distanceToOurMiddleLine,
            distanceToEnemyMiddleLine,
            distanceToEnemyCapsule,
            distanceFromFirstEnemyToOurCapsule,
            distanceFromSecondEnemyToOurCapsule,
            ourRestFoodCount,
            enemyRestFoodCount,
            ourFoodTotal,
            enemyFoodTotal,
            foodCarrying,
            myScaredTimer,
            myInvincibleTimer,
            distanceFromEnemyToOurFood,
            distanceToEnemyNearestFood
        ]

        return currentState
        ################################################################################################################


    def getIndex(self, index):
        new_index = index + self.Agent.index
        if new_index>3:
            new_index = new_index-4
        return new_index

    def getEnemyNearestFood(self, getMazeDistance, ourFood, firstEnemyCurPosition, secondEnemyCurPosition):
        minDistance = 100
        minFood = None
        for i, food in enumerate(ourFood.asList()):
            dis1 = getMazeDistance(firstEnemyCurPosition, food) if firstEnemyCurPosition else 100
            dis2 = getMazeDistance(secondEnemyCurPosition, food) if secondEnemyCurPosition else 100
            dis = min(dis1, dis2)
            if dis < minDistance:
                minDistance = dis
                minFood = food
        return (minDistance, minFood)

    def getOurMiddleLine(self, Agent, gameState):
        middleLine = []
        mapWidth = gameState.data.layout.width
        mapHeight = gameState.data.layout.height
        if Agent.red:
          x = int((mapWidth - 2) / 2)
        else:
          x = int((mapWidth - 2) / 2 + 1)
        wallList = gameState.getWalls().asList()
        for y in range(1, mapHeight):
          if (x, y) not in wallList:
            middleLine.append((x,y))
        return middleLine

    def getEnemyMiddleLine(self, Agent, gameState):
        middleLine = []
        mapWidth = gameState.data.layout.width
        mapHeight = gameState.data.layout.height
        if Agent.red:
            x = int((mapWidth - 2) / 2 + 1)
        else:
            x = int((mapWidth - 2) / 2 - 1)
        wallList = gameState.getWalls().asList()
        for y in range(1, mapHeight):
            if (x, y) not in wallList:
                middleLine.append((x,y))
        return middleLine

    def getInitEnemyFood(self, Agent, gameState):
        return gameState.getRedFood() if Agent.index & 1 else gameState.getBlueFood()

    def getInitialStates_1(self):
        state = {}
        state['ourMiddleLine'] = self.ourMiddleLine
        state['enemyMiddleLine'] = self.enemyMiddleLine
        state['initEnemyFood'] = self.initEnemyFood
        state['initOurFood'] = self.initOurFood
        state['initEnemyCapsules'] = self.initEnemyCapsules
        state['initOurCapsules'] = self.initOurCapsules
        return state
