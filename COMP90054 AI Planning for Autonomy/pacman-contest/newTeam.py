# myTeam.py
# ---------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
#
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).
from util import manhattanDistance
import sys
sys.path.append('teams/kdbnb/')
import copy
import InitialMap
from captureAgents import CaptureAgent
import myProblem
import random
import getEnemyPosition
import time
import util
import collections
import gameData
#################
# Team creation #
#################
# debug = True
debug = False
enemyPosition = getEnemyPosition.enemyPosition()
gameData = gameData.gamedata()
deadEnemy = {}
teammateState = {}
enemyCarryFoodNumber = collections.defaultdict(float)
actionHistory = {}
agentMod = {}
positionHistory  ={}


def createTeam(firstIndex, secondIndex, isRed,
               first='AttackAgent', second='AttackAgent'):
    """
    This function should return a list of two agents that will form the
    team, initialized using firstIndex and secondIndex as their agent
    index numbers.  isRed is True if the red team is being created, and
    will be False if the blue team is being created.

    As a potentially helpful development aid, this function can take
    additional string-valued keyword arguments ("first" and "second" are
    such arguments in the case of this function), which will come from
    the --redOpts and --blueOpts command-line arguments to capture.py.
    For the nightly contest, however, your team will be created without
    any extra arguments, so you should make sure that the default
    behavior is what you want for the nightly contest.
    """

    # The following line is an example only; feel free to change it.
    return [eval(first)(firstIndex), eval(second)(secondIndex)]

##########
# Agents #
##########

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

class AttackAgent(CaptureAgent):
    def registerInitialState(self, gameState):
        CaptureAgent.registerInitialState(self, gameState)  # must be put ahead to set value of self.red
        self.walls = gameState.getWalls()
        self.midX = self.getMiddleX(gameState)
        self.enemyMidX = self.getEnemyMiddleX(gameState)
        self.midLine = self.getMiddleLine(gameState)
        self.enemyMidLine = self.getEnemyMiddleLine(gameState)
        self.mapMatrix = getMapMatrix(gameState)
        self.capsules = self.getCapsules(gameState)
        self.deadEnd = InitialMap.searchDeadEnd(self.mapMatrix)  # keys are deadEnds, values are corresponding depth
        self.sumOfFood = len(self.getFood(gameState).asList())
        # used for separate pacman
        self.randomFoodIndex = random.randint(0, self.sumOfFood - 1)
        # print(self.randomFoodIndex)
        self.randomSelectFood = True
        self.allienIndex = (self.index + 2) % 4
        self.lastAction = None
        self.manhattanSight = self.getManhattanSight()
        teammateState[self.index] = gameState
        positionHistory[self.index] = []
        if self.red:
            self.ourRegionX = range(0,self.midX + 1)
            self.enemyRegionX = range(self.midX + 1,len(self.mapMatrix[0]))
        else:
            self.enemyRegionX = range(0,self.midX)
            self.ourRegionX = range(self.midX,len(self.mapMatrix[0]))
        # print(self.enemyRegionX,self.ourRegionX)
        # for x in self.ourRegionX:
        #     for y in range(0,len(self.mapMatrix)):
        #         self.debugDraw((x,y),[1,0,0])
        if self.red:
            self.enemyIndex = [1, 3]
            deadEnemy[1] = 0
            deadEnemy[3] = 0
        else:
            self.enemyIndex = [0, 2]
            deadEnemy[0] = 0
            deadEnemy[2] = 0
        if gameState.data.timeleft == 1200:
            enemyPosition.start = True
            enemyPosition.initial(gameState, self.red, self.mapMatrix)
        self.enemyDied = {}
        self.enemyDied[self.enemyIndex[0]] = False
        self.enemyDied[self.enemyIndex[1]] = False
        # if debug:
        #     for i in self.deadEnd:
        #         # x = i[1]
        #         # y = len(self.mapMatrix) - i[0] -1
        #         if i[0] <= self.midX:
        #             self.debugDraw(i, [self.deadEnd[i] / 100 + 0.3, 0, 0])
        #         else:
        #             self.debugDraw(i, [0, self.deadEnd[i] / 100 + 0.3, 0])
        actionHistory[self.index] = []
        agentMod[self.index] = []
        # self.foodRegion = InitialMap.getFoodRegion(self,gameState)
        # foodCluster = InitialMap.cluster1(gameState,self.getFood(gameState).asList()[0],self)
        # for i in foodCluster:
        #     self.debugDraw(i,[0.3,.4,.5])
        # self.debugDraw(self.getFood(gameState).asList()[0],[0.9,0.6,0.1])
    def getMiddleX(self, gameState):
        mapWidth = gameState.data.layout.width
        if self.red:
            x = int((mapWidth - 2) / 2)
        else:
            x = int((mapWidth - 2) / 2 + 1)
        return x

    def getEnemyMiddleX(self, gameState):  # x of middle line on enemy's side
        mapWidth = gameState.data.layout.width
        if self.red:
            enemyX = int((mapWidth - 2) / 2 + 1)
        else:
            enemyX = int((mapWidth - 2) / 2)
        return enemyX

    # def inLoop(self):
    #     loopCheck = [positionHistory[self.index][:-1]]
    #     loopCheck2 = []
    #     loopNum = 0
    #     for i,pos in enumerate(reversed(positionHistory[self.index])[1:20]):
    #         loopCheck2.append(pos)
    #         if pos == loopCheck:
    #             loopNum += 1
    #             loopCheck = []


    def getMiddleLine(self, gameState):
        midLine = []
        mapHeight = gameState.data.layout.height
        x = self.midX
        wallList = gameState.getWalls().asList()
        for y in range(1, mapHeight):
            if (x, y) not in wallList:
                midLine.append((x, y))
        return midLine

    def getEnemyMiddleLine(self, gameState):
        enemyMidLine = []
        mapHeight = gameState.data.layout.height
        x = self.enemyMidX
        wallList = gameState.getWalls().asList()
        for y in range(1, mapHeight):
            if (x, y) not in wallList:
                enemyMidLine.append((x, y))
        return enemyMidLine

    def getIndex(self, index):  # 0:self;2:teammate;1/3:enemy
        new_index = index + self.index
        if new_index > 3:
            new_index = new_index - 4
        return new_index

    def getMinDistToEnemy(self, curPos, enemyList):
        curMinDist = 99999
        for enemy in enemyList:
            dist = self.distancer.getDistance(enemy,curPos)
            curMinDist = min(dist, curMinDist)
        return curMinDist

    def getAgentIndexCloseToTarget(self, gameState, curPos, teammatePos, targetList):
        curMinDist = 99999
        for target in targetList:
            dist = self.distancer.getDistance(curPos, target)
            curMinDist = min(dist, curMinDist)
        teammateMinDist = 99999
        for target in targetList:
            dist = self.distancer.getDistance(teammatePos, target)
            teammateMinDist = min(dist, teammateMinDist)
        if curMinDist <= teammateMinDist:
            return self.index
        else:
            return self.getIndex(2)

    # def capsuleEatenLastMove(self, gameState):
    #   prevGameState = self.getPreviousObservation()
    #   if prevGameState != None:
    #     prevCapsules = self.getCapsules(prevGameState)
    #     curCapsules = self.getCapsules(gameState)
    #     for capsule in prevCapsules:
    #       if capsule not in curCapsules:
    #         return True
    #   return False

    def pacmanEnemy(self, enemyList):
        res = []
        for pos in enemyList:
            if self.red:
                if pos != None and pos[0] <= self.midX:
                    res.append(pos)
            else:
                if pos != None and pos[0] >= self.midX:
                    res.append(pos)
        return res

    def ghostEnemy(self, enemyList):
        res = []
        for pos in enemyList:
            if self.red:
                if pos != None and pos[0] >= self.enemyMidX:
                    res.append(pos)
            else:
                if pos != None and pos[0] <= self.enemyMidX:
                    res.append(pos)
        return res

    def enemySucide(self, gameState):
        eatEnemy = {}
        for i in self.enemyIndex:
            eatEnemy[i] = False
        if gameState.data.timeleft < 1190:
            preGameState = self.getPreviousObservation()
            curPos = gameState.getAgentPosition(self.index)
            curPosTeammate = gameState.getAgentPosition(self.allienIndex)
            for enemyIndex in self.enemyIndex:
                curPosE = gameState.getAgentPosition(enemyIndex)
                prePosE = preGameState.getAgentPosition(enemyIndex)
                if curPosE is None and (not prePosE is None):
                    distance1 = self.distancer.getDistance(curPos, prePosE)
                    distance2 = self.distancer.getDistance(curPosTeammate, prePosE)
                    if (distance1 == 1) or (distance2 == 1):
                        eatEnemy[enemyIndex] = True
        return eatEnemy

    def eatEnemy1(self, gameState, action):
        eatEnemy = {}
        nextGameState = gameState.generateSuccessor(self.index, action)
        nextPos = nextGameState.getAgentPosition(self.index)
        for index in self.enemyIndex:
            curPosE = gameState.getAgentPosition(index)
            nextPosE = nextGameState.getAgentPosition(index)
            # if debug:
            # print("Sel next pos:", nextPos, "Enemy Current", curPosE, "Enemy Next", nextPosE)
            if (gameState.getInitialAgentPosition(index) == nextPosE):
                eatEnemy[index] = True
            else:
                eatEnemy[index] = False
        return eatEnemy

    def eatEnemy(self, gameState):
        eatEnemy = {}
        preGameState = self.getPreviousObservation()
        for index in self.enemyIndex:
            eatEnemy[index] = False
            if gameState.data.timeleft < 1190:
                enemyCur = gameState.getAgentPosition(index)
                enemyPre = preGameState.getAgentPosition(index)
                posPre = preGameState.getAgentPosition(self.index)
                allienPre = preGameState.getAgentPosition(self.allienIndex)
                if enemyCur is None and (not enemyPre is None) and (
                    gameState.getAgentPosition(self.index) != gameState.getInitialAgentPosition(self.index)):
                    distance = self.distancer.getDistance(posPre, enemyPre)
                    distance2 = self.distancer.getDistance(allienPre, enemyPre)
                    if (distance < 2) or (distance2 < 2):
                        eatEnemy[index] = True
                    else:
                        eatEnemy[index] = False
                else:
                    if (not enemyPre is None) and (enemyCur is None):
                        distance = self.distancer.getDistance(posPre, enemyPre)
                        if distance > 2:
                            eatEnemy[index] = True
                        else:
                            eatEnemy[index] = False
                    else:
                        eatEnemy[index] = False
        return eatEnemy

    def getEnemyTrueP(self, gameState):
        enemyPosition = {}
        if self.red:
            enemyPosition[1] = gameState.getAgentPosition(1)
            enemyPosition[3] = gameState.getAgentPosition(3)
        else:
            enemyPosition[0] = gameState.getAgentPosition(0)
            enemyPosition[2] = gameState.getAgentPosition(2)
        return enemyPosition

    def foodBeenEaten(self, gameState):
        if gameState.data.timeleft < 1190:
            curFoods = self.getFoodYouAreDefending(gameState).asList()
            preFoods = self.getFoodYouAreDefending(teammateState[self.allienIndex]).asList()
        else:
            return set()
        return set(preFoods) - set(curFoods)

    def capsuleBeenEaten(self, gameState):
        if gameState.data.timeleft < 1190:
            curCap = self.getCapsulesYouAreDefending(gameState)
            preCap = self.getCapsulesYouAreDefending(self.getPreviousObservation())
        else:
            return set()
        return set(preCap) - set(curCap)

    def getEnemyPosition(self, gameState):
        curPos = gameState.getAgentPosition(self.index)
        noiseDistance = gameState.agentDistances
        if gameState.data.timeleft < 1200:
            eatEnemy = self.enemySucide(gameState)
            # if debug:
            #     print(eatEnemy)
            for i in eatEnemy:
                if eatEnemy[i] and deadEnemy[i] == 0:
                    enemyPosition.updateWithDeath(i)
                    deadEnemy[i] = 4
                else:
                    enemyPosition.updateWithNoise(noiseDistance, self.index, curPos, i)
            enemyTruePosition = self.getEnemyTrueP(gameState)
            for i in enemyTruePosition:
                if not enemyTruePosition[i] is None:
                    enemyPosition.updateWithVision(i, enemyTruePosition[i])
            if len(self.foodBeenEaten(gameState)) != 0:
                enemyPosition.updateWithEatenFood(list(self.foodBeenEaten(gameState))[0])
            if len(self.capsuleBeenEaten(gameState)) != 0:
                enemyPosition.updateWithEatenFood(list(self.capsuleBeenEaten(gameState))[0])
            a = enemyPosition.enemyPosition
            # if debug:
            #     self.debugClear()
            #     for i in a[self.enemyIndex[0]]:
            #         self.debugDraw(i, [0, .3, .9])
            #     # for i in enemyPosition.validPosition:
            #     #   self.debugDraw(i,[0,0,1])
            #     for i in a[self.enemyIndex[1]]:
            #         self.debugDraw(i, [.1, .75, .7])

    def getEnemyInRegion(self,gameState,enemyPosition):
        enemyInRegion = {}
        enemy = {}
        for index in self.enemyIndex:
            enemyInRegion[index] = [0,0]
            for pos in enemyPosition[index]:
                if pos[0] in self.ourRegionX:
                    enemyInRegion[index][0] += 1
                else:
                    enemyInRegion[index][1] +=1
            total = enemyInRegion[index][0]+enemyInRegion[index][1]
            pro = enemyInRegion[index][0]/total
            # print(enemyInRegion[index][0],enemyInRegion[index][1])
            if pro>0.8:
                enemy[index] = "Our"
            else:
                if pro > 0.5:
                    enemy[index] = "Mid"
                else:
                    enemy[index] = "Enemy"
        return enemy


    def updateDeath(self, gameState, action):
        enemyDeath = self.eatEnemy1(gameState, action)
        for i in enemyDeath:
            if enemyDeath[i]:
                enemyPosition.updateWithDeath(i)
                deadEnemy[i] = 4


    def getBlockRegions(self,gameState):
        block = []
        cur = gameState.getAgentPosition(self.index)
        for i in self.enemyIndex:
            enemy = gameState.getAgentPosition(i)
            if not enemy is None:
                enemyDistance = self.distancer.getDistance(cur,enemy)
                depth = enemyDistance / 2
                # if debug:
                #     print(self.index,cur,enemy,depth)
                for cell in self.deadEnd:
                    if self.deadEnd[cell] >= depth:
                        block.append(cell)
        return list(set(block))

    def getNeedOfDefenceEnemyPosition(self, gameState, enemyPosition):
        curFoods = self.getFoodYouAreDefending(gameState).asList()
        enemyPositionDict = {}
        for index in self.enemyIndex:
            minDis = 999999
            minPos = None
            for food in curFoods:
                for pos in enemyPosition[index]:
                    dis = self.distancer.getDistance(food, pos)
                    if dis < minDis:
                        minDis = dis
                        minPos = pos
            enemyPositionDict[index] = minPos
        # if debug:
        #     for i in enemyPositionList:
        #         self.debugDraw(i, [.9,0,0]) if i else None
        return enemyPositionDict

    def getFoodFarFromEnemy(self, gameState, curPos, enemyPositionToDefend):
        curFoods = self.foodGrid.asList()
        minDis = 999999
        minPos = None
        enemyList = []
        for i in enemyPositionToDefend:
            enemyList.append(enemyPositionToDefend[i])
        for i in enemyList:
            # print("enemyList",enemyList)
            if debug:
                self.debugDraw(i,[0,0.8,.4])
        for i in curFoods:
            distanceToFood = self.distancer.getDistance(curPos, i)
            distanceToGhost = min(map(lambda x: self.distancer.getDistance(x, curPos), enemyList))
            dis = distanceToFood - distanceToGhost
            # print(dis)
            if dis < minDis:
                minDis = dis
                minPos = i
        if debug:
            self.debugDraw(minPos, [.98,.41,.07]) if minPos else None
        return minPos

    def getManhattanSight(self):
        v = []
        for i in range(-5,6):
            for j in range(-5, 6):
                if abs(i) + abs(j) <= 5:
                    v.append((i,j))
        return v

    def getEnemySight(self, enemyPosition):
        sights = {}
        sight = collections.defaultdict(int)
        # print("enemy Position",enemyPosition)
        for index in self.enemyIndex:
            for enemyX, enemyY in enemyPosition[index]:
                for sightX, sightY in self.manhattanSight:
                    s = (enemyX + sightX, enemyY + sightY)
                    if s[0] in range(0,len(self.mapMatrix[0])+1) and s[1] in range(0,len(self.mapMatrix)+1):
                        sight[s] += 1
            sights[index] = sight
        return sights

    def curInsightOfEnemy(self, curPos, enemyList):
        insight = False
        for enemy in enemyList:
            if enemy != None:
                insight = insight or (manhattanDistance(curPos, enemy) <= 5)
        return insight

    def scoreChanged(self, gameState):
        myCurScore = 0
        teammateNextMoveScore = 0
        if gameState.data.timeleft < 1190:
            myCurScore = gameState.data.score
            teammateNextMoveScore = teammateState[self.allienIndex].data.score
        scoreChange = myCurScore - teammateNextMoveScore
        return scoreChange

    def getEnemyCarryFoodNumber(self, gameState, enemyPosition):
        enemyKeys = list(enemyPosition.keys())
        foodBeenEaten = list(self.foodBeenEaten(gameState))
        if len(foodBeenEaten) != 0:
            food = foodBeenEaten[0]
            if food in enemyPosition[enemyKeys[0]] and food not in enemyPosition[enemyKeys[1]]:
                enemyCarryFoodNumber[enemyKeys[0]] += 1
            elif food in enemyPosition[enemyKeys[1]] and food not in enemyPosition[enemyKeys[0]]:
                enemyCarryFoodNumber[enemyKeys[1]] += 1
            elif food in enemyPosition[enemyKeys[0]] and food in enemyPosition[enemyKeys[1]]:
                enemyCarryFoodNumber[enemyKeys[1]] += 0.5
        scoreChanged = self.scoreChanged(gameState)
        if scoreChanged:
            for pos in enemyPosition[enemyKeys[0]]:
                if pos in self.enemyMidLine:
                    enemyCarryFoodNumber[enemyKeys[0]] = 0
            for pos in enemyPosition[enemyKeys[1]]:
                if pos in self.enemyMidLine:
                    enemyCarryFoodNumber[enemyKeys[1]] = 0
        if deadEnemy[enemyKeys[0]]:
            enemyCarryFoodNumber[enemyKeys[0]] = 0
        if deadEnemy[enemyKeys[1]]:
            enemyCarryFoodNumber[enemyKeys[1]] = 0

    def getClostestMidDistance(self,curPos,gameState):
        minDist = 999
        for i in self.midLine:
            dist = self.distancer.getDistance(curPos,i)
            if minDist > dist:
                minDist = dist
                closestMid = i
        return minDist,closestMid

    def updateEnemyDied(self):
        for index in self.enemyDied:
            if self.enemyDied[index]:
                if self.enemyInRegion[index] != "Enemy":
                    self.enemyDied[index] = False
            else:
                if deadEnemy[index]> 0:
                    self.enemyDied[index] = True
                else:
                    self.enemyDied[index] = False
                if self.enemyDied[index]:
                    print("enemy Died")

    def getSafeFood(self,gameState,block):
        foodList = self.getFood(gameState).asList()
        return list(set(foodList) - set(block))

    def getEnemyAllatHome(self):
        enemyAllHome = True
        for enemy in self.enemyInRegion:
            enemyAllHome = enemyAllHome & (self.enemyInRegion[enemy] != "Our")
        return enemyAllHome

    # def intercept(self,gameState,enemyIndex):
    #     enemyPositionList = enemyPosition.enemyPosition[enemyIndex]
    #     minDist = 999
    #     for i in enemyPositionList:
    #         for j in self.enemyMidLine:
    #             dist = self.distancer.getDistance(i,j)
    #             if minDist > dist:
    #                 minDist = dist
    #                 minMid = j

    def getEnemyNeedToTrace(self):
        minDist = 9999
        for index in self.enemyIndex:
            if self.enemyInRegion[index] == "Our":
                minDist = min(minDist,self.distancer.getDistance(self.enemyPositionsToDefend[index],self.curPos))
                enemy = index
        return enemy

    def getClosedFood(self,gameState,teammateTarget):
        minDist = 999
        foods = self.getFood(gameState).asList()
        food = foods[0]
        foods = list(set(foods) - set(teammateTarget))
        for i in foods:
            dist = self.distancer.getDistance(i,self.curPos)
            if dist < minDist:
                food = i
                minDist = dist
        return food

    def getTeammateTargetRegion(self,gameState):
        foodCluster = []
        if agentMod[self.allienIndex] != []:
            if agentMod[self.allienIndex][0] == "eatFood":
                foodCluster = InitialMap.cluster1(gameState,agentMod[self.allienIndex][1],self)
                # self.debugClear()
                # for i in foodCluster:
                #     self.debugDraw(i,[0.3,self.index / 4,.5])
                # self.debugDraw(agentMod[self.allienIndex][1],[0.9,0.6,0.1])
        return foodCluster
    # def getFoodOutsideSight(self,posSight):
    def getEnemyInsightTogether(self,enemyInsight):
        posInsight = {}
        for i in enemyInsight:
            for j in enemyInsight[i]:
                if j in posInsight:
                    posInsight[j] = posInsight[j] + enemyInsight[i][j]
                else:
                    posInsight[j] = enemyInsight[i][j]
        return posInsight

    def chooseAction(self, gameState):
        self.debugClear()
        self.curPos = gameState.getAgentPosition(self.index)
        teammateCluster = self.getTeammateTargetRegion(gameState)
        closestFood = self.getClosedFood(gameState,teammateCluster)
        # if self.index != 0 :
        #     self.debugDraw(closestFood,[1,0,0.1])
        enemyIndices = self.getOpponents(gameState)
        enemyPos = []
        for idx in enemyIndices:
            enemyPos.append(gameState.getAgentPosition(idx))
        self.foodGrid = self.getFood(gameState)
        s = time.clock()
        self.curPos = gameState.getAgentPosition(self.index)
        self.carryFoods = gameState.data.agentStates[self.index].numCarrying
        # if debug:
        #     print("last action:",self.lastAction)
        for i in deadEnemy:
            if deadEnemy[i] > 0:
                deadEnemy[i] += -1
        curPos = gameState.getAgentPosition(self.index)
        self.block = self.getBlockRegions(gameState)
        # self.debugClear()
        # if debug:
        #     for i in block:
        #         if self.index <2:
        #             self.debugDraw(i, [1, 0, 0])
        #         else:
        #             self.debugDraw(i,[0,1,0])

        teammateIndex = self.getIndex(2)
        teammatePos = gameState.getAgentPosition(teammateIndex)
        # type: 'dict'
        # key: enemyIndex
        # value: list of positions
        self.getEnemyPosition(gameState)
        self.enemyInRegion = self.getEnemyInRegion(gameState,enemyPosition.enemyPosition)
        # print(self.enemyInRegion)
        # ghostEnemy = self.ghostEnemy(enemyPos)
        # pacmanEnemy = self.pacmanEnemy(enemyPos)
        # enemyPositionsToDefend = self.getNeedOfDefenceEnemyPosition(gameState, enemyPosition.enemyPosition)

        posibblePos = set()
        for i in list((enemyPosition.enemyPosition).values()):
            for j in i:
                if j not in posibblePos:
                    posibblePos.add(j)
        # enemySight = self.getEnemySight(list(posibblePos)) # [ [(),(),()...], [(),(),(),...] ]
        # if debug:
        #     for i in enemySight:
        #         self.debugDraw(i, [enemySight[i]/70+0.2,.2,.2])
        self.foodGrid = self.getFood(gameState)
        self.getEnemyCarryFoodNumber(gameState, enemyPosition.enemyPosition)
        # print(enemyCarryFoodNumber)
        self.updateEnemyDied()
        self.teammatePos = gameState.getAgentPosition(self.allienIndex)
        self.curInDangerous = self.curInsightOfEnemy(self.curPos,enemyPos)
        self.teammateInDangerous = self.curInsightOfEnemy(self.teammatePos,enemyPos)
        self.enemyPositionsToDefend = self.getNeedOfDefenceEnemyPosition(gameState, enemyPosition.enemyPosition)
        enemySight = self.getEnemySight(enemyPosition.enemyPosition) # [ [(),(),()...], [(),(),(),...] ]
        # print(enemySight)
        posInsight = self.getEnemyInsightTogether(enemySight)
        foodCluster = self.getTeammateTargetRegion(gameState)
        # print(self.getFood(gameState))
        # for i in foodCluster:
        #     self.debugDraw(i,[1,0,0])
        self.removeFoodsForTeammate(foodCluster)
        # print(self.foodGrid)
         # print(self.foodGrid)
        # self.debugClear()
        # for i in self.foodGrid.asList():
        #     if self.index == 2:
        #         self.debugDraw(i,[1,0,1])
        # print(posInsight)
        # # self.debugClear()
        # # if True: #debug
        # #     for i in enemySight:
        # #         for j in enemySight[i]:
        # #             self.debugDraw(j, [122,122,122])
        self.enemyAllHome = self.getEnemyAllatHome()
        # print(self.enemyDied,self.enemyDied[self.enemyIndex[0]] or self.enemyDied[self.enemyIndex[1]])
        #(self.index == 0 or self.index == 2)
        if (self.index == 0 or self.index == 1) or (self.enemyAllHome and (self.enemyDied[self.enemyIndex[0]] or self.enemyDied[self.enemyIndex[1]])):
            foodFarFromEnemy = self.getFoodFarFromEnemy(gameState, curPos, self.enemyPositionsToDefend)
            # self.debugDraw(foodFarFromEnemy,[1,0,0])
            distToFood = 100
            if foodFarFromEnemy:
                distToFood = self.distancer.getDistance(foodFarFromEnemy,curPos)
            closestMidDist,closestMidPos = self.getClostestMidDistance(curPos,gameState)
            if (((closestMidDist < 5) and (distToFood > closestMidDist + 4) and self.carryFoods > 0) or (len(self.getFood(gameState).asList()) <=2) and (self.curPos[0] in self.enemyRegionX)) or (foodFarFromEnemy is None) :
                # self.debugDraw(foodFarFromEnemy,[self.index/2,0,0])
                if self.curInsightOfEnemy(curPos,enemyPos):
                    escapeProblem = myProblem.EscapeProblem1(gameState, self)
                    actions,target = self.aStarSearch(escapeProblem, gameState, escapeProblem.EscapeHeuristic)
                    if actions == [] or actions == None or actions == "TIMEEXCEED":
                            # print("reachOwnMidWithEnemyInsight2")
                        action,target = myProblem.reachOwnMidWithEnemyInsight(self, gameState, self.index)
                        agentMod[self.index] = ("BackToMid",target)
                        if debug:
                            print("reachOwnMidWithEnemyInsight2", action)
                    else:
                        action = actions[0]
                        agentMod[self.index] = ("BackToMid",target)
                else:
                    action,target = myProblem.minDistance(curPos,[closestMidPos],self.walls,self)
                    agentMod[self.index] = ("BackToMid",target)
            else:
                # self.debugDraw(foodFarFromEnemy,[self.index/2,self.index/2,0])
                if self.curInsightOfEnemy(curPos,enemyPos):
                    self.block = self.getBlockRegions(gameState)
                    safeFood = self.getSafeFood(gameState,self.block)
                    capsules = self.getCapsules(gameState)
                    safeFood = safeFood + capsules
                    if safeFood != []:
                        problem = myProblem.EatOneSafeFoodProblem(gameState,self)
                        actions,target = self.aStarSearch(problem,gameState,problem.eatOneSafeHeuristic)
                        if (not actions is None) and (actions[0] != "T"):
                            action = actions[0]
                            agentMod[self.index] = ("eatFood",target)
                        else:
                            action,target = myProblem.eatCloseFood(self,gameState,self.index)
                            agentMod[self.index] = ("eatFood",target)
                    else:
                        action,target = myProblem.eatCloseFood(self,gameState,self.index)
                        agentMod[self.index] = ("eatFood",target)
                else:
                    # if self.carryFoods == 0:
                        # problem = myProblem.EatOneFoodHiddenProblem(gameState,self,posInsight)
                        # actions,target = self.aStarSearch(problem,gameState,problem.eatOneHeuristic)
                        # action = actions[0]
                        # self.debugDraw(target,[1,0,0])
                    action,target = myProblem.minDistance(curPos,[foodFarFromEnemy],self.walls,self)
                    agentMod[self.index] = ("eatFood",target)
                    # else:
                    #     action,target = myProblem.eatCloseFood(self,gameState,self.index)
                    #     agentMod[self.index] = ("eatFood",target)
        else:
            if self.enemyAllHome:
                # print(self.index,"I'm going to defend")
                action,target = self.defence()
                agentMod[self.index] = ("defend",target)
            else:
                # print(self.index,"Trace")
                enemyNeedToTrace = self.getEnemyNeedToTrace()
                # print(enemyNeedToTrace)
                action,target = self.trace(gameState,enemyNeedToTrace)
                agentMod[self.index] = ("defend",target)
        # print(self.enemyDied,self.enemyAllHome)
        self.updateDeath(gameState, action)
        # print(self.index,time.clock() - s)
        teammateState[self.index] = gameState.generateSuccessor(self.index,action)
        # print(self.index,time.clock() - s)
        actionHistory[self.index].append(action)
        # print(agentMod)
        return action

    def getNewWalls(self,newBlocking):
        walls = copy.deepcopy(self.walls)
        for pos in newBlocking:
            walls[pos[0]][pos[1]] = True
            # self.debugDraw(pos,[1,0,0])
        return walls

    def removeFoodsForTeammate(self,foodCluster):
        for i in foodCluster:
            dist1 = self.distancer.getDistance(i,self.curPos)
            dist2 = self.distancer.getDistance(i,self.teammatePos)
            # print(self.curPos,self.teammatePos,i,dist1,dist2)
            if dist1 > dist2 - 5:
                self.foodGrid[i[0]][i[1]] = False
        # print(self.foodGrid)
        # self.debugClear()
        # for i in self.foodGrid.asList():
        #     self.debugDraw(i,[self.index / 3,self.index / 3,1])
        # for i in foodCluster:
        #     self.debugDraw(i,[self.index / 3,self.index / 3,0])

    def attack(self,gameState):
        # if self.curInDangerous or self.teammateInDangerous:
        #     eatCapsule ###自己或队友在危险中，自己能吃到capsule，自己离capsule比队友近，则去吃capsule，否则吃豆子
        foodsList = self.getFood(gameState)
        foodList = self.getSafeFood(foodsList,self.block)
        foodCluster = self.getTeammateTargetRegion(gameState)
        self.foodGrid = self.removeFoodsForTeammate(foodCluster)
        walls = self.getNewWalls(self.block)
        if foodList != []:
            problem = myProblem.EatOneSafeFoodProblem(gameState,self)
            action,target = myProblem.minDistance(self.curPos,foodList,walls,self)
            agentMod[self.index] = ("eatFood",target)
        else:
            action,target = myProblem.reachOwnMidWithEnemyInsight()



    def trace(self,gameState,enemyIndex):
        enemyPos = self.enemyPositionsToDefend[enemyIndex]
        action,target = myProblem.minDistance(self.curPos,[enemyPos],self.walls,self)
        agentMod[self.index] = ("trace enemy",target)
        return action,target

    def defence(self):
        minDist = 999
        closestMid = random.choice(self.midLine)
        for pos in self.midLine:
            for index in self.enemyIndex:
                dist = self.distancer.getDistance(self.enemyPositionsToDefend[index],pos)
                if dist < minDist:
                    minDist = dist
                    closestMid = pos
        # print(closestMid)
        action,target = myProblem.minDistance(self.curPos, [closestMid], self.walls, self)
        agentMod[self.index] = ("go defence",target)
        # self.debugDraw(target,[0.8,0.4,0.2])
        return action,target

    # def defendFood(self,gameState):
    #     ourFoods = self.getFoodYouAreDefending(gameState)
    #     for food in ourFoods:
    #         minDist =
    #
    # def losing(self,gameState):
    #     if self.getFoodYouAreDefending(gameState) <= 2:




    def aStarSearch(self, problem, gameState, heuristic):
        start = time.clock()
        """Search the node that has the lowest combined cost and heuristic first."""
        # init
        visited = set()
        best_g = dict()
        """state: [position, foodGrid, food]"""
        """state, action list, cost value g"""
        start_node = (problem.getStartState(gameState, self.foodGrid), [], 0)
        frontier = util.PriorityQueue()
        priority = heuristic(problem.getStartState(gameState, self.foodGrid))  # f = h + g(start is 0)
        frontier.push(start_node, priority)

        while not frontier.isEmpty():
            elapsed = (time.clock() - start)
            # todo : if elapsed >= 0.8 return None
            if elapsed >= 0.8:
                if debug:
                    print("Time used:", elapsed)
                    print("time exceed")
                return "TIMEEXCEED",None  # for eatOneSafeFood time exceed
            else:
                current_node = frontier.pop()
                if current_node[0] in best_g.keys():  # reopen
                    if best_g[current_node[0]] > current_node[2]:
                        best_g[current_node[0]] = current_node[2]
                        for successor in problem.getSuccessors(current_node[0]):
                            cost_g = current_node[2] + successor[2]
                            priority = cost_g + heuristic(successor[0])
                            path = current_node[1] + [successor[1]]
                            frontier.push((successor[0], path, cost_g), priority)
                elif current_node[0] not in visited:
                    best_g[current_node[0]] = current_node[2]
                    visited.add(current_node[0])
                    if problem.isGoalState(gameState, current_node[0]):
                        print("New Team Time Used:",elapsed)
                        return current_node[1], current_node[0][0]
                    else:
                        for successor in problem.getSuccessors(current_node[0]):
                            cost_g = current_node[2] + successor[2]
                            priority = cost_g + heuristic(successor[0])
                            path = current_node[1] + [successor[1]]
                            frontier.push((successor[0], path, cost_g), priority)
        print("New Team Time Used",elapsed)
        return None,None
