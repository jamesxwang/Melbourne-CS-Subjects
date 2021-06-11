import copy

from myTeam import *
from game import *
from collections import defaultdict


class EatOneProblem:
    def __init__(self, gameState, agent):
        # number of food to be eat that is used in policy selection
        # change to be applied in a new heuristic
        self.targetFoodNum = 1  # TODO: change
        self.agent = agent
        self.index = agent.index
        self.walls = gameState.getWalls().deepCopy()
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.carriedFood = gameState.data.agentStates[self.index].numCarrying

    def getStartState(self, gameState, foodGrid):
        return (gameState.getAgentPosition(self.index), foodGrid, self.targetFoodNum - self.carriedFood)

    def isGoalState(self, gameState, state):
        return self.carriedFood == self.targetFoodNum
        # return state[0] in self.midLine and carriedFood[self.index] == self.targetFoodNum

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:  # if STOP needed?
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                nextFood = state[1].copy()
                if (nextx, nexty) in state[1].asList():  # successor in enemy's place
                    nextFood[nextx][nexty] = False
                    self.carriedFood += 1
                successors.append((((nextx, nexty), nextFood, self.targetFoodNum - self.carriedFood), direction, 1))
        return successors

    def eatOneHeuristic(self, state):  # target on eating
        """
        used for attack agent
        dist(P,Food): to the nearest x food positions[x = foodToEat]
          + x1 * dist(P,middleLine)(mean for distances to middle line positions)
          - x2 * dist(P,P2)
        @:param index: index of agent the heuristic is used on
                state: [(x, y), foodGrid, foodToEat]
        """
        curPos, foodGrid, foodToEat = state
        foodList = copy.deepcopy(foodGrid.asList())
        sumFoodDist = 0
        closed = [curPos]  # food in path calculated
        while foodToEat > 0:
            minDistToFood = 999999
            for food in foodList:
                for pos in closed:
                    newDist = self.agent.distancer.getDistance(pos, food)
                    if newDist < minDistToFood:
                        minDistToFood = newDist
                        nearestFoodPos = food
            foodList.remove(nearestFoodPos)
            closed.append(nearestFoodPos)
            foodToEat -= 1
            sumFoodDist += minDistToFood

        # midAccesses = self.getMiddleLine(gameState)
        # minDistToMid = 999999
        # for midPos in midAccesses:
        #   for closedPos in closed:
        #     newDist = self.distancer.getDistance(closedPos, midPos)
        #     if newDist < minDistToMid:
        #       minDistToMid = newDist
        #
        # teamIds = self.getTeam(gameState)  # teammate index
        # for idx in teamIds:
        #   if idx != self.index:
        #     tmPos = gameState.getAgentPosition(idx)
        #     # find pos of the other pac man and calculate distance
        #     distToTm = self.distancer.getDistance(curPos, tmPos)
        #     break

        # x = 0.4
        # return sumFoodDist + minDistToMid -  distToTm
        return sumFoodDist


class EatWithDeadEndProblem:  # default: eat one
    def __init__(self, gameState, agent):
        self.targetFoodNum = 1
        self.agent = agent
        self.index = agent.index
        self.walls = gameState.getWalls().deepCopy()
        self.deadEnds = agent.deadEnd
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.carriedFood = gameState.data.agentStates[self.index].numCarrying

    def getStartState(self, gameState, foodGrid):
        for pos in self.deadEnds:  # deadEnd foods are considered nothing
            foodGrid[pos[0]][pos[1]] = False
        return (gameState.getAgentPosition(self.index), foodGrid, self.targetFoodNum - self.carriedFood)

    def isGoalState(self, gameState, state):
        return self.carriedFood == self.targetFoodNum
        # return state[0] in self.middleLine and carriedFood[self.index] == self.targetFoodNum

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:  # if STOP needed?
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                nextFood = state[1].copy()
                if (nextx, nexty) in state[1].asList():  # successor in enemy's place
                    nextFood[nextx][nexty] = False
                    self.carriedFood += 1
                successors.append((((nextx, nexty), nextFood, self.targetFoodNum - self.carriedFood), direction, 1))
        return successors

    def eatWithDeadEndHeuristic(self, state):
        curPos, foodGrid, foodToEat = state
        foodList = copy.deepcopy(foodGrid.asList())
        sumFoodDist = 0
        closed = [curPos]  # food in path calculated
        while foodToEat > 0:
            minDistToFood = 999999
            for food in foodList:
                for pos in closed:
                    newDist = self.agent.distancer.getDistance(pos, food)
                    if newDist < minDistToFood:
                        minDistToFood = newDist
                        nearestFoodPos = food
            foodList.remove(nearestFoodPos)
            closed.append(nearestFoodPos)
            foodToEat -= 1
            sumFoodDist += minDistToFood
        return sumFoodDist


class ReachMiddleListProblem:
    def __init__(self, gameState, agent):
        self.agent = agent
        self.index = agent.index
        self.walls = gameState.getWalls().deepCopy()
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine

    def getStartState(self, gameState, foodGrid):
        return (gameState.getAgentPosition(self.index),)

    def isGoalState(self, gameState, state):
        return state[0][0] == self.enemyMiddleX

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                successors.append((((nextx, nexty),), direction, 1))
        return successors

    def reachMiddleListHeuristic(self, state):  # reach the side of our own on the middle list
        curPos = state[0]
        enemyMiddleList = self.enemyMiddleLine
        minDistToMid = 999999
        for midPoint in enemyMiddleList:
            newDist = self.agent.distancer.getDistance(curPos, midPoint)
            if newDist < minDistToMid:
                minDistToMid = newDist
        return minDistToMid


class BackToMiddleListProblem:
    def __init__(self, gameState, agent):
        self.agent = agent
        self.index = agent.index
        self.walls = gameState.getWalls().deepCopy()
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine

    def getStartState(self, gameState, foodGrid):
        return (gameState.getAgentPosition(self.index),)

    def isGoalState(self, gameState, state):
        return state[0][0] == self.middleX

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                successors.append((((nextx, nexty),), direction, 1))
        return successors

    def backToMiddleListHeuristic(self, state):  # reach the side of our own on the middle list
        curPos = state[0]
        middleList = self.middleLine
        minDistToMid = 999999
        for midPoint in middleList:
            newDist = self.agent.distancer.getDistance(curPos, midPoint)
            if newDist < minDistToMid:
                minDistToMid = newDist
        return minDistToMid


class DefendingProblem:
    # TODO: [reach the change element in two situations]: enemy in sight/foodGrid change
    pass

    def defendingHeuristic(agent, state):
        # TODO:
        pass


class EnemyEatCloseFoodProblem:
    def __init__(self, gameState, agent, enemyPos):
        # self.index = enemyIndex
        self.agent = agent
        self.walls = gameState.getWalls().deepCopy()
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.foods = agent.getFoodYouAreDefending(gameState).deepCopy()
        self.foodList = self.foods.asList()
        self.startPos = enemyPos

    def getStartState(self, gameState, foodGrid):
        # 3: num of steps; 4: DeadEnd depth
        return (self.startPos, self.foods)

    def isGoalState(self, gameState, state):
        return len(state[1].asList()) < len(self.foodList)

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:  # if STOP needed?
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                nextFood = state[1].copy()
                if (nextx, nexty) in state[1].asList():  # successor in enemy's place
                    nextFood[nextx][nexty] = False
                successors.append((((nextx, nexty), nextFood), direction, 1))
        return successors

    def EnemyEatCloseFoodHeuristic(self, state):
        curPos, foods = state
        minDist = 9999
        foodList = copy.deepcopy(foods.asList())
        for food in foodList:
            dis = self.agent.distancer.getDistance(curPos, food)
            minDist = min(minDist, dis)
        return minDist

class EnemyBackToMid:
    def __init__(self, gameState, agent, enemyPos):
        # self.index = enemyIndex
        self.agent = agent
        self.walls = gameState.getWalls().deepCopy()
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.foods = agent.getFoodYouAreDefending(gameState).deepCopy()
        self.foodList = self.foods.asList()
        self.startPos = enemyPos
        self.range = agent.enemyRegionX

    def getStartState(self, gameState, foodGrid):
        # 3: num of steps; 4: DeadEnd depth
        return (self.startPos)

    def isGoalState(self, gameState, state):
        return state[0] in self.range

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:  # if STOP needed?
            x, y = state
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                successors.append((((nextx, nexty)), direction, 1))
        return successors

    def BackToMidHeuristic(self, state):
        (curPos) = state
        minDist = 9999
        for pos in self.enemyMiddleLine:
            dis = self.agent.distancer.getDistance(curPos, pos)
            minDist = min(minDist, dis)
        return minDist

# calculate path for eating X closest food from target
class EatXClosestFoodFromTargetFoodProblem:
    def __init__(self, gameState, agent, pos, distLimit):
        self.startPos = pos
        self.agent = agent
        self.walls = gameState.getWalls().deepCopy()
        minDist = 99999
        for food in agent.foodGrid.asList():
            dist = self.agent.distancer.getDistance(food, self.startPos)
            if dist < minDist:
                minDist = dist
                self.targetFood = food

        # remove food positions outside distLimit
        self.keepFoods = Grid(agent.foodGrid.width, agent.foodGrid.height, False)
        self.keepFoods[self.targetFood[0]][self.targetFood[1]] = True
        for food in agent.foodGrid.asList():
            if self.agent.distancer.getDistance(self.targetFood, food) <= distLimit:
                self.keepFoods[food[0]][food[1]] = True


    def getStartState(self, gameState, foodGrid):
        # 3: num of steps; 4: DeadEnd depth
        return (self.startPos, self.keepFoods)

    def isGoalState(self, gameState, state):
        return len(state[1].asList()) == 0

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:  # if STOP needed?
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                nextFood = state[1].copy()
                if (nextx, nexty) in state[1].asList():  # successor in enemy's place
                    nextFood[nextx][nexty] = False
                successors.append((((nextx, nexty), nextFood), direction, 1))
        return successors

    def EatXClosestFoodFromTargetFoodHeuristic(self, state):
        curPos, foods = state
        maxDist = 0
        foodList = copy.deepcopy(foods.asList())
        for food in foodList:
            dis = self.agent.distancer.getDistance(curPos, food)
            maxDist = max(maxDist, dis)
        return maxDist


class EatOneSafeFoodProblem:
    def __init__(self, gameState, agent):
        self.index = agent.index
        self.agent = agent
        self.pacmanPos = gameState.getAgentPosition(self.index)
        self.deadEnds = agent.deadEnd
        self.walls = gameState.getWalls().deepCopy()
        # for i in self.deadEnds:
        #  self.walls[i[1]][i[0]] = True
        # add deadEnd points to walls
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.enemyIndices = agent.getOpponents(gameState)
        self.foods = agent.foodGrid.deepCopy()
        self.foodList = self.foods.asList()
        self.enemyPositions = []
        for idx in self.enemyIndices:
            enemyPos = gameState.getAgentPosition(idx)
            if enemyPos != None:
                self.enemyPositions.append(enemyPos)
        self.enemyPositions = tuple(self.enemyPositions)
        self.startPos = gameState.getAgentPosition(self.index)
        self.ownScaredTimer = gameState.data.agentStates[self.index].scaredTimer
        #fixme:
        # remove white ghost with timer > 1
        self.enemyScaredTimer = [gameState.data.agentStates[idx].scaredTimer for idx in self.enemyIndices]
        for listIndex, timer in enumerate(self.enemyScaredTimer):
            if timer > 1:
                removeIndex = self.enemyIndices[listIndex]
                removePos = gameState.getAgentPosition(removeIndex)
                if removePos != None:
                    positions = list(self.enemyPositions)
                    positions.remove(removePos)
                    self.enemyPositions = tuple(positions)
        self.startDeadEndDepth = 0
        x, y = self.startPos
        if (x, y) in self.deadEnds:  # deadEnds store reversed x,y
            self.startDeadEndDepth = self.deadEnds[(x, y)]
        if agent.block != []:
            for pos in agent.block:
                self.walls[pos[0]][pos[1]] = True
            if self.startPos not in self.deadEnds:  # fixme
                for food in self.foodList:
                    if food in agent.block:
                        # 修改了foodGrid和foodList，下一步之前不能再调eatOneSafeFoodProblem;
                        self.foods[food[0]][food[1]] = False
                        self.foodList = self.foods.asList()

    def getStartState(self, gameState, foodGrid):
        # 3: num of steps; 4: DeadEnd depth
        return (self.startPos, self.enemyPositions, self.foods, 1, self.startDeadEndDepth)

    def isGoalState(self, gameState, state):
        return len(state[2].asList()) < len(self.foods.asList())

    def getSuccessors(self, state):
        successors = []
        expandedForbidden = self.getExpandedForbidden(state[1])  # set
        separateExpandedForbidden = self.getSeparateExpandedForbidden(state[1])  # dict
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                newEnemyPositions = self.getNewEnemyPostion((nextx, nexty), separateExpandedForbidden)
                if (nextx, nexty) not in expandedForbidden:  # self.testValid((nextx,nexty),state[1],state[3]):
                    # if self.expandedForbidden[state[2]+1] == None:
                    #   self.storeExpandedForbidden(state[2]+1)
                    closeDist = 999
                    for enemy in newEnemyPositions:
                        # enemy in our side / own agent is scared
                        if (not enemy is None) and ((enemy[0] in self.agent.enemyRegionX) or self.ownScaredTimer > 0):
                            dis = self.agent.distancer.getDistance((nextx, nexty), enemy)
                            closeDist = min(closeDist, dis)
                    if ((nextx, nexty) in self.deadEnds):
                        numDE = self.agent.deadEnd[(nextx, nexty)]
                    else:
                        numDE = 0
                    if closeDist > numDE * 2 - self.startDeadEndDepth:
                        nextFood = state[2].copy()
                        nextFood[nextx][nexty] = False
                        successors.append(
                            (((nextx, nexty), newEnemyPositions, nextFood, state[3] + 1, numDE), direction, 1))
        return successors

    def eatOneSafeHeuristic(self, state):
        curPos, enemy, foods, step, dead = state
        minDist = 9999
        foodList = copy.deepcopy(foods.asList())
        for food in foodList:
            dis = self.agent.distancer.getDistance(curPos, food)
            minDist = min(minDist, dis)
        return minDist

    def getExpandedForbidden(self, enemySet):
        expandedForbidden = set()
        for enemy in enemySet:
            expandedForbidden.add(enemy)
            (x, y) = enemy
            for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction)
                expandedForbidden.add((int(x + dx), int(y + dy)))
        return expandedForbidden

    def getSeparateExpandedForbidden(self, enemySet):  # set to dict
        expandedForbidden = defaultdict(set)
        for idx, enemyPos in enumerate(enemySet):
            subPos = set()  # positions from one possible ghost
            subPos.add(enemyPos)
            (x, y) = enemyPos
            for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction)
                subPos.add((int(x + dx), int(y + dy)))
            expandedForbidden[idx] = subPos
        return expandedForbidden

    def getNewEnemyPostion(self, pos, enemyDict):  # dict to tuple
        newEnemySet = set()
        enemyExpansion = set()
        for enemySet in enemyDict.values():
            minDist = 9999
            for enemy in enemySet:
                # print("enemy",enemy)
                if not self.walls[enemy[0]][enemy[1]]:
                    dis = self.agent.distancer.getDistance(pos, enemy)
                    if dis == minDist:
                        enemyExpansion.add(enemy)
                    if dis < minDist:
                        minDist = dis
                        enemyExpansion = set()
                        enemyExpansion.add(enemy)
            newEnemySet.update(enemyExpansion)
        return tuple(newEnemySet)

    def testValid(self, pos, enemyList, step):
        for enemy in enemyList:
            if self.agent.distancer.getDistance(pos, enemy) <= step:
                return False
        return True

    # forceReturn
    def getInSightEnemyDistances(self, curPos):  # distance to enemy from current position
        distList = []
        for enemyPos in self.enemyPositions:
            if enemyPos != None:
                distList.append(self.agent.distancer.getDistance(enemyPos, curPos))
        return distList


class EatCapsuleProblem:
    def __init__(self, gameState, agent):
        self.index = agent.index
        self.agent = agent
        self.pacmanPos = gameState.getAgentPosition(self.index)
        self.capsules = tuple(agent.capsules)
        self.walls = gameState.getWalls().deepCopy()
        # self.deadEnds = agent.deadEnd
        # self.foods = agent.foodGrid.deepCopy()
        # self.foodList = self.foods.asList()
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.enemyIndices = agent.getOpponents(gameState)
        self.enemyPositions = []
        for idx in self.enemyIndices:
            enemyPos = gameState.getAgentPosition(idx)
            if enemyPos != None:
                self.enemyPositions.append(enemyPos)
        self.enemyPositions = tuple(self.enemyPositions)
        self.startPos = gameState.getAgentPosition(self.index)
        #fixme:
        # remove white ghost with timer > 1
        self.enemyScaredTimer = [gameState.data.agentStates[idx].scaredTimer for idx in self.enemyIndices]
        for listIndex, timer in enumerate(self.enemyScaredTimer):
            if timer > 1:
                removeIndex = self.enemyIndices[listIndex]
                removePos = gameState.getAgentPosition(removeIndex)
                if removePos != None:
                    positions = list(self.enemyPositions)
                    positions.remove(removePos)
                    self.enemyPositions = tuple(positions)

    def getStartState(self, gameState, foodGrid):
        # 3: num of steps; 4: DeadEnd depth
        return (self.startPos, self.enemyPositions, self.capsules, 1)

    def isGoalState(self, gameState, state):
        return len(state[2]) < len(self.capsules)

    def getSuccessors(self, state):
        successors = []
        expandedForbidden = self.getExpandedForbidden(state[1])  # set
        separateExpandedForbidden = self.getSeparateExpandedForbidden(state[1])  # dict
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                newEnemyPositions = self.getNewEnemyPostion((nextx, nexty), separateExpandedForbidden)
                if (nextx, nexty) not in expandedForbidden:
                    nextCapsules = state[2]
                    if (nextx, nexty) in nextCapsules:
                        nextCapsules = list(nextCapsules)
                        nextCapsules.remove((nextx, nexty))
                    successors.append(
                        (((nextx, nexty), newEnemyPositions, tuple(nextCapsules), state[3] + 1), direction, 1))
        return successors

    def eatCapsuleHeuristic(self, state):
        curPos, enemy, capsules, step = state
        minDist = 9999
        for capsule in capsules:
            dis = self.agent.distancer.getDistance(curPos, capsule)
            minDist = min(minDist, dis)
        return minDist

    def getExpandedForbidden(self, enemySet):
        expandedForbidden = set()
        for enemy in enemySet:
            expandedForbidden.add(enemy)
            (x, y) = enemy
            for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction)
                expandedForbidden.add((int(x + dx), int(y + dy)))
        return expandedForbidden

    def getSeparateExpandedForbidden(self, enemySet):  # set to dict
        expandedForbidden = defaultdict(set)
        for idx, enemyPos in enumerate(enemySet):
            subPos = set()  # positions from one possible ghost
            subPos.add(enemyPos)
            (x, y) = enemyPos
            for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction)
                subPos.add((int(x + dx), int(y + dy)))
            expandedForbidden[idx] = subPos
        return expandedForbidden

    def getNewEnemyPostion(self, pos, enemyDict):  # dict to tuple
        newEnemySet = set()
        enemyExpansion = set()
        for enemySet in enemyDict.values():
            minDist = 9999
            for enemy in enemySet:
                # print("enemy",enemy)
                if not self.walls[enemy[0]][enemy[1]]:
                    dis = self.agent.distancer.getDistance(pos, enemy)
                    if dis == minDist:
                        enemyExpansion.add(enemy)
                    if dis < minDist:
                        minDist = dis
                        enemyExpansion = set()
                        enemyExpansion.add(enemy)
            newEnemySet.update(enemyExpansion)
        return tuple(newEnemySet)

    def testValid(self, pos, enemyList, step):
        for enemy in enemyList:
            if self.agent.distancer.getDistance(pos, enemy) <= step:
                return False
        return True

    # forceReturn
    def getInSightEnemyDistances(self, curPos):  # distance to enemy from current position
        distList = []
        for enemyPos in self.enemyPositions:
            if enemyPos != None:
                distList.append(self.agent.distancer.getDistance(enemyPos, curPos))
        return distList


class EscapeProblem1:
    def __init__(self, gameState, agent):
        self.index = agent.index
        self.agent = agent
        self.pacmanPos = gameState.getAgentPosition(self.index)
        self.deadEnds = agent.deadEnd
        self.walls = gameState.getWalls().deepCopy()
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.enemyIndices = agent.getOpponents(gameState)
        self.foods = agent.foodGrid.deepCopy()
        self.foodList = self.foods.asList()
        self.enemyPositions = []
        for idx in self.enemyIndices:
            enemyPos = gameState.getAgentPosition(idx)
            if enemyPos != None:
                self.enemyPositions.append(enemyPos)
        self.enemyPositions = tuple(self.enemyPositions)
        #fixme:
        # remove white ghost with timer > 1
        self.enemyScaredTimer = [gameState.data.agentStates[idx].scaredTimer for idx in self.enemyIndices]
        for listIndex, timer in enumerate(self.enemyScaredTimer):
            if timer > 1:
                removeIndex = self.enemyIndices[listIndex]
                removePos = gameState.getAgentPosition(removeIndex)
                if removePos != None:
                    positions = list(self.enemyPositions)
                    positions.remove(removePos)
                    self.enemyPositions = tuple(positions)

    def getStartState(self, gameState, foodGrid):
        return (gameState.getAgentPosition(self.index), self.enemyPositions)

    def isGoalState(self, gameState, state):
        return state[0][0] == self.middleX

    def getSuccessors(self, state):
        successors = []
        expandedForbidden = self.getExpandedForbidden(state[1])  # set
        separateExpandedForbidden = self.getSeparateExpandedForbidden(state[1])  # dict
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
            x, y = state[0]
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not self.walls[nextx][nexty]:
                newEnemyPositions = self.getNewEnemyPostion((nextx, nexty), separateExpandedForbidden)
                if (nextx, nexty) not in expandedForbidden:  # self.testValid((nextx,nexty),state[1],state[3]):
                    successors.append((((nextx, nexty), newEnemyPositions), direction, 1))
        return successors

    def EscapeHeuristic(self, state):
        middleLine = self.middleLine
        # print(state)
        curPos, enemy = state
        minDist = 9999
        for mid in middleLine:
            minDist = min(minDist, self.agent.distancer.getDistance(curPos, mid))
        return minDist

    def getExpandedForbidden(self, enemySet):
        expandedForbidden = set()
        for enemy in enemySet:
            expandedForbidden.add(enemy)
            (x, y) = enemy
            for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction)
                expandedForbidden.add((int(x + dx), int(y + dy)))
        return expandedForbidden

    def getSeparateExpandedForbidden(self, enemySet):  # set to dict
        expandedForbidden = defaultdict(set)
        for idx, enemyPos in enumerate(enemySet):
            subPos = set()  # positions from one possible ghost
            subPos.add(enemyPos)
            (x, y) = enemyPos
            for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction)
                subPos.add((int(x + dx), int(y + dy)))
            expandedForbidden[idx] = subPos
        return expandedForbidden

    def getNewEnemyPostion(self, pos, enemyDict):  # dict to tuple
        newEnemySet = set()
        enemyExpansion = set()
        for enemySet in enemyDict.values():
            minDist = 9999
            for enemy in enemySet:
                # print("enemy",enemy)
                if not self.walls[enemy[0]][enemy[1]]:
                    dis = self.agent.distancer.getDistance(pos, enemy)
                    if dis == minDist:
                        enemyExpansion.add(enemy)
                    if dis < minDist:
                        minDist = dis
                        enemyExpansion = set()
                        enemyExpansion.add(enemy)
            newEnemySet.update(enemyExpansion)
        return tuple(newEnemySet)

    def testValid(self, pos, enemyList, step):
        for enemy in enemyList:
            if self.agent.distancer.getDistance(pos, enemy) <= step:
                return False
        return True

    # forceReturn
    def getInSightEnemyDistances(self, curPos):  # distance to enemy from current position
        distList = []
        for enemyPos in self.enemyPositions:
            if enemyPos != None:
                distList.append(self.agent.distancer.getDistance(enemyPos, curPos))
        return distList


class EatOneEscapeProblem:
    def __init__(self, gameState, agent):
        self.index = agent.index
        self.agent = agent
        self.pacmanPos = gameState.getAgentPosition(self.index)
        self.deadEnds = agent.deadEnd
        self.walls = gameState.getWalls().deepCopy()
        # for i in self.deadEnds:
        #  self.walls[i[1]][i[0]] = True
        # add deadEnd points to walls
        self.middleX = agent.midX
        self.enemyMiddleX = agent.enemyMidX
        self.middleLine = agent.midLine
        self.enemyMiddleLine = agent.enemyMidLine
        self.enemyIndices = agent.getOpponents(gameState)
        self.foods = agent.getFood(gameState)
        self.foodList = self.foods.asList()
        self.enemyPositions = []
        for idx in self.enemyIndices:
            enemyPos = gameState.getAgentPosition(idx)
            if enemyPos != None:
                self.enemyPositions.append(enemyPos)
        self.enemyPositions = tuple(self.enemyPositions)

    def getStartState(self, gameState, foodGrid):
        return (gameState.getAgentPosition(self.index), self.enemyPositions, self.foods, 1)

    def isGoalState(self, gameState, state):
        return ((state[0][0] == self.middleX) and (len(state[2].asList()) < len(self.foods.asList()))) or (
            state[3] > 10)

    def getSuccessors(self, state):
        successors = []
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
            x, y = state[0]
            expandedForbidden = self.getExpandedForbidden(state[1])
            newEnemyList = self.getNewEnemyPostion(state[0], expandedForbidden)
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if (not self.walls[nextx][nexty]) and (
                (nextx, nexty) not in expandedForbidden):  # self.testValid((nextx,nexty),state[1],state[3]):
                # if self.expandedForbidden[state[2]+1] == None:
                #   self.storeExpandedForbidden(state[2]+1)
                nextFood = state[2].copy()
                nextFood[nextx][nexty] = False
                successors.append((((nextx, nexty), newEnemyList, nextFood, state[3] + 1), direction, 1))
        return successors

    def eatOneEscapeHeuristic(self, state):
        middleLine = self.middleLine
        # print(state)
        curPos, enemy, foods, step = state
        minDist = 9999
        foodList = copy.deepcopy(foods.asList())
        if len(self.foodList) > len(foodList):
            minDist = 9999
            for mid in middleLine:
                minDist = min(minDist, self.agent.distancer.getDistance(curPos, mid))
            return minDist
        for food in foodList:
            for mid in middleLine:
                dis = self.agent.distancer.getDistance(curPos, food) + self.agent.distancer.getDistance(food, mid)
                minDist = min(minDist, dis)
        return minDist

    def getNewEnemyPostion(self, pos, enemyList):
        newEnemyList = []
        minDist = 9999
        for enemy in enemyList:
            # print("enemy",enemy)
            if not self.walls[enemy[0]][enemy[1]]:
                dis = self.agent.distancer.getDistance(pos, enemy)
                if dis == minDist:
                    newEnemyList.append((enemy[0], enemy[1]))
                if dis < minDist:
                    minDist = dis
                    newEnemyList = []
                    newEnemyList.append(enemy)
        return tuple(newEnemyList)

    def testValid(self, pos, enemyList, step):
        for enemy in enemyList:
            if self.agent.distancer.getDistance(pos, enemy) <= step:
                return False
        return True

    def getExpandedForbidden(self, enemyList):
        expandedForbidden = []
        for enemy in enemyList:
            expandedForbidden.append(enemy)
            (x, y) = enemy
            for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction)
                expandedForbidden.append((int(x + dx), int(y + dy)))
        return expandedForbidden

    # forceReturn
    def getInSightEnemyDistances(self, curPos):  # distance to enemy from current position
        distList = []
        for enemyPos in self.enemyPositions:
            if enemyPos != None:
                distList.append(self.agent.distancer.getDistance(enemyPos, curPos))
        return distList


def getActualWalls(gameState, agent):
    walls = copy.deepcopy(agent.walls)
    return walls


def getWallsWithAdditionList(gameState, agent, additionList):
    walls = copy.deepcopy(agent.walls)
    for pos in additionList:  # x y of deadEnd is reversed
        walls[pos[0]][pos[1]] = True
    return walls


# find shortest path, to any pos in posList
def minDistance(pos, posList, walls, agent,secondList = []):
    minDist = 9999
    action = Directions.STOP
    goal = pos
    # print("minDistance=======")
    # print("curPos:", pos)
    # agent.debugDraw(pos,[1,0,0])
    # for i in posList:
    #     agent.debugDraw(i,[0.5,0.5,0.5])
    # w = walls.asList()
    # for i in w:
    #     agent.debugDraw(i,[0,0,0.9])
    for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST, Directions.STOP]:  # if STOP needed?
        x, y = pos
        dx, dy = Actions.directionToVector(direction)
        nextx, nexty = int(x + dx), int(y + dy)
        wallsList = walls.asList()
        # print("[minDistance]walls", walls)
        # print("[minDistance]wallsList", wallsList)
        # agent.debugDraw((nextx,nexty),[0.1,0.7,0.3])
        # print("[minDistance]next POS:", (nextx, nexty))
        # print("[minDistance]action", direction)
        # print("isWall in grid", walls[nextx][nexty])
        # print("isWall in list", (nextx,nexty) in wallsList)
        # if not walls[nextx][nexty]:
        if not (nextx, nexty) in wallsList:
            for target in posList:
                dist = agent.distancer.getDistance((nextx, nexty), target)
                # if secondList != []:

                if dist < minDist:
                    # print("[minDistance]current dist",dist)
                    # print("[minDistance]current goal",target)
                    goal = target
                    minDist = dist
                    action = direction
    # print("[minDistance]target food:", goal)
    return (action, goal)


def minDistanceToFarthestFood(pos, posList, walls, agent):
    maxDist = 0
    x, y = pos
    foodPos = posList[0]
    for target in posList:
        dist = agent.distancer.getDistance((x, y), target)
        if dist > maxDist:
            maxDist = dist
            foodPos = target
    minDist = 9999
    action = Directions.NORTH
    for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:  # if STOP needed?
        x, y = pos
        dx, dy = Actions.directionToVector(direction)
        nextx, nexty = int(x + dx), int(y + dy)
        if not walls[nextx][nexty]:
            dist = agent.distancer.getDistance((nextx, nexty), foodPos)
            if dist < minDist:
                minDist = dist
                action = direction
    return action


# find shortest path, to any pos in posList
def minDistanceAvoidGhost(pos, posList, walls, agent, ghostList):
    minDist = 9999
    action = Directions.STOP
    goal = pos
    # fixme: annotate debug part
    # for i in walls.asList():
    #     agent.debugDraw(i,[1,1,0])

    # print("[minDistanceAvoidGhost] curPos", pos)
    for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:  # if STOP needed?
        x, y = pos
        dx, dy = Actions.directionToVector(direction)
        nextx, nexty = int(x + dx), int(y + dy)
        adjacentToGhost = False
        # print("[minDistanceAvoidGhost] direction \"", direction, "\" is wall:", walls[nextx][nexty])
        if not walls[nextx][nexty]:
            # action shouldn't lead to the adjacent position from ghost
            for direction2 in [Directions.NORTH, Directions.SOUTH, Directions.EAST, Directions.WEST]:
                dx, dy = Actions.directionToVector(direction2)
                adjacentx, adjacenty = int(nextx + dx), int(nexty + dy)
                if (adjacentx, adjacenty) in ghostList:
                    adjacentToGhost = True
                    break
            if adjacentToGhost == True and (nextx not in agent.ourRegionX or agent.ownScaredTimer > 0):
                #(nextx, nexty)邻接有威胁的ghost，则不考虑这个(nextx,nexty)
                continue
            for target in posList:
                dist = agent.distancer.getDistance((nextx, nexty), target)
                if dist < minDist:
                    # print("[minDistanceAvoidGhost]current dist",dist)
                    # print("[minDistanceAvoidGhost]current target",target)
                    goal = target
                    minDist = dist
                    action = direction
    return (action, goal)


def getFoodExceptDeadEnds(agent, gameState):
    food = agent.getFood(gameState)
    for pos in agent.deadEnd:
        food[pos[0]][pos[1]] = False
    foodList = food.asList()
    return foodList


def eatCloseFood(agent, gameState, index):
    food = agent.foodGrid.deepCopy()
    foodList = food.asList()
    pos = gameState.getAgentPosition(index)
    walls = getActualWalls(gameState, agent)
    action, target = minDistance(pos, foodList, walls, agent)
    return (action, target)


def eatCloseFoodAvoidGhost(agent, gameState, index):
    ghostIndices = agent.getOpponents(gameState)
    ghostList = []
    for idx in ghostIndices:
        enemyPos = gameState.getAgentPosition(idx)
        if enemyPos != None:
            ghostList.append(enemyPos)
    #only regard ghost enemy as walls, don't regard pacman enemy as walls
    ghostEnemy = agent.ghostEnemy(ghostList)
    addList = ghostEnemy + agent.block

    food = agent.foodGrid.deepCopy()
    foodList = food.asList()
    pos = gameState.getAgentPosition(index)
    walls = getWallsWithAdditionList(gameState, agent, addList)
    action, target = minDistanceAvoidGhost(pos, foodList, walls, agent, ghostList)
    return (action, target)


def eatRandomFood(agent, gameState, index):
    foodIndex = agent.randomFoodIndex
    foodPos = agent.foodGrid.asList()[foodIndex]
    pos = gameState.getAgentPosition(index)
    walls = getActualWalls(gameState, agent)
    action, target = minDistance(pos, [foodPos], walls, agent)
    return (action, target)


def eatFoodOutsideDeadEnd(agent, gameState, index):
    foodList = getFoodExceptDeadEnds(agent, gameState)
    pos = gameState.getAgentPosition(index)
    deadEnd = agent.deadEnd
    walls = getWallsWithAdditionList(gameState, agent, deadEnd)
    action, target = minDistance(pos, foodList, walls, agent)
    return (action, target)


# reach the top middleLine position
def reachSpecificEnemyMidPos(agent, gameState, index):
    middleList = agent.enemyMidLine
    topMidPos = [middleList[0]]
    pos = gameState.getAgentPosition(index)
    walls = getActualWalls(gameState, agent)
    action, target = minDistance(pos, topMidPos, walls, agent)
    return action, target


def reachOwnMidList(agent, gameState, index):
    middleList = agent.midLine
    pos = gameState.getAgentPosition(index)
    walls = getActualWalls(gameState, agent)
    action, target = minDistance(pos, middleList, walls, agent)
    return action, target


# breaking used
def reachSpecificMidWithEnemyInsight(agent, gameState, index, targetPos):
    ghostIndices = agent.getOpponents(gameState)
    ghostList = []
    for enemyIndex in ghostIndices:
        enemyPos = gameState.getAgentPosition(enemyIndex)
        if enemyPos != None:
            if gameState.data.agentStates[enemyIndex].scaredTimer <= 1:
                ghostList.append(enemyPos) # list of no-scared ghost
    # only regard ghost enemy as walls, don't regard pacman enemy as walls
    ghostEnemy = agent.ghostEnemy(ghostList)
    addList = ghostEnemy + list(agent.deadEnd.keys())

    pos = gameState.getAgentPosition(index)
    walls = getWallsWithAdditionList(gameState, agent, addList)
    action, target = minDistanceAvoidGhost(pos, [targetPos], walls, agent, ghostList)
    return action, target


# used when escape & eatSafeFood return nothing
def reachOwnMidWithEnemyInsight(agent, gameState, index):
    ghostIndices = agent.getOpponents(gameState)
    ghostList = []
    for enemyIndex in ghostIndices:
        enemyPos = gameState.getAgentPosition(enemyIndex)
        if enemyPos != None:
            if gameState.data.agentStates[enemyIndex].scaredTimer <= 1:
                ghostList.append(enemyPos) # list of no-scared ghost
    # only regard ghost enemy as walls, don't regard pacman enemy as walls
    ghostEnemy = agent.ghostEnemy(ghostList)
    addList = ghostEnemy + list(agent.deadEnd.keys())

    middleList = agent.midLine
    pos = gameState.getAgentPosition(index)
    walls = getWallsWithAdditionList(gameState, agent, addList)
    action, target = minDistanceAvoidGhost(pos, middleList, walls, agent, ghostList)
    return action, target


def reachEnemyMidList(agent, gameState, index):
    enemyMiddleList = agent.enemyMidLine
    pos = gameState.getAgentPosition(index)
    walls = getActualWalls(gameState, agent)
    action, target = minDistance(pos, enemyMiddleList, walls, agent)
    return action, target


def eatFoodClosestToMidList(agent, gameState, index):
    midList = agent.midLine
    foodList = getFoodExceptDeadEnds(agent, gameState)
    pos = gameState.getAgentPosition(index)
    deadEnd = agent.deadEnd
    walls = getWallsWithAdditionList(gameState, agent, deadEnd)
    minDist = 9999
    for midPos in midList:
        x, y = midPos
        if not walls[x][y]:
            for target in foodList:
                dist = agent.distancer.getDistance((x, y), target)
                if dist < minDist:
                    minDist = dist
                    minDistFoodPos = target
    action, target = minDistance(pos, [minDistFoodPos], walls, agent)
    return action, target


def eatClosestGhost(agent, gameState, index):
    ghostIndices = agent.getOpponents(gameState)
    ghostList = []
    for idx in ghostIndices:
        enemyPos = gameState.getAgentPosition(idx)
        if enemyPos != None:
            ghostList.append(enemyPos)
    pos = gameState.getAgentPosition(index)
    deadEnd = agent.deadEnd
    walls = getActualWalls(gameState, agent)
    action, target = minDistance(pos, ghostList, walls, agent)
    return action, target


def eatClosestEnemyPacman(agent, gameState, index):
    enemyIndices = agent.getOpponents(gameState)
    enemyList = []
    for idx in enemyIndices:
        enemyPos = gameState.getAgentPosition(idx)
        if enemyPos != None:
            if agent.red:
                if enemyPos[0] <= agent.midX:
                    enemyList.append(enemyPos)
            else:
                if enemyPos[0] >= agent.enemyMidX:
                    enemyList.append(enemyPos)
    pos = gameState.getAgentPosition(index)
    walls = getActualWalls(gameState, agent)
    action, target = minDistance(pos, enemyList, walls, agent)
    return action, target


def eatFarthestFoodFromGhost(agent, gameState, index):
    pass
    # fixme: blocked
    # enemyIndices = agent.getOpponents()
    # enemyPosList = [gameState.getAgentPosition()]
    # pos = gameState.getAgentPosition(index)
    # deadEnd = agent.deadEnd
    # walls = getWallsWithAdditionList(gameState, agent, deadEnd)
    # action, target = minDistance(pos, foodList, walls, agent)
    # return action, target


def foolGhost(agent, gameState, index):
    ghostIndices = agent.getOpponents(gameState)
    ghostList = []
    for idx in ghostIndices:
        enemyPos = gameState.getAgentPosition(idx)
        if enemyPos != None:
            ghostList.append(enemyPos)
    pos = gameState.getAgentPosition(index)
    deadEnd = agent.deadEnd
    walls = getWallsWithAdditionList(gameState, agent, deadEnd)
    action, target = minDistance(pos, ghostList, walls, agent)
    return action, target


# used when in stalemate
def breakStalemate(agent, gameState, index):
    x, y = gameState.getAgentPosition(index)
    walls = getActualWalls(gameState, agent)
    lastAction = agent.lastAction
    if lastAction != None and lastAction in [Directions.NORTH, Directions.SOUTH]:
        dx, dy = Actions.directionToVector(lastAction)
        nextx, nexty = int(x + dx), int(y + dy)
        if not walls[nextx][nexty]:
            return lastAction, (nextx, nexty)
    if agent.red:  # red team back to left
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.WEST]:  # , Directions.EAST]:
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not walls[nextx][nexty]:
                return direction, (nextx, nexty)
    else:  # blue team back to right
        for direction in [Directions.NORTH, Directions.SOUTH, Directions.EAST]:  # , Directions.WEST]:
            dx, dy = Actions.directionToVector(direction)
            nextx, nexty = int(x + dx), int(y + dy)
            if not walls[nextx][nexty]:
                return direction, (nextx, nexty)
