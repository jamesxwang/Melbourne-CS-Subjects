import numpy as np
from captureAgents import CaptureAgent
from game import *
import copy
import getTrainingdata
from newUCT import mcts
from State_2_back import State_2
import newUCT
def createTeam(firstIndex, secondIndex, isRed, first = 'Random', second = 'QlearningAgent'):
    return [eval(first)(firstIndex), eval(second)(secondIndex)]

class Random(CaptureAgent):
    def registerInitialState(self, gameState):
        CaptureAgent.registerInitialState(self, gameState)
    def chooseAction(self,gameState):
        # print(gameState)
        # getTrainingdata.getTrainingData(gameState=gameState)
        actions = gameState.getLegalActions(self.index)
        return random.choice(actions)


class QlearningAgent(CaptureAgent):

    def registerInitialState(self, gameState):
        # getTrainingdata.getTrainingData(gameState)
        CaptureAgent.registerInitialState(self, gameState)
        self.qlearning = QLearning(0.5,0.9)
        self.prevState = gameState
        self.initialPosition = gameState.getAgentPosition(self.index)
        self.middleLine = self.getOurMiddleLine(gameState)
        self.nextState = gameState
        self.test = mcts(timeLimit=500)

    def getMaxAction(self,gameState):
        actions = gameState.getLegalActions(self.index)
        actions.remove("Stop")
        actionsQ = {}
        maxQ = float("-inf")
        for action in actions:
            newGameState = gameState.generateSuccessor(self.index, action)
            nextFeature = self.getFeature(gameState,newGameState,newGameState)
            nextQ = self.qlearning.getQvalue(nextFeature)
            actionsQ[action] = nextQ
            maxQ = max(maxQ,nextQ)
        maxValue = []
        for i in actionsQ:
            if actionsQ[i] == maxQ:
                maxValue.append(i)
        action = random.choice(maxValue)
        return action,maxQ,gameState.generateSuccessor(self.index, action)

    def chooseAction(self, gameState):
        # getTrainingdata.getTrainingData(gameState)
        reward = self.getReward(self.prevState,gameState)
        nextA,nextQ,nextGameState = self.getMaxAction(gameState)
        if True:
                # (reward != 0) and (gameState.data.timeleft<1190):
            feature = self.getFeature(self.prevState,gameState,self.nextState)
            curQ = self.qlearning.getQvalue(feature)
            self.qlearning.update(feature,curQ,nextQ,reward)
            if reward != 0:
                print("reward",reward,"Qvalue:",curQ,nextQ," CurrentFeature:",feature,"CurrentPosition:",gameState.getAgentPosition(self.index),"PrePosition",self.nextState.getAgentPosition(self.index))
        score = gameState.getScore()
        action = nextA
        t = State_2(self, gameState, self.getScore(gameState), gameState.data.agentStates[self.index].numCarrying)
        # print("position and food:",self.getFood(gameState).asList())
        # action = self.test.search(t)

        if random.randint(0,10) < 2:
            action = random.choice(gameState.getLegalActions(self.index))
        else:
            action = nextA
        self.prevState = gameState
        self.nextState = gameState.generateSuccessor(self.index,action)
        return action
        # return action
        # action, Qvalue,nextGameState = self.getMaxAction(gameState)
        # reward = self.getReward(gameState,nextGameState)
        # if reward != 0  and (gameState.data.timeleft < 1190):
        #     # self.getReward(gameState,nextGameState)
        #     nextAction,nextQ,newGameState = self.getMaxAction(nextGameState)
        #     feature = self.getFeature(gameState,nextGameState)
        #     self.qlearning.update(feature,Qvalue,nextQ,reward)

        # ['agentDistances', 'blueTeam', 'data', 'deepCopy', 'generateSuccessor', 'getAgentDistances', 'getAgentPosition',
        #  'getAgentState', 'getBlueCapsules', 'getBlueFood', 'getBlueTeamIndices', 'getCapsules', 'getDistanceProb',
        #  'getInitialAgentPosition', 'getLegalActions', 'getNumAgents', 'getRedCapsules', 'getRedFood', 'getRedTeamIndices',
        #  'getScore', 'getWalls', 'hasFood', 'hasWall', 'initialize', 'isOnRedTeam', 'isOver', 'isRed', 'makeObservation',
        #  'redTeam', 'teams']

    def getReward(self, preState,gameState):
        score = (len(self.getFood(preState).asList()) - len(self.getFood(gameState).asList()))
        # score = 0
        curPosition = gameState.getAgentPosition(self.index)
        prePosition = preState.getAgentPosition(self.index)
        if self.distancer.getDistance(prePosition,curPosition)>1:
            score = -2
            # print(curPosition,prePosition)
        changeScore = (gameState.getScore() - preState.getScore())*2
        # if changeScore>0:
        #     print(changeScore)
        score+=changeScore
        # print(score)
        # score += -0.1
        return score

    def getOurMiddleLine(self,gameState):
        middleLine = []
        mapWidth = gameState.data.layout.width
        mapHeight = gameState.data.layout.height
        if self.red:
          x = int((mapWidth - 2) / 2)
        else:
          x = int((mapWidth - 2) / 2 + 1)
        wallList = gameState.getWalls().asList()
        for y in range(1, mapHeight):
          if (x, y) not in wallList:
            middleLine.append((x,y))
        return middleLine

    #distToEnemy,distToFood,distToMid,FoodNumber,carryFood
    def getFeature(self,pregameState,gameState,gameState_back):
        feature = []
        curPosition = gameState.getAgentPosition(self.index)
        prePosition = pregameState.getAgentPosition(self.index)
        curPosition1 = copy.deepcopy(curPosition)
        if self.distancer.getDistance(prePosition,curPosition) > 1:
            curPosition1 = copy.deepcopy(curPosition)
            curPosition = gameState_back.getAgentPosition(self.index)
            gameState = gameState_back
            print("PositionChange:",curPosition1,curPosition,prePosition)
            # print(gameState_back)
        enemyPosition = gameState.getAgentPosition(3)
        # enemyPosition2 = gameState.getAgentPosition(1)
        foods = self.getFood(gameState).asList()
        minDist = 9999
        for food in foods:
            minDist = min(minDist,self.distancer.getDistance(food,curPosition))
        if (len(self.getFood(pregameState).asList()) > len(self.getFood(gameState).asList())) or (foods == []):
            minDist = 0
        midDist = 9999
        for i in self.middleLine:
            midDist = min(midDist,self.distancer.getDistance(i,curPosition))
        if gameState.data.agentStates[self.index].numCarrying >0:
            if gameState.getScore() > pregameState.getScore():
                feature.append(1)
            else:
                feature.append((100-midDist)/100)
        else:
            feature.append(0)
        if gameState.getScore() - pregameState.getScore() > 0:
            feature.append(0)
        else:
            feature.append((100-minDist)/100)
        feature.append(len(foods)/100)
        if not enemyPosition is None:
            distToEnemy = self.distancer.getDistance(curPosition,enemyPosition)
        else:
            distToEnemy = 100
        # if self.distancer.getDistance(prePosition,curPosition1) >1:
        #     for i in range(len(feature)):
        #         feature[i] = 0
        if (self.distancer.getDistance(prePosition,curPosition) > 1) or (self.distancer.getDistance(prePosition,curPosition1) > 1):
            # for i in range(len(feature)):
            #     feature[i] = 0
            feature.append(0.1/100) 
        else:
            feature.append(distToEnemy/100)
        if feature[-1] == 0:
            feature[-1] = 0.1/100
        # if distToEnemy == 0 or (self.distancer.getDistance(prePosition,curPosition) > 1):
        #     for i in range(len(feature)):
        #         feature[i] = 0
        #     feature.append(1)
        # else:
        #     feature.append(0)
        # if distToEnemy == 1:
        #     feature.append(1)
        # else:
        #     feature.append(0)
        # if distToEnemy == 2:
        #     feature.append(1)
        # else:
        #     feature.append(0)
        # if distToEnemy == 3:
        #     feature.append(1)
        # else:
        #     feature.append(0)
        if gameState.getScore() > pregameState.getScore():
            feature.append(1)
            if pregameState.data.agentStates[self.index].numCarrying is None:
                feature.append(0)
            else:
                feature.append(pregameState.data.agentStates[self.index].numCarrying/100)
        else:
            feature.append(0)
            if gameState.data.agentStates[self.index].numCarrying is None:
                feature.append(0)
            else:
                feature.append(gameState.data.agentStates[self.index].numCarrying/100)
        if self.distancer.getDistance(prePosition,curPosition1) >1 or (self.distancer.getDistance(prePosition,curPosition) >1):
            feature[-1] = 0
        # print(feature)
        return feature
    # def getFeature(self,pregameState,gameState):
    #     feature = []
    #     curPosition = gameState.getAgentPosition(self.index)
    #     prePosition = pregameState.getAgentPosition(self.index)
    #     enemyPostion = gameState.getAgentPosition(3)
    #     midDist = 9999
    #     for i in self.middleLine:
    #         midDist = min(midDist,self.distancer.getDistance(i,curPosition))
    #     if not enemyPostion is None:
    #         dist = self.distancer.getDistance(curPosition,enemyPostion)/100
    #     else:
    #         dist = 1
    #     if self.distancer.getDistance(prePosition,curPosition)>1:
    #         dist = 0.1/100
    #     feature.append(dist)
    #     foods = self.getFood(gameState).asList()
    #     minDist = 9999
    #     for food in foods:
    #         minDist = min(minDist,self.distancer.getDistance(food,curPosition))
    #     if (len(self.getFood(pregameState).asList()) > len(self.getFood(gameState).asList())) or (foods == []):
    #         minDist = 0
    #     feature.append((100-minDist)/100)
    #     if gameState.data.agentStates[self.index].numCarrying>0:
    #         feature.append((100-midDist)/100)
    #     else:
    #         feature.append(0)
    #     feature.append((100-len(foods))/100)
    #     scoreChange = 0
    #     feature.append(gameState.getScore()/100)
    #     # if gameState.getScore() > pregameState.getScore():
    #     #     # feature.append(pregameState.data.agentStates[self.index].numCarrying/20)
    #     #     feature.append(pregameState.data.agentStates[self.index].numCarrying)
    #     # else:
    #     #     # feature.append(gameState.data.agentStates[self.index].numCarrying/20)
    #     #     feature.append(0)
    #     # if gameState.getScore
    #     print(feature)
    #     return feature

        ##################################################################
        # return self.action.pop(0)[1]

class QLearning():
    def __init__(self,alpha,gamma):
        with open(".\model.txt","r") as F:
            read = F.readline()
        self.weights = []
        for i,value in enumerate(read.split(" ")):
            self.weights.append(float(value))
        # self.weights = np.array(self.weights)
        self.alpha = alpha
        print(self.weights)
        self.gamma = gamma

    def update(self,feature,Qcur,Qnext,reward):
        for i in range(0,len(self.weights)):
            self.weights[i] = self.weights[i]+ self.alpha * (reward + self.gamma*Qnext - Qcur) * feature[i]
            # if self.weights[2] < 0 :
            #     print("???")
            # print(self.weights[i])
        # print(self.weights)
        # print(self.weights)
        # print(str(self.weights))
        string = ""
        for i in self.weights:
            string+= str(i) + " "
        with open("./model.txt","w+") as F:
            F.write(string[:-1])


    def getQvalue(self,feature):
        feature = np.array(feature)
        weight = np.array(self.weights)
        return np.dot(weight,feature)


