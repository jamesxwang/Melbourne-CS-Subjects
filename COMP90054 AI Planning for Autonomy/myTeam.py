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


from captureAgents import CaptureAgent
import sys
import random, time, util
from game import Directions
import game
import math
from util import PriorityQueue
from util import nearestPoint

#################
# Team creation #
#################

def createTeam(firstIndex, secondIndex, isRed,
               first = 'OffensiveDummyAgent', second = 'OffensiveDummyAgent'):
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

class DummyAgent(CaptureAgent):
  """
  A Dummy agent to serve as an example of the necessary agent structure.
  You should look at baselineTeam.py for more details about how to
  create an agent as this is the bare minimum.
  """

  def registerInitialState(self, gameState):
    """
    This method handles the initial setup of the
    agent to populate useful fields (such as what team
    we're on).

    A distanceCalculator instance caches the maze distances
    between each pair of positions, so your agents can use:
    self.distancer.getDistance(p1, p2)

    IMPORTANT: This method may run for at most 15 seconds.
    """

    '''
    Make sure you do not delete the following line. If you would like to
    use Manhattan distances instead of maze distances in order to save
    on initialization time, please take a look at
    CaptureAgent.registerInitialState in captureAgents.py.
    '''
    CaptureAgent.registerInitialState(self, gameState)

    '''
    Your initialization code goes here, if you need any.
    '''
    self.mode='Start'
    self.protectedFood=self.getFoodYouAreDefending(gameState).asList()
    self.distancer.getMazeDistances()
    self.bestProtecPos=None

  def getSuccessor(self, gameState, action):
    """
    Finds the next successor which is a grid position (location tuple).
    """
    successor = gameState.generateSuccessor(self.index, action)
    pos = successor.getAgentState(self.index).getPosition()
    self.debugDraw(pos,[1,0,0],clear=True)
    if pos != nearestPoint(pos):
      # Only half a grid position was covered
      return successor.generateSuccessor(self.index, action)
    else:
      return successor

  def evaluate(self, gameState, action):
    """
    Computes a linear combination of features and feature weights
    """
    features = self.getFeatures(gameState, action)
    weights = self.getWeights(gameState, action)
    return features * weights

  def getFeatures(self, gameState, action):
    """
    Returns a counter of features for the state
    """
    features = util.Counter()
    successor = self.getSuccessor(gameState, action)

    if action == Directions.STOP: features['stop'] = 1
    rev = Directions.REVERSE[gameState.getAgentState(self.index).configuration.direction]
    if action == rev: features['reverse'] = 1
    return features

  def getWeights(self, gameState, action):
    """
    Normally, weights do not depend on the gamestate.  They can be either
    a counter or a dictionary.
    """
    return {'stop': -100, 'reverse': -2}

  def chooseAction(self,gameState):
    actions = gameState.getLegalActions(self.index)
    values = [self.evaluate(gameState,a) for a in actions]
    maxValue = max(values)
    bestActions = [a for a, v in zip(actions,values) if v == maxValue]
    return random.choice(bestActions)

class OffensiveDummyAgent(DummyAgent):
  """
  A reflex agent that seeks food. This is an agent
  we give you to get an idea of what an offensive agent might look like,
  but it is by no means the best or only way to build an offensive agent.
  """

  def getBestFrontPos(self,gameState):
    layoutWidth=gameState.data.layout.width
    layoutHeight=gameState.data.layout.height
    if self.red:
        targetPos=(layoutWidth/2,layoutHeight/2)
    else:
        targetPos=(layoutWidth/2+1,layoutHeight/2+1)

    return (16,8)
#Use Manhattan distance as heuristic and find the path to it.
  def aStarSearch(self,gameState,targetPos):
    openList=PriorityQueue()
    closedList=[]
    bestG={}
    path=[]
    rootState=gameState.getAgentState(self.index).getPosition()
    rootNode=(rootState,None,None,0) #state,parent node pointer, action,cost from parent node
    openList.push(rootNode,1)
    while (openList.isEmpty()!=True):
        currentNode=openList.pop()
        # print(currentNode[0])
        if ((currentNode[0] not in closedList) or (currentNode[3]<bestG[currentNode[0]])):
            closedList.append(currentNode[0])
            bestG[currentNode[0]]=currentNode[3]
            #if current node is targetPos return a path
            if (currentNode[0][0]==targetPos[0] and currentNode[0][1]==targetPos[1]):
                #retrun and extract the path to node
                node=currentNode
                while (node[1]!=None):
                    path.insert(0,node[2])
                    node=node[1]
                return path
            currentSuccessors=self.getValidAdjectPos(currentNode[0],gameState)
            for value in currentSuccessors:
                tempNode=(value[0],currentNode,value[1],1+currentNode[3]) #make node for each successor,calculate the g cost from root node
                if(self.getManhantanDistance(tempNode[0],targetPos)<float('inf')):
                    openList.push(tempNode,tempNode[3]+self.getManhantanDistance(tempNode[0],targetPos) )

    return None

  def getManhantanDistance(self,start,target):
    return abs(start[0]-target[0])+abs(start[1]-target[1])


  def getValidAdjectPos(self,pos,gameState):
    allValidPos=gameState.getWalls().asList(False)
    validAdjectPos=[]
    northPos=(pos[0],pos[1]+1)
    southPos=(pos[0],pos[1]-1)
    westPos=(pos[0]-1,pos[1])
    eastPos=(pos[0]+1,pos[1])
    if northPos in allValidPos:
        validAdjectPos.append((northPos,'North'))
    if southPos in allValidPos:
        validAdjectPos.append((southPos,'South'))
    if westPos in allValidPos:
        validAdjectPos.append((westPos,'West'))
    if eastPos in allValidPos:
        validAdjectPos.append((eastPos,'East'))

    return tuple(validAdjectPos)

  def mctsSearch(self,gameState):
    '''
    create RootNode for current position
    '''
    initState=gameState.getAgentState(self.index)
    rootNode=[initState,None,[],0,0]# visited child Node,reward and access times
    '''
    print(self.index)
    print(initState)
    print(initState.getDirection())
    print(rootNode)
    '''
#startTime=time.time()
    expandTimes=0
    while(expandTimes<10):
        # print('Expand Times')
        # print(expandTimes)
        expandTimes=expandTimes+1
        #select a node to do simulation
        (expandNode,simulationState)=self.treePolicy(rootNode,gameState)
        # print(expandNode[0])
        # print(expandNode[1][0].getPosition())
        #do simulation and get the reward
        reward=self.defaultPolicy(expandNode,self.getSuccessor(simulationState,expandNode[0].getDirection()))
        #backprograte

        self.backup(expandNode,reward)
#input()
        '''
        print(expandNode[0])
        print(expandNode[1][0].getPosition())
        print(reward)
        '''
#input()
    #choose the best child node
    bestChild=self.bestChild(rootNode,False)

    return bestChild

#expand a node in specified node
  def expand(self,node,gameState):
    triedSubNodeStates=[subNode[0].getPosition() for subNode in node[2]]
    #random action until generate a new state haven't been visited
    findAction=True
    while findAction:
        actions=gameState.getLegalActions(self.index)
        actions.remove(Directions.STOP)
        currentDirection=node[0].getDirection()
        reversedDirection=Directions.REVERSE[currentDirection]
        if reversedDirection in actions and len(actions)>1:
            actions.remove(reversedDirection)
        for action in actions:
            successorState=self.getSuccessor(gameState,action)
            newState=successorState.getAgentState(self.index)
            if newState.getPosition() not in triedSubNodeStates:
                findAction=False
                break;
    subNode=[newState,node,[],0,0]
    node[2].append(subNode)
    return subNode


#expand a node to do simulation
  def treePolicy(self,node,gameState):
    simulationState=gameState.deepCopy()
    while len(self.getFood(simulationState).asList())>2 or  node[0].isPacman==True :
        actions=simulationState.getLegalActions(self.index)
        actions.remove(Directions.STOP)
        currentDirection=node[0].getDirection()
        reversedDirection=Directions.REVERSE[currentDirection]
        if reversedDirection in actions and len(actions)>1:
            actions.remove(reversedDirection)
        if len(node[2])==len(actions):
            node=self.bestChild(node,True) # accoring to UCB to find the most suitable node to do expand
            simulationState=self.getSuccessor(simulationState,node[0].getDirection())
        else:
            #return one child node that currentNode hasn't visited
            subNode=self.expand(node,simulationState)
            return (subNode,simulationState)

    return (node,simulationState)

  def defaultPolicy(self,node,gameState):
    simulationState=gameState.deepCopy()
    reward=0
    numSimulation=0
# or simulationState.getAgentState(self.index).isPacman==True
    while len(self.getFood(simulationState).asList())>2 and numSimulation<5:
        numSimulation=numSimulation+1
        actions=simulationState.getLegalActions(self.index)
        actions.remove(Directions.STOP)

        currentDirection = simulationState.getAgentState(self.index).configuration.direction
#print(currentDirection)
        # The agent should not use the reverse direction during simulation
        reversedDirection = Directions.REVERSE[currentDirection]
        if reversedDirection in actions and len(actions) > 1:
            actions.remove(reversedDirection)
#       print(actions)
#        input()
        action=random.choice(actions)
#print('selected action')
#        print(action)
        successor=self.getSuccessor(simulationState,action)
        myState = successor.getAgentState(self.index)
        myPos=myState.getPosition()
#print(myPos)
#        if self.getFood(simulationState)[int(myPos[0])][int(myPos[1])]:
#            reward=reward+10000
#print('eat food')
#print(myPos)
        simulationState=successor
    reward=self.evaluate(simulationState,None)
    myState=simulationState.getAgentState(self.index)
    # print(myState.getPosition())
    # print(reward)
    return reward*1.0

  def bestChild(self,node,isExploration):
     # TODO: Use the min float value
     bestScore = -sys.maxsize
     bestSubNode = None
    # Travel all sub nodes to find the best one
     for subNode in node[2]:
        # Ignore exploration for inference
        if isExploration:
            C = 1 / math.sqrt(2.0)
        else:
            C = 0.0

        # UCB = quality / times + C * sqrt(2 * ln(total_times) / times)
        # if subNode[4]==0:
        #     print(subNode[0].getPosition())
        #     print(subNode[1][0].getPosition())
        #     print(subNode[3])
        #     print(subNode[4])
        left = subNode[3] / subNode[4]

        right = 2.0 * math.log(node[4] *1.0/ subNode[4])
        score = left + C * math.sqrt(right)

        if score > bestScore:
            bestSubNode = subNode
            bestScore = score

     return bestSubNode

  def backup(self,node,reward):
    while node !=None:
        node[4]=node[4]+1
        node[3]=node[3]+reward
        node=node[1]

  def isChase(self,gameState):
    enemies = [gameState.getAgentState(i) for i in self.getOpponents(gameState)]
    invaders = [a for a in enemies if a.isPacman and a.getPosition() != None]
    if len(invaders)>0:
        self.mode='Chase'
    else:
        if self.mode=='Chase':
            self.mode='Attack'
  def locateBestPosToProtect(self,pacmanPos,gameState):
    nearestFood=None
    nearestFoodDis=float('inf')
    myPos=gameState.getAgentState(self.index).getPosition()

    for food in self.protectedFood:
        dis=self.getMazeDistance(food,pacmanPos)
        if dis<nearestFoodDis:
            nearestFoodDis=dis
            nearestFood=food
    return nearestFood

  def isAttacked(self,gameState):
    curFoodList=self.getFoodYouAreDefending(gameState).asList()
    if len(curFoodList)<len(self.protectedFood):
        #Protected Food is attacked
#input()
        self.mode=='Defend'
        for food in self.protectedFood:
            if food not in curFoodList:
                self.protectedFood=curFoodList
                return (self.locateBestPosToProtect(food,gameState),True)
    else:
        return False

  def getFeatures(self, gameState, action):
    if self.mode=='Attack':
        features = util.Counter()
        myState = gameState.getAgentState(self.index)
        features['successorScore']=-len(self.getFood(gameState).asList())
        myPos = myState.getPosition()
        foodList=self.getFood(gameState).asList()
        if len(foodList) > 0: # This should always be True,  but better safe than sorry
          minDistance = min([self.getMazeDistance(myPos, food) for food in foodList])
          features['distanceToFood'] = minDistance
        return features
    elif self.mode=='Chase':
        features = util.Counter()
        successor = self.getSuccessor(gameState, action)

        myState = successor.getAgentState(self.index)
        myPos = myState.getPosition()

        # Computes whether we're on defense (1) or offense (0)
        features['onDefense'] = 1
        if myState.isPacman: features['onDefense'] = 0

        # Computes distance to invaders we can see
        enemies = [successor.getAgentState(i) for i in self.getOpponents(successor)]
        invaders = [a for a in enemies if a.isPacman and a.getPosition() != None]
        features['numInvaders'] = len(invaders)
        if len(invaders) > 0:
          dists = [self.getMazeDistance(myPos, a.getPosition()) for a in invaders]
          features['invaderDistance'] = min(dists)

        if action == Directions.STOP: features['stop'] = 1
        rev = Directions.REVERSE[gameState.getAgentState(self.index).configuration.direction]
        if action == rev: features['reverse'] = 1

        return features

  def getWeights(self, gameState, action):
    if self.mode=='Attack':
        return {'successorScore': 1000, 'distanceToFood': -20}
    elif self.mode=='Chase':
        return {'numInvaders': -1000, 'onDefense': 100, 'invaderDistance': -10, 'stop': -100, 'reverse': -2}


  def chooseAction(self,gameState):
#actions = gameState.getLegalActions(self.index)
#print(actions)
#input()
    if self.mode=='Start':
        target=self.getBestFrontPos(gameState)
        myPos=gameState.getAgentState(self.index).getPosition()
        if target!=myPos:
            actionPlan=self.aStarSearch(gameState,target)
            if actionPlan!=None:
                return actionPlan[0]
        else:
            self.mode='Attack'
    self.isChase(gameState)
    if self.mode=='Chase':
        actions = gameState.getLegalActions(self.index)
        values = [self.evaluate(gameState,a) for a in actions]
        maxValue = max(values)
        bestActions = [a for a, v in zip(actions,values) if v == maxValue]
        return random.choice(bestActions)
    foodState=self.isAttacked(gameState)
    if foodState:
        self.mode='Defend'
        self.bestProtecPos=foodState[0]
    if self.mode=='Defend':
        actionPlan=self.aStarSearch(gameState,self.bestProtecPos)
        if actionPlan!=None:
            return actionPlan[0]

    actions = gameState.getLegalActions(self.index)
    '''
    values = [self.evaluate(gameState,a) for a in actions]
    maxValue = max(values)
    bestActions = [a for a, v in zip(actions,values) if v == maxValue]
    '''
    bestChild=self.mctsSearch(gameState)
    # print(bestChild[0].getDirection())
    # print('best child action')
#return random.choice(bestActions)
    return bestChild[0].getDirection()
