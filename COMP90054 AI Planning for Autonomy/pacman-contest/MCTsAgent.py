from captureAgents import CaptureAgent
from newUCT import mcts
from State_2 import State_2

def createTeam(firstIndex, secondIndex, isRed, first = 'MCTsAgent', second = 'MCTsAgent'):
    return [eval(first)(firstIndex), eval(second)(secondIndex)]

class MCTsAgent(CaptureAgent):

    def registerInitialState(self, gameState):
        CaptureAgent.registerInitialState(self, gameState)
        # self.test = mcts(iterationLimit=10)
        self.test = mcts(timeLimit=800)
        self.wallList = gameState.getWalls().asList()

    def chooseAction(self, gameState):
        # t = State_2(self, gameState, self.getScore(gameState), gameState.data.agentStates[self.index].numCarrying)
        t = State_2(gameState,
                    self.getScore(gameState),
                    gameState.data.agentStates[self.index].numCarrying,
                    self.index,
                    self.red,
                    self.getFood(gameState).asList(),
                    self.distancer.getDistance,
                    int((gameState.data.layout.width - 2) / 2 ) if self.red else int((gameState.data.layout.width - 2) / 2 + 1),
                    self.getOurMiddleLine(gameState, self.red)
                    )

        return self.test.search(t)

        ##################################################################
        # return self.action.pop(0)[1]

    def getOurMiddleLine(self, gameState, isRed):
        middleLine = []
        mapWidth = gameState.data.layout.width
        mapHeight = gameState.data.layout.height
        if isRed:
          x = int((mapWidth - 2) / 2 )
        else:
          x = int((mapWidth - 2) / 2 + 1)
        wallList = self.wallList
        for y in range(1, mapHeight):
          if (x, y) not in wallList:
            middleLine.append((x,y))
        return middleLine
