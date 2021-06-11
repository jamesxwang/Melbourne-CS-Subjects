from __future__ import division

import time
import math
import random



def randomPolicy(state):
    reward = 0
    # for i in range(0,10):
    limit = 0
    # (not state.isTerminal()) and
    while (limit < 15):
        try:
            action = random.choice(state.getPossibleActions())
        except IndexError:
            raise Exception("Non-terminal state has no possible actions: " + str(state))
        reward += state.getReward() * (0.9 ** limit)
        state = state.takeAction(action)
        limit += 1
    # print(reward)
    return reward
        # discont = 0.9**limit
        # r = state.getReward()
        # reward += r

    # while limit<20:
    #     try:
    #         action = random.choice(state.getPossibleActions())
    #     except IndexError:
    #         raise Exception("Non-terminal state has no possible actions: " + str(state))
    #     state = state.takeAction(action)
    #     limit += 1
    #     discont = 0.9**limit
    #     r = state.getReward()
    #     reward += r
        # reward += state.getReward()
    # reward += state.getReward()* 0.9**limit
    # print(state.getReward())
    # reward = state.getReward() * (0.99 ** limit)
    # print(state.getReward())
    # print(reward)
    return reward


class treeNode():
    def __init__(self, state, parent):
        self.state = state
        self.isTerminal = state.isTerminal()
            # state.isTerminal()
        self.isFullyExpanded = self.isTerminal
        self.parent = parent
        self.numVisits = 0
        self.totalReward = 0
        self.nodeReward = self.state.getReward()
        self.children = {}


class mcts():
    def __init__(self, timeLimit=None, iterationLimit=None, explorationConstant=0.1,
                 rolloutPolicy=randomPolicy, disfactor = 0.9):
        if timeLimit != None:
            if iterationLimit != None:
                raise ValueError("Cannot have both a time limit and an iteration limit")
            # time taken for each MCTS search in milliseconds
            self.timeLimit = timeLimit
            self.limitType = 'time'
        else:
            if iterationLimit == None:
                raise ValueError("Must have either a time limit or an iteration limit")
            # number of iterations of the search
            if iterationLimit < 1:
                raise ValueError("Iteration limit must be greater than one")
            self.searchLimit = iterationLimit
            self.limitType = 'iterations'
        self.explorationConstant = explorationConstant
        self.rollout = rolloutPolicy
        self.disfactor = disfactor

    def search(self, initialState):
        self.root = treeNode(initialState, None)
        self.count = 0
        if self.limitType == 'time':
            timeLimit = time.time() + self.timeLimit / 1000
            while time.time() < timeLimit:
                self.count += 1
                self.executeRound()
        else:
            for i in range(self.searchLimit):
                self.executeRound()
        bestChild = self.getBestChild(self.root, 0)
        # bestChild = None
        # maxValue = 0
        # for i in self.root.children.values():
        #     print(i.numVisits)
        # # #     if maxValue < i.totalReward:
        # # #         maxValue = i.totalReward
        # # #         bestChild = i
        # print("=====================")
        # # print(self.count)
        return self.getAction(self.root, bestChild)

    def executeRound(self):
        node = self.selectNode(self.root)
        reward = self.rollout(node.state)
        self.backpropogate(node, reward)

    def selectNode(self, node):
        while not node.isTerminal:
            if node.isFullyExpanded:
                node = self.getBestChild(node, self.explorationConstant)
            else:
                return self.expand(node)
        return node

    def expand(self, node):
        actions = node.state.getPossibleActions()
        for action in actions:
            if action not in node.children:
                newNode = treeNode(node.state.takeAction(action), node)
                node.children[action] = newNode
                if len(actions) == len(node.children):
                    node.isFullyExpanded = True
                return newNode

        raise Exception("Should never reach here")

    # def backpropogate(self,node,reward):
    #     node.numVisits += 1
    #     node.totalReward = node.nodeReward + reward
    #     # node.totalReward = reward
    #     node = node.parent
    #     while node is not None:
    #         node.numVisits +=1
    #         maxReward = 0
    #         for i in node.children.values():
    #             maxReward = max(maxReward, i.totalReward)
    #         node.totalReward = node.nodeReward + maxReward*self.disfactor
    #         # node.totalReward += maxReward * self.disfactor
    #         node = node.parent

    def backpropogate(self, node, reward):
        num = 0
        while node is not None:
            node.numVisits += 1
            node.totalReward += reward
            num += 1
            node = node.parent

    def getBestChild(self, node, explorationValue):
        bestValue = float("-inf")
        bestNodes = []

        for child in node.children.values():
            nodeValue = child.totalReward / child.numVisits + explorationValue * math.sqrt(
                2 * math.log(node.numVisits) / child.numVisits)
            # nodeValue = child.totalReward  + explorationValue * math.sqrt(
            #     2 * math.log(node.numVisits) / child.numVisits)
            if nodeValue > bestValue:
                bestValue = nodeValue
                bestNodes = [child]
            elif nodeValue == bestValue:
                bestNodes.append(child)
        return random.choice(bestNodes)

    def getAction(self, root, bestChild):
        for action, node in root.children.items():
            if node is bestChild:
                return action
