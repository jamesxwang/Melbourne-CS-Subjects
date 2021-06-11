# myTeam.py
# ---------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


from captureAgents import CaptureAgent
import random, time, util, sys
from game import Directions
import game
import distanceCalculator
from util import nearestPoint

#################
# Team creation #
#################


def createTeam(firstIndex, secondIndex, isRed, first='OffensiveAgent',
                 second='DefensiveAgent'):
    """
    This function returns a list of two agents that will form the
    team, initialized using firstIndex and secondIndex as their agent
    index numbers.  isRed is True if the red team is being created, and
    will be False if the blue team is being created.
    """

    # The following line is an example only; feel free to change it.
    return [eval(first)(firstIndex), eval(second)(secondIndex)]


##########
# Agents #
##########


class InferenceAgent(CaptureAgent):
    def registerInitialState(self, gameState):
        CaptureAgent.registerInitialState(self, gameState)

        # Get the starting position of our agent.
        self.start = gameState.getInitialAgentPosition(self.index)

        # Get the midpoint of the board.
        self.midWidth = gameState.data.layout.width/2

        self.midHeight = gameState.data.layout.height/2

        # Get the legal positions that agents could be in.
        self.legalPositions = [p for p in gameState.getWalls().asList(False) if p[1] > 1]

        # So we can use maze distance.
        self.distancer.getMazeDistances()

        # Get our team agent indexes.
        self.team = self.getTeam(gameState)

        # Flag for offense.
        self.offensing = False

        # Get our enemy indexes.
        self.enemies = self.getOpponents(gameState)

        # Initialize the belief to be 1 at the initial position for each of the
        # opposition agents. The beliefs will be a dictionary of dictionaries.
        # The inner dictionaries will hold the beliefs for each agent.
        self.beliefs = {}
        for enemy in self.enemies:
            self.beliefs[enemy] = util.Counter()
            self.beliefs[enemy][gameState.getInitialAgentPosition(enemy)] = 1.


    def initializeBeliefs(self, enemy):
        """
        Tracking function from HW5 for Question 1. Initializing a uniform
        distribution for the beliefs. Meaning that when we have no knowledge
        of the state, we can assume that it is equally likely that the agent
        could be in any position.
        """

        self.beliefs[enemy] = util.Counter()

        for p in self.legalPositions:
            # This value of 1, could be anything since we will normalize it.
            self.beliefs[enemy][p] = 1.0

        self.beliefs[enemy].normalize()


    def elapseTime(self, enemy, gameState):
        """
        This is nearly the same as HW5 #2 where elapse time was done in order
        to be able to incorporate information about how pacman may move.
        In the homework we had exact information and could use getPositionDistribution.
        In this case, we will set the distribution by looking at all the
        possible successor positions and checking that they are legal positions.
        Of the legal positions we will set it to be uniformly likely to
        transition to the legal state.
        """
        new_belief = util.Counter()

        for oldPos in self.legalPositions:
            # Get the new probability distribution.
            newPosDist = util.Counter()

            # Possible positions are not moving or taking one step vertical
            # or horizontal. Some of these may not be lega.
            possiblePositions = [(oldPos[0]+i, oldPos[1]+j) for i in [-1,0,1]
                                 for j in [-1,0,1] if not (abs(i) == 1 and abs(j) == 1)]

            # Check if the positions are legal. If they are add to the new
            # distribution of possible positions.
            for p in possiblePositions:
                if p in self.legalPositions:
                    newPosDist[p] = 1.0
                else:
                    pass

            # Normalize to be unifom assuming the movement is random.
            newPosDist.normalize()

            # Get the new belief distibution.
            for newPos, prob in newPosDist.items():
                # Update the probabilities for each of the positions.
                new_belief[newPos] += prob * self.beliefs[enemy][oldPos]

        # Normalize and update the belief.
        new_belief.normalize()
        self.beliefs[enemy] = new_belief


    def observe(self, enemy, observation, gameState):
        """
        This is nearly the same as HW5 #1 where we did exact inference and
        created observations using the HMM model. Adding in are some
        tricks to get more information about the position of an enemy
        that go beyond the noisy reading so that the true position can
        be narrowed down significantly.
        """

        # Get the noisy observation for the current enemy.
        noisyDistance = observation[enemy]

        # Get the position of the calling agent.
        myPos = gameState.getAgentPosition(self.index)

        # Create new dictionary to hold the new beliefs for the current enemy.
        new_belief = util.Counter()

        # For each of the legal positions get the new belief.
        for p in self.legalPositions:
            # Calculating true distance to the position.
            trueDistance = util.manhattanDistance(myPos, p)

            # Emission probability for the current position. This is the
            # the followoing equation P(e_t|x_t).
            emissionModel = gameState.getDistanceProb(trueDistance, noisyDistance)

            # There are a few possible ways we can eliminate moves that we would
            # think could be possible based off of our readings. This is a check
            # to see if the possible move cannot be the true position because
            # the type of the agent does not match up.
            if self.red:
                pac = p[0] < self.midWidth
            else:
                pac = p[0] > self.midWidth

            # If the the true distance to the position is less than or equal to
            # 5 then we know this cannot be the true position because we
            # would have gotten it as a true reading then and not a noisy
            # distance reading so we can set the belief to 0.
            if trueDistance <= 5:
                new_belief[p] = 0.
            elif pac != gameState.getAgentState(enemy).isPacman:
                new_belief[p] = 0.
            else:
                # This equation is the online belief update that is given by
                # the following equation:
                # P(x_t|e_1:t) = P(x_t|e_1:t) * P(e_t:x_t).
                new_belief[p] = self.beliefs[enemy][p] * emissionModel

        if new_belief.totalCount() == 0:
            self.initializeBeliefs(enemy)
        else:
            # Normalize and set the new belief.
            new_belief.normalize()
            self.beliefs[enemy] = new_belief


    def chooseAction(self, gameState):
        """
        Base choose action. In this function we begin by updating our beliefs
        and elapsing time for the beliefs. We also show our beliefs on the
        screen by using the provided debugging function.
        """

        myPos = gameState.getAgentPosition(self.index)
        noisyDistances = gameState.getAgentDistances()
        newState = gameState.deepCopy()

        for enemy in self.enemies:
            enemyPos = gameState.getAgentPosition(enemy)
            if enemyPos:
                new_belief = util.Counter()
                new_belief[enemyPos] = 1.0
                self.beliefs[enemy] = new_belief
            else:
                self.elapseTime(enemy, gameState)
                self.observe(enemy, noisyDistances, gameState)

        #  self.displayDistributionsOverPositions(self.beliefs.values())

        # Using the most probable position update the game state.
        # In order to use expectimax we need to be able to have a set
        # position where the enemy is starting out.
        for enemy in self.enemies:
            probablePosition = self.beliefs[enemy].argMax()
            conf = game.Configuration(probablePosition, Directions.STOP)
            newState.data.agentStates[enemy] = game.AgentState(conf, newState.isRed(probablePosition) != newState.isOnRedTeam(enemy))

        # Do expectimax to depth 2 and get the best action to use. This is
        # the furthest out that we could do this because of time constraints.
        action = self.maxFunction(newState, depth=2)[1]

        return action


    def maxFunction(self, gameState, depth):
        """
        This is the maxFunction of expectimax in HW2. We are are choosing the
        move to maximize our expected utility for the agent on our team.
        This is done by also using the expectiFunction from HW2 to get
        the expected result of the enemy moves.
        """

        # Check for the end of the game or reaching the end of the recursion.
        if depth == 0 or gameState.isOver():
            return self.evaluationFunction(gameState), Directions.STOP

        # Get the successor game states for the possible moves.
        actions = gameState.getLegalActions(self.index)

        # We found better results when we always required a move.
        actions.remove(Directions.STOP)
        successorGameStates = [gameState.generateSuccessor(self.index, action)
                                 for action in actions]

        # Get the expected scores of enemy moves.
        scores = [self.expectiFunction(successorGameState, self.enemies[0], depth)[0]
                    for successorGameState in successorGameStates]

        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if
                         scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices)

        return bestScore, actions[chosenIndex]


    def expectiFunction(self, gameState, enemy, depth):
        """
        This is the expectimax function from HW2. This will be called for
        each of the enemy agents. Once it goes to the next level we will use
        the max function again since we will be back on our team.
        """

        # Check for end of game or reaching end of the recursion.
        if depth == 0 or gameState.isOver():
            return self.evaluationFunction(gameState), Directions.STOP

        # Get the successor game states for the possible moves.
        actions = gameState.getLegalActions(enemy)
        successorGameStates = []
        for action in actions:
            try:
                successorGameStates.append(gameState.generateSuccessor(enemy, action))
            except:
                pass

        # If there is another ghost, then call the expecti function for the
        # next ghost, otherwise call the max function for pacman.
        if enemy < max(self.enemies):
            scores = [self.expectiFunction(successorGameState, enemy + 2, depth)[0]
                        for successorGameState in successorGameStates]
        else:
            scores = [self.maxFunction(successorGameState, depth - 1)[0]
                        for successorGameState in successorGameStates]

        # Calculate the expected value.
        bestScore = sum(scores) / len(scores)

        return bestScore, Directions.STOP


    def enemyDistances(self, gameState):
        """
        If we are getting a reading for the agent distance then we will return
        this exact distance. In the case that the agent is beyond our sight
        range we will assume that the agent is in the position where our
        belief is the highest and return that position. We will then get the
        distances from the agent to the enemy.
        """
        dists = []
        for enemy in self.enemies:
            myPos = gameState.getAgentPosition(self.index)
            enemyPos = gameState.getAgentPosition(enemy)
            if enemyPos:  # This is the case we know the exact position.
                pass
            else:  # If we don't know exact position, get most likely.
                enemyPos = self.beliefs[enemy].argMax()
            dists.append((enemy, self.distancer.getDistance(myPos, enemyPos)))
        return dists


    def evaluationFunction(self, gameState):
        """
        Evaluate the utility of a game state.
        """
        util.raiseNotDefined()


class OffensiveAgent(InferenceAgent):
    """
    An offensive agent that will immediately head for the side of the opposing
    team and will never chase agents on its own team side. We use several
    features and weights that we iterated to improve by viewing games and
    results. The agent also has limits on carrying so that it will go back
    to the other side after collecting a number of food.
    """

    def registerInitialState(self, gameState):
        InferenceAgent.registerInitialState(self, gameState)
        self.retreating = False


    def chooseAction(self, gameState):
        scaredTimes = [gameState.getAgentState(enemy).scaredTimer for enemy in self.enemies]
        score = self.getScore(gameState)

        # Choose how many food to collect before attempting to turn back based
        # off the score of the game.
        if score < 7:
            carryLimit = 6
        else:
            carryLimit = 4

        # Do not set as a retreating agent if the carrying limit is not reached
        # or there is the minimum amount of food left.
        if gameState.getAgentState(self.index).numCarrying < carryLimit and len(self.getFood(gameState).asList()) > 2:
            self.retreating = False
        else:
            if min(scaredTimes) > 5: # Do not retreat but search for food.
                self.retreating = False
            else:
                self.retreating = True

        return InferenceAgent.chooseAction(self, gameState)


    def evaluationFunction(self, gameState):
        # Get the current position.
        myPos = gameState.getAgentPosition(self.index)

        # Get the food on the board.
        targetFood = self.getFood(gameState).asList()

        # Get the closest distance to the middle of the board.
        distanceFromStart = min([self.distancer.getDistance(myPos, (self.midWidth, i))
                                 for i in range(gameState.data.layout.height)
                                 if (self.midWidth, i) in self.legalPositions])

        # Getting the distances to the enemy agents that are ghosts.
        ghostDistances = []
        for enemy in self.enemies:
            if not gameState.getAgentState(enemy).isPacman:
                enemyPos = gameState.getAgentPosition(enemy)
                if enemyPos != None:
                    ghostDistances.append(self.distancer.getDistance(myPos, enemyPos))

        # Get the minimum distance of any of the ghost distances.
        # If it is greater than 4, we do not care about it so make it 0.
        minGhostDistances = min(ghostDistances) if len(ghostDistances) else 0
        if minGhostDistances >= 4:
            minGhostDistances = 0

        # Get whether there is a power pill we are chasing.
        capsulesChasing = None
        if self.red:
            capsulesChasing = gameState.getBlueCapsules()
        else:
            capsulesChasing = gameState.getRedCapsules()

        # distance and minimum distance to the capsule.
        capsulesChasingDistances = [self.distancer.getDistance(myPos, capsule) for capsule in
                                    capsulesChasing]
        minCapsuleChasingDistance = min(capsulesChasingDistances) if len(capsulesChasingDistances) else 0

        # Time to go back to safety, or trying to find food still.
        if self.retreating:
            # Want to get back to the other side at this point. Weight is on
            # staying safe and getting back to the halfway point.
            return - 2 * distanceFromStart + 500 * minGhostDistances
        else:
            # Actively looking for food.
            foodDistances = [self.distancer.getDistance(myPos, food) for
                             food in targetFood]
            minFoodDistance = min(foodDistances) if len(foodDistances) else 0
            scaredTimes = [gameState.getAgentState(enemy).scaredTimer for enemy
                             in self.enemies]

            # If they are scared be aggressive.
            if min(scaredTimes) <= 6 and minGhostDistances < 4:
                minGhostDistances *= -1

            return 2 * self.getScore(gameState) - 100 * len(targetFood) - \
                   3 * minFoodDistance - 10000 * len(capsulesChasing) - \
                   5 * minCapsuleChasingDistance + 100 * minGhostDistances


class DefensiveAgent(InferenceAgent):
    """
    This is a defensive agent that likes to attack. If there are no enemy pacman
    then the defensive agent will act on the offensive agent evaluation function.
    We do not use carry limits though because the agent will retreat when the
    other team has a pacman.
    """
    def registerInitialState(self, gameState):
        InferenceAgent.registerInitialState(self, gameState)
        self.offensing = False


    def chooseAction(self, gameState):
        # Check if the enemy has any pacman.
        invaders = [a for a in self.enemies if
                    gameState.getAgentState(a).isPacman]
        numInvaders = len(invaders)

        # Check if we have the poison active.
        scaredTimes = [gameState.getAgentState(enemy).scaredTimer for enemy in
                         self.enemies]

        # If there are no pacman on our side or the poison pill is active we
        # should act like an offensive agent.
        if numInvaders == 0 or min(scaredTimes) > 8:
            self.offensing = True
        else:
            self.offensing = False

        return InferenceAgent.chooseAction(self, gameState)


    def evaluationFunction(self, gameState):
        myPos = gameState.getAgentPosition(self.index)

        # Get the most likely enemy distances.
        enemyDistances = self.enemyDistances(gameState)

        # Get the pacman on our side.
        invaders = [a for a in self.enemies if
                    gameState.getAgentState(a).isPacman]

        # Get the distance to the pacman and find the minimum.
        pac_distances = [dist for id, dist in enemyDistances if
                         gameState.getAgentState(id).isPacman]
        minPacDistances = min(pac_distances) if len(pac_distances) else 0

        # Find the distance to the ghosts and find the minimum.
        ghost_distances = [dist for id, dist in enemyDistances if
                             not gameState.getAgentState(id).isPacman]
        minGhostDistances = min(ghost_distances) if len(ghost_distances) else 0

        # Get min distance to a food.
        targetFood = self.getFood(gameState).asList()
        foodDistances = [self.distancer.getDistance(myPos, food) for food in
                         targetFood]
        minFoodDistance = min(foodDistances) if len(foodDistances) else 0

        # Get min distance to a power pill.
        capsules = self.getCapsulesYouAreDefending(gameState)
        capsulesDistances = [self.getMazeDistance(myPos, capsule) for capsule in
                             capsules]
        minCapsuleDistance = min(capsulesDistances) if len(capsulesDistances) else 0


        if self.offensing == False:
            return -999999 * len(invaders) - 10 * minPacDistances - minCapsuleDistance
        else:
            return 2 * self.getScore(gameState) - 100 * len(targetFood) - \
                   3 * minFoodDistance + minGhostDistances

