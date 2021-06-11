# generateTournamentLayouts.py
# ----------------------------
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


import sys, random

import mazeGenerator

"""
This is a helper file which generates the random seeds for the map
layouts for the nightly tournament.
"""

if __name__=="__main__":
  num = 9
  if len(sys.argv) > 1: # command line argument: number of maps to generate
    num = int(sys.argv[1])

  seedsfile = '../driver/SEEDS'
  with open(seedsfile,'w') as out:
    pass

  for i in range(num):
    seed = random.randint(0,99999999)
    layout = 'layouts/random%08dCapture.lay' % seed
    print('Generating random layout in %s' % layout)
    with open(layout, 'w') as out:
      maze = mazeGenerator.generateMaze(seed)
      out.write(maze)
      print(maze)

    with open(seedsfile, 'a') as out:
      out.write("%d\n"%seed)



