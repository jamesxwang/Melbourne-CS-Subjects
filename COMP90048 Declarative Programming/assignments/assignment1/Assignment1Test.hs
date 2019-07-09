--  File     : publictests.hs
--  Author   : Peter Schachte
--  Purpose  : test cases for Lab1 project

import Assignment1
import HaskellTest

suite =
  TimeLimit 2.0 $
  Suite [
    expect (subst 0 1 [0,1,2,3]) ([1,1,2,3]),
    expect (subst 0 1 []) ([]),
    expect (subst 2 3 [2,3,4,5]) ([3,3,4,5]),
    expect (subst 6 3 [2,3,4,6,6]) ([2,3,4,3,3]),
    expect (interleave [1,2,3,4] [11,12,13,14]) ([1,11,2,12,3,13,4,14]),
    expect (interleave [1,2,3,4] []) ([1,2,3,4]),
    expect (interleave [] [11,12,13,14]) ([11,12,13,14]),
    expect (interleave [1] [11,12,13]) ([1,11,12,13]),
    expect (interleave [1,2,3,4] [11,12,13]) ([1,11,2,12,3,13,4]),
    expect (interleave [1,2,3] [11,12,13,14]) ([1,11,2,12,3,13,14]),
    expect (unroll 3 [1,2,3,4,5]) ([1,2,3])
    ]

main :: IO ()
main = do
  testVerbose suite
