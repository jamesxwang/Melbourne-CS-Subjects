--  File     : Proj1.hs
--  Author   : Xu Wang <xuwang2@student.unimelb.edu.au>
--  Origin   : Tue April 2 14:11:23 2019
--  Purpose  : Project 1 for COMP90048 Declarative Programming

module Proj1 (Pitch, toPitch, feedback,
            GameState, initialGuess, nextGuess) where

import Data.List
import Data.Ord

-- | The Possible Notes deriving Show
data Note = A | B | C | D | E | F | G
    deriving (Show, Eq, Ord)


-- | The Possible Octaves
data Octave = Oc1 | Oc2 | Oc3
    deriving (Eq, Ord)


-- | Possible Pitches
data Pitch = Pitch Note Octave
    deriving (Eq, Ord)


-- | define Gamestate: weight for each guess, the higher the best
type GameState = [([Pitch], Int)]


-- | show the actual octave from data Octave
showOctave :: Octave -> String
showOctave Oc1 = "1"
showOctave Oc2 = "2"
showOctave Oc3 = "3"
instance Show Octave where show = showOctave


-- | show the actual pitch from data Pitch
showPitch :: Pitch -> String
showPitch (Pitch n o) = show n ++ showOctave o 
instance Show Pitch where show = showPitch


-- | get all possible pitches and set it to allPitches
getAllPitches :: [Note] -> [Octave] -> [Pitch]
getAllPitches n o = [(Pitch note octave) | note <- n, octave <- o]
allPitches = getAllPitches n o
                where n = [A,B,C,D,E,F,G]
                      o = [Oc1,Oc2,Oc3]


-- | return True if the value of input string equals to pitch
match :: String -> Pitch -> Bool
match inputString p
    |   inputString == show p = True
    |   otherwise   = False


-- | gives Just the Pitch named by the string, or Nothing if the 
-- string is not a valid pitch name
toPitch :: String -> Maybe Pitch
toPitch x
    |   result /= [] = Just (head result)
    |   otherwise = Nothing
        where result = filter (match x) allPitches

-- | get note from pitch
getNote :: Pitch -> Note
getNote (Pitch note octave) = note

-- | get octave from pitch
getOctave :: Pitch -> Octave
getOctave (Pitch note octave) = octave


-- | takes a target and a guess, respectively, and returns
-- the appropriate feedback (correct pitches, correct notes, correct octaves)
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback target guess = (pitch, note, octave)
    where pitch = length (target `intersect` guess)
          note  = num - length (guessNotes \\ targetNotes) - pitch
          octave = num - length (guessOctave \\ targetOctave) - pitch
          num   = length guess
          guessNotes = map (\p -> getNote p) guess
          targetNotes = map (\p -> getNote p) target
          guessOctave = map (\p -> getOctave p) guess
          targetOctave = map (\p -> getOctave p) target


-- | returns all possible guesses after the first guess
firstGamestate :: GameState
firstGamestate = [ ([p1,p2,p3], score) | p1 <- allPitches, p2 <- allPitches, p3 <- allPitches, p1 > p2, p2 > p3]
    where score = 0


-- | takes no input arguments, and returns a pair of an initial
-- guess and a game state
initialGuess :: ([Pitch], GameState)
initialGuess = (firstGuess, firstGamestate)
    -- firstGuess = [A1,B2,C3]
    where firstGuess = [pitch1, pitch2, pitch3]
          pitch1 = Pitch A Oc1
          pitch2 = Pitch B Oc2
          pitch3 = Pitch C Oc3

-- | calculate the score for each guess
calScore :: [Pitch] -> GameState -> Int
calScore guess [] = 0
calScore guess (x:xs) = score + (calScore guess xs)
    where score = correctPitch + correctNote + correctOctave
          (correctPitch,correctNote,correctOctave) = feedback guess (fst x)


-- | takes as input a pair of the previous guess and game state,
-- and the feedback to this guess as a triple of correct pitches,
-- notes, and octaves, and returns a pair of the next guess and game state
nextGuess :: ([Pitch], GameState) -> (Int,Int,Int) -> ([Pitch], GameState)
nextGuess (guess, gameState) (pitchCount, noteCount, octaveCount) = (newGuess, newGameState)
  where newGuess     = fst $ maximumBy (comparing snd) [((fst x), (calScore (fst x) newGameState)) | x <- newGameState ]
        newGameState = [((fst singleGuess), initialScore) | singleGuess <- gameState, (feedback guess (fst singleGuess)) == (pitchCount, noteCount, octaveCount)]
        initialScore = 0
