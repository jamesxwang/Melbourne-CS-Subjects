--  File     : Assignment1.hs
--  Author   : Xu Wang <xuwang2@student.unimelb.edu.au>
--  Origin   : Thu Mar 14 00:50:00 2019
--  Purpose  : Assesment 1 for COMP90048 Declarative Programming

module Assignment1 (subst, interleave, unroll) where

-- subst takes two values and a list, and replaces every occurrence of the first value with
-- the second in the list
subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b (x:xs) =
    if a == x
        then [b] ++ subst a b xs
        else [x] ++ subst a b xs

-- interleave takes two lists and returns the interleaving of the two arguments. That, the
-- result is a list in which the first, third, fifth . . . elements come fromt the first argument
-- and the second, fourth, sixth . . . come from second. If either argument is shorter than
-- the other, the excess elements of the longer comprise the end of the resulting list
interleave :: [t] -> [t] -> [t]
interleave [] [] = []
interleave [] (x) = (x)
interleave (x) [] = (x)
interleave (x:xs) (y:ys) = 
    x:y:interleave xs ys

-- unroll takes a list and an integer and constructs a list of the specified length made up
-- by “unrolling” the input list as many times as needed to construct a list of that length.
-- That is, the output consists of the input list repeated as many times as necessary to
-- have the specified length
unroll :: Int -> [a] -> [a]
unroll x [] = []
unroll 0 x = []
unroll n (x:xs)
    | n >= length (x:xs) = [x] ++ xs ++ unroll (n-length(xs)-1) (x:xs)
    | otherwise = [x] ++ unroll (n-1) xs