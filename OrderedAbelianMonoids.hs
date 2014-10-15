{-
OrderedAbelianMonoids.hs

Copyright 2014 Mike Haskel <mhaskel@nd.edu>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

-- |Count finite ordered abelian monoids.
module OrderedAbelianMonoids where
import Data.Array.Unboxed
import System.Environment
import System.Exit

-- |Represent an abelian magma such that pieces of the representation can be shared.
type AbMagmaShareable = [[Int]]

-- |Add elements of a magma represented by 'AbMagmaShareable'.
--
-- Precondition: i <= j < size, and the magma is of the given size.
-- These conditions are not checked.
addShareable :: Int -- ^size
                -> AbMagmaShareable
                -> Int -- ^i
                -> Int -- ^j
                -> Int
addShareable sz magma i j = sz - (magma !! i !! j) - 1

-- |Represent an abelian magma such the addition operation is fast.
type AbMagmaFast = UArray Int Int

-- |Add elements of a magma represented by 'AbMagmaFast'.
--
-- Precondition: i <= j < size, and the magma is of the given size.
-- These conditions are not checked.
addFast :: Int -- ^size
           -> AbMagmaFast
           -> Int -- ^i
           -> Int -- ^j
           -> Int
addFast sz magma i j = magma ! ix
  where ix = i * (2*sz - i + 1) `div` 2 + j - i

-- |Similar to 'addFast', but the arguments need not be in order.
addFastFlexible :: Int -- ^size
                   -> AbMagmaFast
                   -> Int -- ^i
                   -> Int -- ^j
                   -> Int
addFastFlexible sz magma i j = addFast sz magma (min i j) (max i j)

-- |Convenrt 'AbMagmaShareable' to 'AbMagmaFast'.
shareableToFast :: Int -- ^size
                   -> AbMagmaShareable
                   -> AbMagmaFast
shareableToFast sz shareable = listArray (0, arrlen-1) list
  where arrlen = sz * (sz + 1) `div` 2
        list = [ sz - e - 1 |
                 row <- shareable,
                 e <- row ]

-- |Represents a single test of associativity.
--
-- The first coordinate is the pivot, and permuting the second two
-- coordinates yields the same test.
type AssociativityTest = (Int, (Int, Int))

-- |Generate all the associativity tests involving at least one use of
-- |the element 0.
associativityTestsInvolvingZero :: Int -- ^size
                                   -> [AssociativityTest]
associativityTestsInvolvingZero sz =
  [ (0, (i, j)) |
    i <- [0..sz-1],
    j <- [i..sz-1] ] ++
  [ (p, (0, j)) |
    p <- [1..sz-1],
    j <- [0..sz-1] ]

-- |Run a single associativity test.
isAssociativeAt :: Int -- ^size
                   -> AbMagmaFast
                   -> AssociativityTest
                   -> Bool
isAssociativeAt sz magma (p, (i, j)) =
  (i `add` p) `add` j ==
  i `add` (p `add` j)
  where add = addFastFlexible sz magma

-- |Test to see if a magma is associative, under the assumption that
-- |any associativity problems will involve 0.
isAssociativeInvolvingZero :: Int -- ^size
                              -> AbMagmaFast
                              -> Bool
isAssociativeInvolvingZero sz magma =
  all (isAssociativeAt sz magma) $ associativityTestsInvolvingZero sz


-- |List the ordered abelian unitary monoids of a given size.
--
-- The unit is assumed to be the least element, and does not
-- contribute to the size.
orderedAbelianMonoids :: Int -- ^size
                         -> [AbMagmaShareable]
orderedAbelianMonoids 0 = [[[]]]
orderedAbelianMonoids sz =
  [ shareable |
    smaller <- orderedAbelianMonoids (sz-1),
    newRow <- goodExpansions sz $ head smaller,
    let shareable = newRow : smaller,
    let fast = shareableToFast sz shareable,
    isAssociativeInvolvingZero sz fast ]
  where goodExpansions sz [] =
          [ [n] |
            n <- [0..sz-1] ]
        goodExpansions sz oldRow =
          [ n:newRow |
            newRow <- go (sz-2) oldRow,
            n <- [head newRow..sz-1] ]
        go upperBound [] = [[]]
        go upperBound [old] =
          [ [n] |
            n <- [old..upperBound] ]
        go upperBound oldRow =
          [ n:newRow |
            newRow <- go (upperBound-1) $ tail oldRow,
            n <- [max (head oldRow) (head newRow)..upperBound] ]

topLevel :: IO ()
topLevel =
  do args <- getArgs
     case args of
       [sz_str] -> case reads sz_str of
         [(sz,"")] -> runMain sz
         _ -> printUsage >> exitFailure
       _ -> printUsage >> exitFailure

printUsage :: IO ()
printUsage =
  do progName <- getProgName
     putStrLn $ "Usage: " ++ progName ++ " size"

runMain :: Int -> IO ()
runMain size =
  do let results = orderedAbelianMonoids size
     mapM_ print results
