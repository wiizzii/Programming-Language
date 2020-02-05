{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
module Main where

import SiegeGame
import SiegeGameImpl
import Game

import Data.Map (Map)
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit.Base hiding (Test)
import Test.QuickCheck

-- placér mellem 0 og 24 attackers.
-- placér 2 defenders.
-- Fremgangsmåde: Shuffle liste af vertices

attackerMax :: SiegeGame -> Int
attackerMax (SiegeGame g _) = length [ v | (v, vi) <- M.toList g, initialPiece vi == Just True ]

randomGameState :: [V] -> Int -> Int -> Gen (Map V Player)
randomGameState vs amax dcount = do
  acount        <- choose (0,amax)
  vs'           <- shuffle vs
  return $ M.fromList $ zip vs' (replicate dcount False ++ replicate acount True)

prop_placeDefenders = let sg@(SiegeGame g _) = defaultGame
                      in forAll (randomPlaceDefenders sg) $ \m ->
                         forAll (randomStateFromMove sg m) $ \s ->
                      (
                        length [ v | (v,False) <- M.toList $ moveImpl m s ] == 2 &&
                        length [ v | (v,True ) <- M.toList $ moveImpl m s ] == length [ v | (v,True) <- M.toList s ]
                      )

prop_simpleMove = let sg@(SiegeGame g _) = defaultGame
                      in forAll (randomSimpleMove sg) $ \m ->
                         forAll (randomStateFromMove sg m) $ \s ->
                      (
                        length [ v | (v,False) <- M.toList $ moveImpl m s ] == length [ v | (v,False) <- M.toList s ] &&
                        length [ v | (v,True ) <- M.toList $ moveImpl m s ] == length [ v | (v,True) <- M.toList s ]
                      )

prop_captureMove = let sg@(SiegeGame g _) = defaultGame
                      in forAll (randomCaptureMove sg) $ \m@(CaptureMove v cs) ->
                         forAll (randomStateFromMove sg m) $ \s ->
                      (
                        length [ v | (v,False) <- M.toList $ moveImpl m s ] == length [ v | (v,False) <- M.toList s ] &&
                        length [ v | (v,True ) <- M.toList $ moveImpl m s ] == length [ v | (v,True) <- M.toList s ] - length cs
                      )

prop_initialGame = let
  sg@(SiegeGame g _) = defaultGame
  s = startStateImpl sg
  in and [M.lookup v s == initialPiece vi  | (v,vi) <- M.toList g]

randomPlaceDefenders :: SiegeGame -> Gen SiegeMove
randomPlaceDefenders sg = do
  vs <- shuffle $ gameVertices sg
  return $ PlaceDefenders $ take 2 vs

randomSimpleMove :: SiegeGame -> Gen SiegeMove
randomSimpleMove (SiegeGame g _) = do
  let cs = [ SimpleMove v' v' | (v,vi) <- M.toList g, (NeighborInfo v' _) <- neighbors vi ]
  cs' <- shuffle cs
  return $ head cs

--
candidateCaptureNodes :: SiegeGame -> V -> [V]
candidateCaptureNodes (SiegeGame g _) v@(x,y) = case M.lookup v g of
  Just vi -> [ v' | NeighborInfo v'@(x',y') _ <- neighbors vi, isJust $ M.lookup (2 * x' - x, 2 * y' - y) g ]
  Nothing -> []

randomCapturePath :: SiegeGame -> V -> [V] -> Gen [V]
randomCapturePath sg v@(x,y) vs = do
  cs <- shuffle (candidateCaptureNodes sg v \\ vs)
  case cs of
    [] -> return []
    (v'@(x',y'):_) -> do
      vs' <- randomCapturePath sg (2 * x' + x, 2 * y' + y) (v':vs)
      return $ v' : vs'

gameVertices :: SiegeGame -> [V]
gameVertices (SiegeGame g _) = [ v | (v, _) <- M.toList g ]

randomCaptureMove :: SiegeGame -> Gen SiegeMove
randomCaptureMove sg = do
  (v:_) <- shuffle $ gameVertices sg
  vs <- randomCapturePath sg v []
  i <- choose (1,length vs)
  return $ CaptureMove v (take i vs)

verticesBetweenCaptures :: V -> [V] -> [V]
verticesBetweenCaptures = scanl (\(x,y) (x',y') -> (2*x' - x, 2*y' - y))

randomStateFromMove :: SiegeGame -> SiegeMove -> Gen (Map V Player)
randomStateFromMove sg (PlaceDefenders ds) = do
  let vs = gameVertices sg \\ ds
  let amax = attackerMax sg
  randomGameState vs amax 0
randomStateFromMove sg (SimpleMove v v') = do
  let vs = gameVertices sg \\ [v,v']
  let amax = attackerMax sg
  s <- randomGameState vs amax 1
  return $ M.insert v False s
randomStateFromMove sg (CaptureMove v cs) = do
  let vs = (gameVertices sg \\ cs) \\ verticesBetweenCaptures v cs
  let amax = attackerMax sg - length cs
  s <- randomGameState vs amax 1
  return $ M.union (M.fromList $ (v,False) : zip cs (repeat True)) s

hasVertex :: SiegeGame -> V -> Bool
hasVertex (SiegeGame g _) v = isJust $ M.lookup v g

hasNeighbors :: SiegeGame -> V -> V -> Bool
hasNeighbors (SiegeGame g _) v v' = maybe False (\vi -> not.null $ [v' | NeighborInfo v'' _ <- neighbors vi, v'' == v']) (M.lookup v g)

hasCaptureSequence :: SiegeGame -> V -> [V] -> Bool
hasCaptureSequence sg v [] = hasVertex sg v
hasCaptureSequence sg v@(x,y) (c@(x',y'):cs) = hasNeighbors sg v c && hasCaptureSequence sg (2 * x' - x, 2 * y' - y) cs

isValidMove :: SiegeGame -> SiegeMove -> Bool
isValidMove sg (PlaceDefenders vs) = all (hasVertex sg) vs
isValidMove sg (SimpleMove v v')   = hasNeighbors sg v v'
isValidMove sg (CaptureMove v cs)  = hasCaptureSequence sg v cs

--randomGameState :: [V] -> Int -> Int -> Gen (Map V Player)
--randomGameState vs amax dcount = do

prop_validMoves = let sg = defaultGame
                      in forAll (randomGameState (gameVertices sg) (attackerMax sg) 2) $ \state ->
                      forAll (arbitrary :: Gen Bool) $ \player -> let ms = movesImpl sg player state in
                      not (null ms) ==> forAll (elements ms) $ \m -> isValidMove sg m


isValidMove2 :: Player -> Map V Player -> SiegeMove -> Bool
isValidMove2 _ s (PlaceDefenders vs) = all (\v -> isNothing $ M.lookup v s) vs
isValidMove2 p s (SimpleMove v v')   = M.lookup v s == Just p && isNothing (M.lookup v' s)
isValidMove2 _ s (CaptureMove v cs)  = M.lookup v s == Just False && all (\v -> isNothing $ M.lookup v s) is && all (\v -> M.lookup v s == Just True) cs
  where is = nub (verticesBetweenCaptures v cs) \\ (v : cs)


prop_validMoves2 = let sg = defaultGame
                   in forAll (randomGameState (gameVertices sg) (attackerMax sg) 2) $ \state ->
                      forAll (arbitrary :: Gen Bool) $ \player -> let ms = movesImpl sg player state in
                      not (null ms) ==> forAll (elements ms) $ \m -> isValidMove2 player state m

prop_placeDefenders_unique = length (nub xss) == length xss
  where
    sg = defaultGame
    s  = startStateImpl sg
    xss = [ sort xs | PlaceDefenders xs <- movesImpl sg False s]


testState1 = M.fromList [ ((-1,1), False), ((0,1), True),
                          ((-1,0), True),  ((0,0), True), ((1,0), True),
                                           ((0,-1),True), ((1,-1), False) ]

prop_movesImplTest1Defender = movesImplTest False testState1 ms
 where
   ms = [ CaptureMove (-1,1) [(-1,0),(0,0),(0,1)],
          CaptureMove (-1,1) [(0,1),(0,0),(-1,0)],
          CaptureMove (1,-1) [(0,-1),(0,0),(1,0)],
          CaptureMove (1,-1) [(1,0),(0,0),(0,-1)]
        ]

prop_movesImplTest1Attacker = movesImplTest True testState1 ms
  where
    ms = [ SimpleMove (0,0) (1,1),
           SimpleMove (0,1) (0,2),
           SimpleMove (1,0) (1,1) ]


testState2 = M.fromList [ ((-1,1), False), ((0,1), True),
                                            ((0,0), True),
                                            ((0,-1),True), ((1,-1), False) ]

prop_movesImplTest2Defender = movesImplTest False testState2 ms
 where
   ms = [ CaptureMove (-1,1) [(0,1),(0,0)],
          CaptureMove (1,-1) [(0,-1),(0,0)] ]


prop_movesImplTest2Attacker = movesImplTest True testState2 ms
  where
    ms = [ SimpleMove (0,0) (1,1),
           SimpleMove (0,1) (0,2) ]

movesImplTest p s ms = let sg = defaultGame in forAll (return (movesImpl sg p s)) $ \ms' -> sort ms' == sort ms

main :: IO ()
main = $(defaultMainGenerator)
