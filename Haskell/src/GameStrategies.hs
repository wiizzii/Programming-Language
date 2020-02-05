{-# LANGUAGE FlexibleContexts                  #-}
module GameStrategies where
import Game
import Data.List (intercalate)

data Tree m v   = Tree v [(m,Tree m v)]
type GTree g    = Tree (Move g) (Player,GameState g)
type AlphaBeta = (Value,Value)
type Depth = Int

instance (Show m, Show v) => Show (Tree m v) where
  show (Tree x xs) = "Tree " ++ show x ++ " ["++ sub' ++"]"
    where
      sub = intercalate ",\n" (map show xs)
      sub' = if null sub
                then ""
                else "\n" ++ unlines (map ("  " ++ ) $ lines sub)

startTree :: Game g => g -> Player -> GTree g
startTree g p = tree g (p, startState g)

-- Task 2.1)
tree :: Game g => g -> (Player, GameState g) -> GTree g
tree g ps@(p,s) = Tree (p,s) [((m), tree g ((not p), (move g p s m))) | (m) <- moves g p s]

-- Task 2.2)
takeTree :: Depth -> Tree m v -> Tree m v
takeTree 0 (Tree v subtrees) = Tree v []
takeTree d (Tree v subtrees) = Tree v(map(hentTree d) subtrees)

hentTree d (m,subtree) = (m, takeTree (d-1) subtree)

-- Task 2.3)
minimax :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minimax g (Tree (p,s) []) = (Nothing, (value g p s))
minimax g (Tree (p,s) v) = if p == True
	                     then takeMax ([(Just (fst x), (snd (minimax g(snd x)))) | (x) <- v])
						 else takeMinimum ([(Just (fst x), (snd (minimax g(snd x)))) | (x) <- v])
						 
						 
takeMinimum::[(a, Value)] -> (a,Value)
takeMinimum (x:[]) = x
takeMinimum (x:y:xs) = if snd x < snd y
	             then takeMinimum (x:xs)
				 else takeMinimum (y:xs)
				 
takeMax::[(a,Value)] -> (a, Value)
takeMax (x:[]) = x
takeMax (x:y:xs) = if snd x >= snd y
	             then takeMax (x:xs)
				 else takeMax (y:xs)
				 

-- Task 2.4)
minimaxAlphaBeta :: Game g => g -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minimaxAlphaBeta = undefined
