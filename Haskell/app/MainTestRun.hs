module MainTestRun where
import Data.Map (Map)
import Game
import SiegeGame
import SiegeGameImpl
import GameStrategies
import SiegeGameAI

import System.Environment

import Control.Monad
import Text.Read


fromMove = const id
toBoard = const id

play :: SiegeGame -> (String,String) -> (Player, Map V Player) -> IO ()
play g cs@(defenderConf, attackerConf) (p,s) =
  let conf = if p then attackerConf else defenderConf
  in getNextMove conf (toBoard g s) p >>= (\m ->
    handleMove g cs p s (fmap (fromMove g) m) play
  )

handleMove g cs p s m rec = do
  putStrLn $ showGame g s
  case m of
    Nothing -> putStrLn "No available moves!"
    Just m -> do
      putStrLn $ "Player " ++ show p ++ " performs move " ++ show m
      rec g cs (not p, move g p s m)

main = do
  let g = defaultGame
  let s = startState g
  xs <- getArgs
  case xs of
    []      -> print desc
    [x]     -> play g (x,x) (False, s)
    (x:y:_) -> play g (x,y) (False, s)
  where
    desc = "Usage: stack exec testrun alg1 [alg2]\nExample: stack exec testrun \"(AlphaBeta, Score1, 5)\" \"(Minimax, Score1, 2)\""
