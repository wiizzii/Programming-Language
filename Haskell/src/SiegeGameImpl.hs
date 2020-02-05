{-# LANGUAGE TypeFamilies,FlexibleContexts #-}
module SiegeGameImpl where
import Game
import SiegeGame
import SiegeGameScore1

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List (tails, partition, inits)

data ScoreAlg = Score0 | Score1 deriving (Show,Read)
getGame :: ScoreAlg -> SiegeGame
getGame Score0 = defaultGame
getGame Score1 = case defaultGame of SiegeGame g _ -> SiegeGame g (score1 g)

---

-- Task 1.1) Bruger Data Map insert til at indsÃ¦tte korditatet v og playeren i Mappet
placeDefenders :: [V] -> Map V Player -> Map V Player 
placeDefenders [v] s = M.insert v False s
placeDefenders (v:v1:vs) s = M.insert v False (M.insert v1 False s) 


-- Task 1.2) Finder om det er en atacaker eller en defender (True/false) med M.! der finder value () med key v0, dette bruges nÃ¥r der insÃ¦ttes den nye key (position) og den gamle position (key) slettes

simpleMove     :: V -> V -> Map V Player -> Map V Player
simpleMove v0 v1 s = M.insert v1 (s M.! v0) (M.delete v0 s)



-- Task 1.3) Sletter defender og attacker og indsÃ¦tter defender pÃ¥ den nye plads, gennelÃ¸ber liste via recursion
captureMove   :: V -> [V] -> Map V Player -> Map V Player
captureMove (_) [] s = s
captureMove (x,y)((x1,y1):[]) s = M.insert (2*x1-x,2*y1-y) False (M.delete (x1,y1) (M.delete (x,y) s))
captureMove (x,y)((x1,y1):tail) s = captureMove (newX,newY) tail (M.delete (x1,y1) (M.delete (x,y) s))
    where (newX, newY) = (2*x1-x,2*y1-y)


-- Task 1.4)
moveImpl :: SiegeMove -> Map V Player -> Map V Player
moveImpl (PlaceDefenders vs) s = placeDefenders vs s
moveImpl (SimpleMove v0 v1) s = simpleMove v0 v1 s
moveImpl (CaptureMove v vs) s = captureMove v vs s


-- Task 1.5)
findPlayer :: [(V, VertexInfo)] -> [(V, Player)] -> [(V, Player)]
findPlayer [] list = list
findPlayer ((v,vi):(vs)) list = 
	if isJust(initialPiece(vi))
		then findPlayer vs (list ++ [(v, fromJust (initialPiece (vi)))])  --- sÃ¥ Ã¦ndres med fromJust til en tuple med en key og en value og de addes til listen
        else findPlayer vs list							--- hvis ikke den returnere en just skal de lede videre i listen


startStateImpl :: SiegeGame -> Map V Player
startStateImpl (SiegeGame g _) = M.fromList(findPlayer(M.toList g) []) 


-- Task 1.6)
findV :: [(V, VertexInfo)] -> [(V)] -> [(V)]
findV [] list = list
findV ((v,vi):(vs)) list = 
    if isJust(initialPiece(vi)) 	-- nÃ¥r isJust returnere nothing sÃ¥ skal den add til listen
    then findV vs list   			-- hvis isJust returnere en just skal den kigge videre i listen
    else findV vs (list ++ [(v)])	-- nÃ¥r isJust returnere nothing sÃ¥ skal den add til listen

toLists :: [V] -> [[V]]
toLists xs = [[x1, x2] | (x1:xs1) <- tails xs, x2 <- xs1]

--tests af toList giver: toLists [1,2,3] retur: [[1,2],[1,3],[2,3]]

placeDefenderMoves :: SiegeGame -> [SiegeMove]
placeDefenderMoves (SiegeGame g _) = [(PlaceDefenders[x,y]) | (x:xs) <-toLists((findV(M.toList g) [])), y <- xs] -- den tager den fÃ¸rste plads, parre den med alle andre pladser, og sÃ¥ tager den, den nÃ¦ste plads og gÃ¸r det samme. (findV(M.toList g) []) indeholder listen over alle de tomme pladser funder i findV


-- Task 1.7)

-- for at teste 1.7: simpleMoves defaultGame True (startStateImpl defaultGame)
-- resultat af test skal vÃ¦re: [SimpleMove (-2,0) (-1,1),SimpleMove (-2,1) (-1,1),SimpleMove (-1,0) (-1,1),SimpleMove (0,0) (-1,1),SimpleMove (0,0) (0,1),SimpleMove (0,0) (1,1),SimpleMove (1,0) (1,1),SimpleMove (2,0) (1,1),SimpleMove (2,1) (1,1)]
-- test: simpleMoves defaultGame False (M.fromList([((0,0),False)]))

getNeighbors :: VertexInfo  -> V -> [(V,NeighborInfo)]
getNeighbors g v = [(v,x) | x <- neighbors g]


--nabofeltet skal vÃ¦re tomt dvs, det mÃ¥ ikke vÃ¦re i listen af v,player
isEmpty :: [(V,V)]-> Map V Player -> [(V,V)] -> [(V,V)]
isEmpty list s [] = list
isEmpty list s ((v,v1):vs) = if M.notMember v1 s 
	then isEmpty (list ++ [(v,v1)]) s vs
	else isEmpty list s vs

getInfo :: [(V,VertexInfo)] -> Player -> [(V,Player)] -> [(V,NeighborInfo)]
getInfo v p v1 = concat(map (checkInf v1 p) v)

--Finder naboer pÃ¥ den specifikke koordinati map v player 
checkInf :: [(V,Player)] -> Player -> (V,VertexInfo) -> [(V,NeighborInfo)]
checkInf [] p (_) = []
checkInf ((v,player):vs) p (v1,vertexinfo)=
	if v == v1 && p == player
		then getNeighbors vertexinfo v
		else checkInf vs p (v1,vertexinfo)

findisAttackerAllowed :: [(V, NeighborInfo)] -> [(V,V)]
findisAttackerAllowed [] = []
findisAttackerAllowed ((v, neighborinfo):vs) = 
	if isAttackerAllowed(neighborinfo)
		then (v, getNeighbor(neighborinfo)) : findisAttackerAllowed vs
		else findisAttackerAllowed vs

findDef :: [(V, NeighborInfo)] -> [(V,V)]
findDef [] = []
findDef ((v, neighborinfo):vs) = (v, getNeighbor(neighborinfo)) : findDef vs
		
order :: [(V,V)] -> [SiegeMove]	
order [] = []
order (x:vs) = [SimpleMove v v1 | (v,v1) <-(x:vs)]


simpleMoves :: SiegeGame -> Player -> Map V Player -> [SiegeMove]
simpleMoves (SiegeGame g _) p s = if p
	then order(isEmpty [] s (findisAttackerAllowed(getInfo (M.toList g) p (M.toList s))))
	else order(isEmpty [] s               (findDef(getInfo (M.toList g) p (M.toList s))))


-- Task 1.8)
defenderCaptureMoves :: SiegeGame -> Map V Player -> [SiegeMove]
defenderCaptureMoves (SiegeGame g _) s = undefined

-- Task 1.9)

-- test opg 1.9: putStr (showGameImpl defaultGame (startStateImpl defaultGame))
-- test opg 1.9: putStr $ showGameImpl defaultGame (M.fromList [((0,0), True), ((-1,2), True), ((-1,3), False), ((1,0), False)])

showGameImpl :: SiegeGame -> Map V Player -> String
--showGameImpl (SiegeGame g _) m = concat(insertInList 7 "\n" (map (onBoard  g m) listF))
showGameImpl (SiegeGame g _) m = concat(insertInList (map (onBoard  g m) listF))


insertInList [] = []
insertInList v = take 7 v ++ "\n" : insertInList (drop 7 v)


{--
insertInList :: Int -> a -> [a] -> [a]
insertInList n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs 
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs
--}
listF :: [(V)]
listF = reverse[(x,y)| y <-[-3..3], x <-[-3..3]]


onBoard :: Map V VertexInfo -> Map V Player -> V -> String
onBoard g m (x,y) = 
	if M.notMember (x,y) g
		then " "
	else emptyNonEmpty (x,y) (M.toList m)

emptyNonEmpty :: V -> [(V,Player)] -> String		
emptyNonEmpty (x,y) [] = if isGoalField (x,y)
		then "*"
		else "."
emptyNonEmpty (x,y) (((z,a),player):vs)= if (x,y) == (z,a)
		then whichPlayer (x,y) player
		else emptyNonEmpty (x,y) vs
		
		
whichPlayer:: V -> Player -> String	
whichPlayer (x,y) player = if isGoalField (x,y)
		then if player == True
				then "A"
				else "D"
		else if player == True
				then "a"
				else "d"


isGoalField :: V -> Bool
isGoalField (x,y) = if ((x >= -1 && x <= 1) && (y >= 1))
	then True
	else False


---

defenderMoves :: SiegeGame -> Map V Player -> [SiegeMove]
defenderMoves sg s = case [v | (v, False) <- M.toList s] of
  [] -> placeDefenderMoves sg
  _  -> case defenderCaptureMoves sg s of
    [] -> simpleMoves sg False s
    xs -> xs

movesImpl :: SiegeGame -> Player -> Map V Player -> [SiegeMove]
movesImpl sg True s    = simpleMoves sg True s
movesImpl sg False s   = defenderMoves sg s

valueImpl :: SiegeGame -> Player -> Map V Player -> Double
valueImpl sg p m | null $ movesImpl sg p m = if p then -infinity else infinity
valueImpl (SiegeGame g _) _ m | and [ M.lookup v m == Just True | (v,vi) <- M.toList g, isGoal vi ] = infinity
valueImpl (SiegeGame g s) p m = s m


instance Game SiegeGame where
  type GameState SiegeGame  = Map V Player
  type Move SiegeGame       = SiegeMove

  startState    = startStateImpl
  showGame      = showGameImpl
  move _ _ s m  = moveImpl m s
  moves         = movesImpl
  value         = valueImpl
