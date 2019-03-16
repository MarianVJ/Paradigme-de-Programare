{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState
import Debug.Trace
import qualified Data.Set as S
import Data.List
import Data.Maybe
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = Node s a (Node s a) Int | NodeInit s Int
    deriving (Eq, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (Node s a n l) = s
nodeState (NodeInit s l) = s

nodParent (Node s a n l) = n
nodeAction (Node s a n l) = a

nodeLung (Node s a n l ) = l
nodeLung (NodeInit s l ) = l
{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}
limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs s bol lung  = let
	--Nodul initial care nu are parinte  ( va fi parintele indifrect al tuturo)
	initialNode = NodeInit s 0
    
	funcSort a b  
		| (heuristic (snd a)  ) >= (heuristic (snd b) ) = GT
		| otherwise = LT
	
	finalFunc  []  visited afisare =reverse  afisare  
	finalFunc (x : xs)  visited afisare 
		| S.member (nodeState x) visited = finalFunc xs visited afisare
		| (nodeLung x  == (lung+1) ) = finalFunc xs visited afisare 
		| otherwise =
			let additions = [ (Node (snd  v) (fst v) x ((nodeLung x) + 1)) | v <- (if bol == False then (successors (nodeState x))else (sortBy funcSort (successors (nodeState x)) ))]
			in finalFunc (additions ++ xs ) (S.insert (nodeState x) visited) (x : afisare)
		
	in (finalFunc [initialNode] (S.empty) []) 	
	

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}
--Aceasta functie seamana cu dfs , dar practic este un bfs , si de fiecare data cand schimb nivelul voi adauga la nr , numarul de elemente de pe nivelul curent
--exceptie face ultimul nivel pe care se va afla si solutia pentru care voi adauga doar lungimea - 1 ( pentru ca una este chiar solutia ) , 
-- si in acumulatorul nr adaug de fiecare data cand ma duc cu un nivel mai jos , si de asemenea actualizez si old 
iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening s bol = let 
	--Nodul initial care nu are parinte  ( va fi parintele indifrect al tuturo)
	initialNode = NodeInit s 0
    
	funcSort a b  
		| (heuristic (snd a)  ) >= (heuristic (snd b) ) = GT
		| otherwise = LT
	 
	finalFunc (x : xs)  visited afisare old nr  
		| (isGoal (nodeState x) == True  ) = (x, (nr + (S.size visited)))
		| S.member (nodeState x) visited = finalFunc xs visited afisare old nr 
		| otherwise =
			let additions = [ (Node (snd  v) (fst v) x ((nodeLung x) + 1)) | v <- (if bol == False then (successors (nodeState x))else (sortBy funcSort (successors (nodeState x)) ))]
			in  (finalFunc (xs ++ additions ) (S.insert (nodeState x) visited)  (x : afisare) (if (nodeLung x) > old then (nodeLung x ) else old )  (if (nodeLung x) == old then nr else (nr + (S.size visited))) )
		
	in (finalFunc [initialNode] (S.empty) [] 0 0)  	
	




---Aceasta este o varianta iterativa a iterativeDeeping , dar pentru care nu am gasit o euristica potrivita 	
funcc (s,bol,x,v,nodI ) = let	
		sol = limitedDfs s bol x 
		solution = foldr  (\ x acc -> if (isGoal  (nodeState x) ) then (False , x)  else acc ) (True,nodI)  sol 
		
		varbul = if ((fst solution) == False  ) then False else True 
		funcFinal = if v == False then Nothing else Just  (((length sol) ,(snd solution)), (s , bol, (x+1), varbul , (snd solution)))
		in funcFinal 
iterativeDeepening2 s bol = let
		rez = unfoldr funcc  (s,bol,1,True,(NodeInit s 0))
		nr = (foldr ((+) . (fst)) 0 rez ) - 1
		sol = limitedDfs s bol (length rez)
		solution = foldr (\x acc -> if( isGoal (nodeState x)) then x else acc ) (NodeInit s 0) sol 
		in ( (((snd . last ) rez)), nr)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
verificaNod (NodeInit s n) = True
verificaNod _ = False



extractPath :: Node s a -> [(a, s)]
extractPath nod = reverse ( unfoldr (\ (ac,st,n ) -> if (verificaNod n) then Nothing else Just (((nodeAction n),(nodeState n)),(ac,st,(nodParent n ) ))) ((nodeAction nod), (nodeState nod), nod))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))