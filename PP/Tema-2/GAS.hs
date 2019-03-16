{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M
import Data.Maybe

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

	

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"	

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Patrat Color Heading |Sageata Heading | Cerc Color
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}

--Am incercat sa desfinesc in show dar orice as fi facut imi dadea eroare de parse
--la al treilea constructor , am petrecut minim o ora incercand sa-mi dau seama
--ce este gresit ( nu am gasit) , asa ca am folosit aceasta var ajutatoare ( greseam ceva la sintaxa)
showp (Patrat c h) 
			|c == Red =  "R" ++ (show h)
			|c == Blue = "B" ++ (show h)
			|otherwise = "G" ++ (show h)
showp (Sageata h)     = show h
showp (Cerc c) 
			|c == Red = "r"
			|c == Blue = "b"
			|otherwise = "g"
instance Show Object where
    show = showp
	

	
	

	
{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = EmptyLevel | Level (M.Map Position [Object])
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}

--Aceasta nu are legatura cu tema , mi-am deifinit o eu pentru a intelege mai bine cum functioneaza M.map , si a ma obisnui cu sintaxa 
myMap =(M.insert (toInteger  3,toInteger  2)[(Sageata North)]
	(M.insert (toInteger  0 , toInteger 3) [(Cerc Gray)] (M.insert  (toInteger 0,toInteger 2) [(Cerc Red)] (M.insert (toInteger  3,toInteger 1) [(Patrat Gray East)] 
	(M.insert (toInteger 3 ,toInteger 0) [(Sageata East)] (M.insert (toInteger 0,toInteger 0) [(Patrat Red South)]   M.empty))))))

--Pentru afisare aflu mai intai extremitatile coloana minima , maxima , linia minima maxima , apoi practic imi generez toate coordonatele care vor intra in harta dreptunghiulara , 
--dupa care parcurg in ordine tuplurile care reprezinta coordonatele  si in functie de fiecare element il afisez corespunzator , de asemenea am doua functii ajutatoare verLung
--care verifica lungimea , astfel imi voi da seama cate elemente sunt in acea casuta , iar verifLin verifica daca ce este de afisate , este la sfarsit unde trebuie adauga line terminatorul
--daca este la mijloc unde trebuie adaugat |
afisare EmptyLevel = ""
afisare (Level lst) = let
	minX = M.foldrWithKey (\k a b -> if(fst k) < b then (fst k) else b) 10000 lst
	maxX = M.foldrWithKey (\k a b -> if(fst k) > b then (fst k) else b) 0 lst
	minY = M.foldrWithKey (\k a b -> if(snd k) < b then (snd  k) else b) 10000 lst
	maxY = M.foldrWithKey (\k a b -> if(snd k) > b then (snd k) else b) 0 lst
	verLung text 
		|(length text == 3) = text
		|(length text == 2) = text ++ " "
		|otherwise = "  " ++ text
	verifLin (x,y) text 	
		|(y == maxY) && (x == maxX) = (verLung  text)
		|(y == maxY) && (x /= maxX)  = (verLung text) ++ "\n"
		|otherwise = (verLung text) ++ "|"
	showCoord (x,y) 
		| M.lookup (x,y) lst == Nothing = verifLin (x,y) "   " 
		| otherwise = verifLin (x,y) (concatMap show(fromJust (M.lookup (x,y) lst))) 
	coordonate = [(a,b) | a <- [minX .. maxX] , b <- [minY .. maxY] ]
	in foldr  (\k b -> (showCoord k) ++ b ) "" coordonate

showL EmptyLevel = ""
showL (Level lst) = "C"
instance Show Level where
    show = afisare
	--show (Level lst) = "C"

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = EmptyLevel

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare c h p (EmptyLevel) = Level $ M.insert p [(Patrat c h )] M.empty
addSquare col h pos (Level lv) = Level $ M.insertWith (++) pos[(Patrat col h)] lv

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle c p (EmptyLevel) = Level $ M.insert p [(Cerc c)] M.empty
addCircle c p (Level lv) = Level $ M.insertWith (++) p [(Cerc c)] lv

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow h p (EmptyLevel) = Level $ M.insert p [(Sageata h)] M.empty
addArrow h p (Level lv) = Level $ M.insert p [(Sageata h)] lv

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
--Aceasta functie poate fi implementata folosind recursivitate , conform cerintei 

nextMove North (x,y) = ((x-1) , y)
nextMove South (x,y) = ((x+1) , y)
nextMove East (x,y)  = (x , (y+1))
nextMove West (x,y)  = (x , (y-1))

			
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
	 
verificaPatrat (Patrat col dir) = True
verificaPatrat _ = False 

verificaCerc (Cerc col) = True
verificaCerc _ = False

verificaSageata (Sageata dir) = True
verificaSageata _ = False

getColorPatrat (Patrat col dir) = col
getCOlorCerc (Cerc col) = col

getDirection (Patrat col dir) = dir --Functie care primeste ca param un patrat si arata orientarea sagetii de pe el 
getDirection (Sageata dir) = dir
	

--Aceasta functie se folosete de functia movAux care este o functie recursiva , si care muta toate patratele aflate in linie in directia specificata 
move pos (Level lv) = let	
	valoriLst = M.lookup pos lv --Are just sau este nothing in cazul in care cheia nu exista in map
	lst = fromJust valoriLst   --Lista cu valorile care pot fi (PAtrat - cert , patrat sageata , patrat , sageata , cerc)
	
	patrat  = head lst  -- Aceasta este apelata doar daca se intra pe ramura in care sunt consins ca acela este patrat 
	aldoilea = head (tail lst) -- Si acesta la fel 
	
	--Directie atunci cand sunt patrate in lant 
	directie  = getDirection modificaSageata
	
	
	--Functie care hotaraste in ce directie este urmatoarea mutare conform sagetii patratului
	verificaParametriiDir = nextMove (getDirection patrat) pos  
												
	--Functie care modifica sageata de pe patrat (in conformitate cu sageata casutei pe care o sa fie asezata )
	modificaSageata = Patrat (getColorPatrat patrat) (sageataNext (getDirection patrat) verificaParametriiDir  lv )
										
											
						
	--Aceasta este un apel pt a verifica daca sunt patrate in lant ce vor suferi 
	apelMovAux = moveAux verificaParametriiDir directie lv 					
	
	functie  = if (valoriLst == Nothing) then (Level lv) else if (not (verificaPatrat (head lst))) then (Level lv)
																else Level $ M.insertWith (++) verificaParametriiDir [(modificaSageata)] $ if( length lst > 1) then M.adjust (tail) pos apelMovAux else M.delete pos apelMovAux
	
	in functie 
	
--Modifica sageata unui patrat in functie de pozitia urmatoare pe care urmeaza sa fie mutat , astfel incat sa nu fie probleme la afisare , cu alte cuvine inainte sa o mut pe o patratica ii si schimb directia sagetii
sageataNext directie pos lv = let	
		valoriLst = M.lookup pos lv --Are just sau este nothing in cazul in care cheia nu exista in map
		lst = fromJust valoriLst   --Lista cu valorile care pot fi (PAtrat - cert , patrat sageata , patrat , sageata , cerc)
		
		patrat  = head lst  -- Aceasta este apelata doar daca se intra pe ramura in care sunt consins ca acela este patrat 
		aldoilea = head (tail lst) -- Si acesta la fel 
		
		functie = if(valoriLst == Nothing) then directie else if( length lst == 1) then if(verificaSageata (head lst)) then getDirection (head lst)  else directie 
																					else if(verificaSageata (head (tail lst))) then getDirection (head (tail lst)) else directie 
																							
		in functie 
	
moveAux pos directie  lv = let
		valoriLst = M.lookup pos lv --Are just sau este nothing in cazul in care cheia nu exista in map
		lst = fromJust valoriLst   --Lista cu valorile care pot fi (PAtrat - cert , patrat sageata , patrat , sageata , cerc)
		
		patrat  = head lst  -- Aceasta este apelata doar daca se intra pe ramura in care sunt consins ca acela este patrat 
		aldoilea = head (tail lst) -- Si acesta la fel 
		
		--Cand patratul este mutat pe pozitia corespunzatoare va avea deja aceeasi orientarea ca sageata ( asta face aceasta functie)
		modificaSageata = Patrat (getColorPatrat patrat) (sageataNext (getDirection patrat) newMove  lv )
							
		newMove  = nextMove directie pos
		functie = if(valoriLst == Nothing) then lv else if(not (verificaPatrat (head lst))) then lv 
														else  M.insertWith (++) newMove [(modificaSageata)] $  if( length lst > 1) then M.adjust (tail) pos (moveAux newMove directie lv) else M.delete pos (moveAux newMove directie lv)
		in functie
		
																
	
unlev (Level lv) = lv


{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    
    successors (Level lv) = let	
		--functie care scoate toate pozitiile patratelor din mapa curenta 
		pozPatrate =  map fst $ M.toList $ M.filter (verificaPatrat . head ) lv 
		
		myFunc poz = (poz , (move poz (Level lv)))
		
		finalFunc  = map myFunc pozPatrate 
		in finalFunc

		

    isGoal (Level lv) = let
		--functie care scoate toate patratele 
		pozPatrate =  M.toList $ M.filter (verificaPatrat . head ) lv 
		
		aldoilea  = head . tail 
		verificaUnPAtrat (poz, lstOb) = if(length lstOb == 1) then False else
											if (verificaCerc (aldoilea lstOb)) then if(getCOlorCerc (aldoilea lstOb) == getColorPatrat (head lstOb)) then True else False
																			else False	
		finalFunc = foldr (\ x acc -> (verificaUnPAtrat x ) && acc) True pozPatrate
		in finalFunc


    -- Doar petru BONUS -- Aceasta euristica calculeaza distantele "optime" de la un patrat la cercul curespunzator , si returneaza suma tututror acestor distante 
    heuristic (Level lv) = let
	--functie care scoate toate patratele 
		pozPatrate =  M.toList $ M.filter (verificaPatrat . head ) lv 
		--iau toate cercurile 
		pozCercuri = M.toList $ M.filter  (\x -> if(length x == 1) then (verificaCerc (head x)) else (verificaCerc (aldoilea x))) lv 
		--primeste un cerc si returneaza culoarea
		getColCercF x = if(length x == 1) then (getCOlorCerc (head x)) else (getCOlorCerc (aldoilea x))
		
		
		aldoilea  = head . tail 
		
		calcDist coord1 coord2  = (abs (fst (coord1) - fst(coord2))) + (abs ( snd (coord1) - snd (coord2)))
		--fct care calc distanta de la un patrat la cercul corespunzator 
		calcDistanta (coordP, culP) = foldr (\(coord,cerc) acc -> if(getColCercF cerc == culP)then (calcDist coordP coord) else acc) 0 pozCercuri 	
		finalFunc = foldr (\(poz ,cull) acc -> acc + (calcDistanta (poz, getColorPatrat (head cull)))) 0 pozPatrate
		in finalFunc

