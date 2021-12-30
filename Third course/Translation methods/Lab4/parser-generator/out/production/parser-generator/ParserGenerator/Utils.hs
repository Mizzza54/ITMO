module ParserGenerator.Utils where

import qualified Data.Set as Set
import qualified Data.Map as Map

data Atom
  = Terminal String
  | NotTerminal String
  | Epsilon
  deriving (Show, Ord, Eq)
  
data Rule = Rule 
  { from :: Atom
  , to :: [Atom]
  } deriving (Show, Ord, Eq)


getFirst :: Map.Map Atom (Set.Set Atom) -> [Atom] -> Set.Set Atom
getFirst _ (Terminal x : _)           = Set.singleton $ Terminal x
getFirst _ [Epsilon]                  = Set.singleton Epsilon
getFirst acc (NotTerminal x : xs) = let alpha = (Map.!) acc (NotTerminal x) in
  if Set.member Epsilon alpha then
    alpha `Set.union` getFirst acc xs
  else
    alpha
getFirst _ (Epsilon : _)              = error "get first -> [Epsilon : xs] -> unexpected"
getFirst _ []                         = error "get first -> [] -> unexpected"


buildFirst :: Bool -> [Rule] -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom)
buildFirst flag rules acc = 
  if flag
    then let result = action rules acc in buildFirst (fst result) rules (snd result)
    else acc

action :: [Rule] -> Map.Map Atom (Set.Set Atom) -> (Bool, Map.Map Atom (Set.Set Atom))
action rules acc = let result = foldl func acc rules in (result /= acc, result)

func :: Map.Map Atom (Set.Set Atom) -> Rule -> Map.Map Atom (Set.Set Atom)
func acc Rule {from = from', to = to'} = Map.adjust (Set.union $ getFirst acc to') from' acc


atomE = NotTerminal "E"
atomT = NotTerminal "T"
atomF = NotTerminal "F"
atomOB = Terminal "("
atomCB = Terminal ")"
atomN = Terminal "n"
atomPlus = Terminal "+"
atomMul = Terminal "*"

atoms = [atomE, atomT, atomF, atomOB, atomCB, atomN, atomPlus, atomMul, Epsilon]

rule1 = Rule { from = atomE, to = [atomE, atomPlus, atomT]} 
rule2 = Rule { from = atomE, to = [atomT]} 
rule3 = Rule { from = atomT, to = [atomT, atomMul, atomF]} 
rule4 = Rule { from = atomT, to = [atomF]} 
rule5 = Rule { from = atomF, to = [atomOB, atomE, atomCB]} 
rule6 = Rule { from = atomF, to = [atomN]} 

rules = [rule1, rule2, rule3, rule4, rule5, rule6]

mappa ::  [Atom] -> Map.Map Atom (Set.Set a)
mappa list = foldl add Map.empty list where
  add m x = Map.insert x Set.empty m


test = buildFirst True rules (mappa atoms)


fromList [
(Terminal "(",fromList []),
(Terminal ")",fromList []),
(Terminal "*",fromList []),
(Terminal "+",fromList []),
(Terminal "n",fromList []),
(NotTerminal "E",fromList [Terminal "(",Terminal "n"]),
(NotTerminal "F",fromList [Terminal "(",Terminal "n"]),
(NotTerminal "T",fromList [Terminal "(",Terminal "n"]),
(Epsilon,fromList [])]
