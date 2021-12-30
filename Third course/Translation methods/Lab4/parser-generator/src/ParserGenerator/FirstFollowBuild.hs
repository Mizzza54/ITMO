module ParserGenerator.FirstFollowBuild
  ( Atom (..)
  , Rule (..)
  , getFirst
  , buildFirst
  , mappa
  , buildFollow
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import qualified Data.Map as Map
import qualified Data.Set as Set

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data Atom
  = Terminal String
  | NotTerminal String
  | Epsilon
  | End
  deriving (Show, Ord, Eq)

data Rule = Rule
  { from :: Atom
  , to   :: [Atom]
  } deriving (Show, Ord, Eq)

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

action :: [Rule] -> Map.Map Atom (Set.Set Atom) -> (Bool, Map.Map Atom (Set.Set Atom))
action rules acc = let result = foldl func acc rules in (result /= acc, result)

func :: Map.Map Atom (Set.Set Atom) -> Rule -> Map.Map Atom (Set.Set Atom)
func acc Rule {from = from', to = to'} = Map.adjust (Set.union $ getFirst acc to') from' acc

isNTerminal :: Atom -> Bool
isNTerminal (NotTerminal _) = True
isNTerminal _               = False

action2 :: [Rule] -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom) -> (Bool, Map.Map Atom (Set.Set Atom))
action2 rules first follow = let result = foldl (func2 first) follow rules in (result /= follow, result)

func2 :: Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom) -> Rule -> Map.Map Atom (Set.Set Atom)
func2 first follow Rule {from = from', to = to'} = func3 first from' follow to'

func3 :: Map.Map Atom (Set.Set Atom) -> Atom -> Map.Map Atom (Set.Set Atom) -> [Atom] -> Map.Map Atom (Set.Set Atom)
func3 _ _ follow [] = follow
func3 _ from' follow [NotTerminal x] = Map.adjust (Set.union ((Map.!) follow from')) (NotTerminal x) follow
func3 first from' follow (Terminal _ : xs) = func3 first from' follow xs
func3 first from' follow (NotTerminal x : xs) = if Set.member Epsilon $ getFirst first xs
  then
    let result = Map.adjust (Set.union ((Map.!) follow from' `Set.union` (Set.delete Epsilon $ getFirst first xs))) (NotTerminal x) follow in func3 first from' result xs
  else
    let result = Map.adjust (Set.union (getFirst first xs)) (NotTerminal x) follow in func3 first from' result xs
func3 _ _ follow (Epsilon : _) = follow

----------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------

getFirst :: Map.Map Atom (Set.Set Atom) -> [Atom] -> Set.Set Atom
getFirst _ (Terminal x : _)           = Set.singleton $ Terminal x
getFirst _ [Epsilon]                  = Set.singleton Epsilon
getFirst acc (NotTerminal x : xs) = let alpha = (Map.!) acc (NotTerminal x) in
  if Set.member Epsilon alpha then
    alpha `Set.union` getFirst acc xs
  else
    alpha
getFirst _ (Epsilon : _)              = error "get first -> [Epsilon : xs] -> unexpected"
getFirst _ []                         = Set.empty

buildFirst :: Bool -> [Rule] -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom)
buildFirst flag rules acc =
  if flag
    then let result = action rules acc in buildFirst (fst result) rules (snd result)
    else acc

mappa ::  [Atom] -> Map.Map Atom (Set.Set a)
mappa = foldl add Map.empty where
  add m x = Map.insert x Set.empty m

buildFollow :: Bool -> [Rule] -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom)
buildFollow flag rules first follow =
  if flag
    then let result = action2 rules first follow in buildFollow (fst result) rules first (snd result)
    else follow
