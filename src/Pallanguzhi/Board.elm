module Pallanguzhi.Board exposing (..)

import Array
import Array exposing (Array)
import Maybe
import Debug

type Player = A | B 
type alias Pit = { player : Player, seeds : Int}
type alias PitLocation = (Player, Int)

type alias Board =
  { pits : Array Pit
  , storeA : Int
  , storeB : Int
  }

pitsPerPlayer : number
pitsPerPlayer = 7

seedsPerPit : number
seedsPerPit = 6

init : Board
init = 
  let 
    s = 
      seedsPerPit
    row = 
      [s, s, s, s, s, s, s] |> Array.fromList
    makePit player seeds =
      {player = player, seeds = seeds}
    makeRow player = 
      Array.map (makePit player) row
  in
    { pits = Array.append (makeRow A) (makeRow B)
    , storeA = 0
    , storeB = 0
    }

-- Turn PitLocation into array index.
locToIdx : PitLocation -> Int
locToIdx (player, idx) = 
  casePlayer player idx (idx + pitsPerPlayer)

casePlayer : Player -> a -> a -> a
casePlayer player a b =
  case player of 
    A -> a
    B -> b

-- Return the pit row for this player
rowOf : Player -> Board -> List Pit
rowOf player board =
  casePlayer player 
    (board.pits |> Array.toList |> List.take pitsPerPlayer)
    (board.pits |> Array.toList |> List.drop pitsPerPlayer)

mapRowOf : Player
        -> (PitLocation -> Pit -> b)
        -> Board
        -> List b
mapRowOf player f board =
  rowOf player board
  |> List.indexedMap (f << ((,) player))

displayOrder : Player -> List a -> List a
displayOrder player = casePlayer player identity List.reverse

lookup : PitLocation -> Board -> Pit
lookup loc board = 
  case Array.get (locToIdx loc) board.pits of
    Just pit -> 
      pit
    Nothing  -> 
      -- Invalid index is only possible due to programmer error.
      Debug.crash <| "error: invalid index: " ++ (toString loc)

lookupSeeds : PitLocation -> Board -> Int
lookupSeeds loc = lookup loc >> .seeds

next : PitLocation -> PitLocation
next (player, idx) =
  if idx == pitsPerPlayer - 1 then
    (opponentOf player, 0)
  else
    (player, idx + 1)

update : PitLocation -> (Int -> Int) -> Board -> Board
update loc f board =
  let 
    pit = 
      lookup loc board
    pits = 
      Array.set (locToIdx loc) { pit | seeds = f pit.seeds } board.pits
  in
    { board | pits = pits }

inc : PitLocation -> Board -> Board
inc loc board =
  update loc (\s -> s + 1)  board

clear : PitLocation -> Board -> Board
clear loc board =
  update loc (always 0) board

store : Player -> Int -> Board -> Board
store player seeds board =
  casePlayer player 
    { board | storeA = board.storeA + seeds }
    { board | storeB = board.storeB + seeds }

storeFor : Player -> Board ->Int 
storeFor player = casePlayer player .storeA .storeB

opponentOf : Player -> Player
opponentOf player = casePlayer player B A