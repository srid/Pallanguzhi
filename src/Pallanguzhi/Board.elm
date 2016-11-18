module Pallanguzhi.Board exposing (..)

import Array
import Array exposing (Array)
import Maybe
import Debug

type Player = A | B 
type alias PitLocation = Int
type alias Pit = { player : Player, seeds : Int}

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

-- Return the pit row for this player
rowOf : Player -> Board -> List Pit
rowOf player board =
  case player of
    A -> board.pits |> Array.toList |> List.take pitsPerPlayer
    B -> board.pits |> Array.toList |> List.drop pitsPerPlayer

mapRowOf : Player
        -> (PitLocation -> Pit -> b)
        -> Board
        -> List b
mapRowOf player f board =
  rowOf player board
  |> List.indexedMap f

displayOrder : Player -> List a -> List a
displayOrder player =
  case player of 
    A -> identity
    B -> List.reverse

lookup : PitLocation -> Board -> Pit
lookup loc board = 
  case Array.get loc board.pits of
    Just pit -> 
      pit
    Nothing  -> 
      -- Invalid index is only possible due to programmer error.
      Debug.crash <| "error: invalid index: " ++ (toString loc)

lookupSeeds : PitLocation -> Board -> Int
lookupSeeds loc = lookup loc >> .seeds

next : PitLocation -> PitLocation
next loc =
  let 
    total = 2 * pitsPerPlayer 
  in 
    (loc + 1) % total

update : PitLocation -> (Int -> Int) -> Board -> Board
update loc f board =
  let 
    pit = 
      lookup loc board
    pits = 
      Array.set loc { pit | seeds = f pit.seeds } board.pits
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
  case player of
    A -> { board | storeA = board.storeA + seeds }
    B -> { board | storeB = board.storeB + seeds }

storeFor : Player -> Board ->Int 
storeFor player board =
  case player of
    A -> board.storeA
    B -> board.storeB

opponentOf : Player -> Player
opponentOf player =
  case player of 
    A -> B
    B -> A

locFor : Player -> PitLocation -> PitLocation
locFor player loc =
  case player of
    A -> loc
    B -> pitsPerPlayer + loc

