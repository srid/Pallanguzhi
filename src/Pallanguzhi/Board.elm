module Pallanguzhi.Board exposing (..)

import Array
import Array exposing (Array)
import Maybe
import Debug

type alias Board =
  { pits   : Array Pit
  , storeA : Int
  , storeB : Int
  }

type Player = A | B 
type alias Pit = { player : Player, seeds : Int}

pitsPerPlayer : number
pitsPerPlayer = 7

seedsPerPit : number
seedsPerPit = 12

initialBoard : Board
initialBoard = 
  let 
    makeRow player = Array.repeat pitsPerPlayer {player = player, seeds = seedsPerPit}
  in
    { pits = makeRow A `Array.append` makeRow B
    , storeA = 0
    , storeB = 0
    }

rows : Board -> (List Pit, List Pit)
rows board = 
  let 
    f g = Array.toList >> g pitsPerPlayer
  in 
    ( f List.take board.pits
    , f List.drop board.pits
    )
    
fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

lookup : Int -> Board -> Pit
lookup idx board = 
  Array.get idx board.pits
  |> fromJust

next : Int -> Int
next idx
  = let 
      total = 2 * pitsPerPlayer 
    in 
      (idx + 1) % total

updateSeeds : Int -> (Int -> Int) -> Board -> Board
updateSeeds idx f board =
  let 
    pit = lookup idx board
    pits = Array.set idx { pit | seeds = f pit.seeds } board.pits
  in
    { board | pits = pits }

inc : Int -> Board -> Board
inc idx board =
  updateSeeds idx (\s -> s + 1)  board

clear : Int -> Board -> Board
clear idx board =
  updateSeeds idx (always 0) board

store : Int -> Board -> Board
store seeds board =
  { board | storeA = board.storeA + seeds }

capture : Int -> Board -> Board
capture idx board = 
  let 
    c = lookup idx board |> .seeds
  in
    board |> clear idx |> store c 

validateF : (a -> Bool) -> a -> Maybe a
validateF f v = if f v then Just v else Nothing


sow : Int -> Board -> Board
sow idx board =
  let
    hand = lookup idx board |> .seeds
    spread hand idx board =
      let 
        lk idx = board |> lookup idx |> .seeds
      in
        case hand of
          0 -> -- Empty hand
            case (lk idx, lk (idx |> next)) of
              (0, 0) -> -- Both empty, end turn.
                board
              (0, c) -> -- Capture next and move on.
                board |> capture (idx |> next) |> sow (idx |> next |> next) 
              (_, _) -> -- Continue digging.
                board |> sow idx
          _ -> 
            board |> inc idx |> spread (hand-1) (idx |> next) 
  in
    board |> clear idx |> spread hand (idx |> next)