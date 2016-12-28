module App.AI where

import App.Board as Board
import App.Board (Board, Player, PitRef)
import App.FixedMatrix72 (Ref(..), Row(..))

suggest :: Player -> Board -> PitRef
suggest player board = Ref { row: A, idx: 0 }
