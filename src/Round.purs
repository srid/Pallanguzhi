-- / Game round
module App.Round where

import App.Animation as Animation
import App.Board as Board
import App.View as View
import App.Hand as Hand
import App.Turn as Turn
import App.View (class HasBoard, ViewConfig(..), getBoard, getBoardViewConfig)
import Data.Maybe (Maybe(..))
import Prelude (($), (<$>), pure)
import Pux (EffModel, noEffects)
import Pux.Html (Html)
import Control.Monad.Eff.Timer as T

type HandA = Animation.State Hand.State

data State
  = Sowing HandA
  | Awaiting Board.Player Board.State

data Action
  = AnimateTurn
  | PlayerSelect Board.PitRef

instance hasBoardRound :: HasBoard State where
  getBoard (Sowing handA) = 
    getBoard handA.current
  getBoard (Awaiting _ board) = 
    board

  getBoardViewConfig (Sowing handA) =
    getBoardViewConfig handA.current
  getBoardViewConfig (Awaiting player board) =
    ViewConfig
      { focusPit: Nothing
      , focusPlayer: Just player
      }

init :: Board.Player -> Board.State -> State
init player = Awaiting player

sow :: Board.Player -> Board.PitRef -> Board.State -> State
sow player pitRef board = Sowing $ Animation.init hand rest
  where hand = Hand.init player pitRef board
        rest = Turn.unfoldTurns hand

update :: forall eff. Action -> State -> EffModel State Action (eff)
update AnimateTurn (Sowing handA) =
  case Animation.step handA of
    Nothing -> -- End of turn.
      -- TODO: sould we end the round itself?
      let opponent = Hand.opponent handA.current
      in noEffects $ Awaiting opponent $ getBoard handA.current
    Just handA' ->
      { state: Sowing handA'
      , effects: [ do 
          pure $ AnimateTurn
        ]
      }
update (PlayerSelect pitRef) (Awaiting player board) =
  { state: sow player pitRef board
  , effects: [ do 
      pure $ AnimateTurn
    ]
  }
update _ state =
  -- TODO: make this state transition impossible.
  noEffects state

next :: HandA -> Maybe State
next handA = Sowing <$> Animation.step handA

view :: State -> Html Action
view state = View.viewBoard PlayerSelect state