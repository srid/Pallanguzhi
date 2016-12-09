-- / Game round
module App.Round where

import App.Animation as Animation
import App.Board as Board
import App.Board (Board)
import App.Hand as Hand
import App.Turn as Turn
import App.View (class HasBoard, BoardViewConfig(..), getBoard, getBoardViewConfig, viewBoard)
import Control.Monad.Aff (later')
import Data.Either (Either(..))
import Data.List (length)
import Data.Maybe (Maybe(..))
import Prelude (bind, pure, show, unit, ($), (<>), (>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, text)

type HandA = Animation.State Turn.Turn' Hand.State

type Error = String

data State
  = Sowing HandA
  | Awaiting (Maybe Error) Board.Player Board 

data Action
  = AnimateTurn
  | PlayerSelect Board.PitRef

instance hasBoardRound :: HasBoard State where
  getBoard (Sowing handA) = 
    getBoard handA.current
  getBoard (Awaiting _ _ board) = 
    board

  getBoardViewConfig (Sowing handA) =
    getBoardViewConfig handA.current
  getBoardViewConfig (Awaiting _ player board) =
    BoardViewConfig
      { focusPit: Nothing
      , focusPlayer: Just player
      }

init :: Board.Player -> Board -> State
init player = Awaiting Nothing player 

initSow :: Board.Player -> Board.PitRef -> Board -> Either Error State
initSow player pitRef board = do 
  _ <- verifyPlayer 
  _ <- verifyPit
  pure $ initSow' player pitRef board
    where verifyPlayer = if Board.belongsTo pitRef player
                            then Right unit
                            else Left "Cannot play opponent's pit"
          verifyPit = if Board.lookup pitRef board > 0 
                        then Right unit
                        else Left "Cannot play from empty pit"


initSow' :: Board.Player -> Board.PitRef -> Board -> State
initSow' player pitRef board = Sowing $ Animation.init hand rest
  where hand = Hand.init player pitRef board
        rest = Turn.unfoldTurns hand

update :: forall eff. Action -> State -> EffModel State Action (eff)
update AnimateTurn (Sowing handA) =
  case Animation.step handA of
    Nothing ->
      -- Turn over; either await for opponent or (TODO) end turn
      noEffects $ awaitOpponent handA.current
    Just handA' ->
      -- Continue turn (animation)
      animateSow $ Sowing handA'
update (PlayerSelect pitRef) (Awaiting _ player board) =
  case initSow player pitRef board of 
    Right state -> 
      animateSow state 
    Left error -> 
      noEffects $ Awaiting (Just error) player board
update _ state =
  -- TODO: make this state transition impossible.
  noEffects state

awaitOpponent :: Hand.State -> State 
awaitOpponent hand = Awaiting Nothing (Hand.opponent hand) (getBoard hand)

animateSow :: forall eff. State -> EffModel State Action (eff)
animateSow state = { state: state
                   , effects: [ later' delay $ pure AnimateTurn ] }
                      where delay = animateDelay state

animateDelay :: State -> Int 
animateDelay (Sowing handA) = 
  case handA.lastTransition of 
    Just (Turn.Capture _) -> 1000 
    Just (Turn.Lift _) -> 500 
    Just (Turn.Sow _) -> 120 
    _ -> 80
animateDelay _ =
  100


view :: State -> Html Action
view state = 
  div [] 
    [ heading state
    , errorDiv state
    , viewBoard PlayerSelect state
    , hand state
    , lastTransition state
    ]
    where errorDiv (Awaiting (Just error) _ _) =
            div [] [ text $ "ERROR: " <> error ]
          errorDiv _ = 
            div [] [] 
          heading (Sowing handA) =
            div [] [ text $ "Sowing - " <> show (length handA.rest) <> " turns left"]
          heading (Awaiting _ player _) =
            div [] [ text $ "Awaiting turn by " <> show player ]
          hand (Sowing handA) =
            Hand.view handA.current
          hand (Awaiting _ _ _) =
            div [] [ text "No hand" ]
          lastTransition (Sowing handA) =
            case handA.lastTransition of 
              Just t -> div [] [ text $ "Last transition: " <> show t ]
              Nothing -> div [] [ text "No last transition" ]
          lastTransition _ =
            div [] [ text "" ]
