-- / Game round
module App.Round where

import App.TurnAnimation as TurnAnimation
import App.Board as Board
import App.Board (Board)
import App.Hand as Hand
import App.Turn as Turn
import App.Turn (Turn)
import App.BoardView as BoardView
import App.BoardView (class BoardView, getBoard, isPlaying, pitState)
import Data.Either (Either(..))
import Data.List (length)
import Data.Maybe (Maybe(Nothing, Just))
import Prelude (bind, pure, show, unit, (#), ($), (<>), (>), (>>>), (==))
import Pux (EffModel, noEffects, mapState, mapEffects)
import Pux.Html (Html, div, text)

type HandA = TurnAnimation.State Hand.State Turn
type Error = String

data State
  = Turning HandA
  | Awaiting (Maybe Error) Board.Player Board 

instance boardViewRound :: BoardView State where
  getBoard (Turning handA) = getBoard handA.current
  getBoard (Awaiting _ _ board) = board

  isPlaying (Turning handA) player = isPlaying handA.current player
  isPlaying (Awaiting _ player _) player' = player == player'

  pitState (Turning handA) ref = 
    go lastTurn nextTurn pitState'
      where lastTurn = handA.lastTurn
            nextTurn = TurnAnimation.nextTurn handA 
            pitState' = pitState handA.current ref
            go _ _ BoardView.Normal = BoardView.Normal 
            go Nothing _ ps = ps 
            go (Just Turn.Capture) _ _ = BoardView.Captured 
            go _ (Just Turn.Capture) _ = BoardView.Captured 
            go (Just Turn.Lift) _ _ = BoardView.Lifted 
            go _ (Just Turn.Lift) _ = BoardView.Lifted 
            go (Just Turn.Sow) _ _ = BoardView.Sowed
            go (Just Turn.Advance) _ _ = BoardView.Sowed
  pitState (Awaiting _ player _) ref = 
    if Board.belongsTo ref player 
      then BoardView.Sowed 
      else BoardView.Normal

data Action
  = TurnAnimationAction TurnAnimation.Action
  | PlayerSelect Board.PitRef

init :: Board.Player -> Board -> State
init player = Awaiting Nothing player 

update :: forall eff. Action -> State -> EffModel State Action (eff)
update (TurnAnimationAction action) (Turning state) = 
  case TurnAnimation.update action state of
    Nothing ->  -- Turn over
      awaitOpponent state.current 
      # noEffects
    Just handA' ->
      handA'
      # wrapTurnAnimation

update (PlayerSelect pitRef) (Awaiting _ player board) =
  case beginTurn player pitRef board of 
    Right handA -> 
      handA 
      # TurnAnimation.withAnimateEffect
      # wrapTurnAnimation
    Left error -> 
      Awaiting (Just error) player board
      # noEffects

update _ state =
  -- TODO: make this state transition impossible.
  noEffects state

wrapTurnAnimation :: forall eff. EffModel HandA TurnAnimation.Action (eff)
                  -> EffModel State Action (eff)
wrapTurnAnimation = mapState Turning >>> mapEffects TurnAnimationAction

beginTurn :: Board.Player -> Board.PitRef -> Board -> Either Error HandA
beginTurn player pitRef board = do 
  _ <- verifyPlayer 
  _ <- verifyPit
  pure $ beginTurn' player pitRef board
    where verifyPlayer = if Board.belongsTo pitRef player
                            then Right unit
                            else Left "Cannot play opponent's pit"
          verifyPit = if Board.lookup pitRef board > 0 
                        then Right unit
                        else Left "Cannot play from empty pit"

beginTurn' :: Board.Player -> Board.PitRef -> Board -> HandA
beginTurn' player pitRef board = TurnAnimation.init hand rest
  where hand = Hand.init player pitRef board
        rest = Turn.unfoldTurns hand

awaitOpponent :: Hand.State -> State 
awaitOpponent hand = Awaiting Nothing (Hand.opponent hand) (getBoard hand)

-- View

view :: State -> Html Action
view state = 
  div [] 
    [ heading state
    , BoardView.view PlayerSelect state
    , hand state
    , lastTurn state
    , errorDiv state
    ]
    where errorDiv (Awaiting (Just error) _ _) =
            div [] [ text $ "ERROR: " <> error ]
          errorDiv _ = 
            div [] [] 
          heading (Turning handA) =
            div [] [ text $ "Sowing - " <> show (length handA.remainingTurns) <> " turns left"]
          heading (Awaiting _ player _) =
            div [] [ text $ "Awaiting turn by " <> show player ]
          hand (Turning handA) =
            Hand.view handA.current
          hand (Awaiting _ _ _) =
            div [] [ text "No hand" ]
          lastTurn (Turning handA) =
            case handA.lastTurn of 
              Just t -> div [] [ text $ "Last turn: " <> show t ]
              Nothing -> div [] [ text "No last turn" ]
          lastTurn _ =
            div [] []
