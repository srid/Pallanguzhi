-- / Game round
module App.Round where

import App.Board as Board
import App.BoardView as BoardView
import App.Hand as Hand
import App.Turn as Turn
import Data.List as List
import App.Board (Board, Player, PitRef, opponentOf)
import App.BoardView (class BoardView)
import App.Hand (Hand)
import App.Turn (Turn)
import Control.Monad.Aff (later')
import Data.Either (Either(..))
import Data.List (List, length, uncons)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(..))
import Prelude (bind, pure, show, unit, (#), ($), (<>), (==), (>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, text)

type Error = String

data State
  = Turning Hand Board (Maybe Turn) (List Turn)
  | Awaiting (Maybe Error) Player Board 

instance boardViewRound :: BoardView State where
  getBoard (Turning _ board _ _) = board
  getBoard (Awaiting _ _ board) = board

  isPlaying (Turning hand _ _ _) player = player == hand.player 
  isPlaying (Awaiting _ player' _) player = player == player'

  pitState (Turning hand board lastTurn turns) ref = 
    if hand.pitRef == ref 
      then go lastTurn nextTurn 
      else BoardView.Normal
        where nextTurn = List.head turns
              go Nothing _ = BoardView.Normal
              go (Just Turn.Capture) _ = BoardView.Captured 
              go _ (Just Turn.Capture) = BoardView.Captured 
              go (Just Turn.Lift) _ = BoardView.Lifted 
              go _ (Just Turn.Lift) = BoardView.Lifted 
              go (Just Turn.Sow) _ = BoardView.Sowed
              go (Just Turn.Advance) _ = BoardView.Sowed
  pitState (Awaiting _ player _) ref = 
    if Board.belongsTo ref player 
      then BoardView.Sowed 
      else BoardView.Normal

data Action 
  = PlayerSelect PitRef
  | ContinueTurn

init :: Player -> Board -> State
init = Awaiting Nothing 

update :: forall eff. Action -> State -> EffModel State Action (eff)
update ContinueTurn (Turning hand board lastTurn nextTurns) = 
  case uncons nextTurns of 
    Nothing -> -- Turn over
      awaitOpponent hand board
      # noEffects
    Just { head, tail } ->
      go head tail $ Turn.runTurn head (Tuple hand board)  
        where go head tail (Tuple hand' board') = 
                Turning hand' board' (Just head) tail 
                # withAnimateEffect (Just head) (List.head tail)

update (PlayerSelect pitRef) (Awaiting _ player board) =
  case beginTurn player pitRef board of 
    Right state -> 
      state 
    Left error -> 
      Awaiting (Just error) player board
      # noEffects

update _ state =
  -- TODO: make this state transition impossible.
  noEffects state

withAnimateEffect :: forall eff state. Maybe Turn -> Maybe Turn -> state -> EffModel state Action (eff)
withAnimateEffect turn1 turn2 state = 
  { state: state 
  , effects: [ later' (Turn.turnDelay turn1 turn2) (pure ContinueTurn) ]
  }

beginTurn :: forall eff. Player -> PitRef -> Board -> Either Error (EffModel State Action (eff))
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

beginTurn' :: forall eff. Player -> PitRef -> Board -> EffModel State Action (eff)
beginTurn' player pitRef board = 
  Turning hand board Nothing rest
  # withAnimateEffect Nothing (List.head rest)
    where hand = Hand.init player pitRef 
          rest = Turn.unfoldTurns $ Tuple hand board

awaitOpponent :: Hand -> Board -> State 
awaitOpponent hand = Awaiting Nothing (opponentOf hand.player) 

-- View

view :: State -> Html Action
view state = 
  div [] 
    [ heading state
    , BoardView.view PlayerSelect state
    , viewHand state
    , viewLastTurn state
    , errorDiv state
    ]
    where errorDiv (Awaiting (Just error) _ _) =
            div [] [ text $ "ERROR: " <> error ]
          errorDiv _ = 
            div [] [] 
          heading (Turning hand board _ turns) =
            div [] [ text $ "Sowing - " <> show (length turns) <> " turns left"]
          heading (Awaiting _ player _) =
            div [] [ text $ "Awaiting turn by " <> show player ]
          viewHand (Turning h _ _ _) =
            div [] [ text $ "Hand by " <> show h.player  
                         <> " containing " <> show h.seeds
                         <> " seeds at " <> show h.pitRef ]
          viewHand (Awaiting _ _ _) =
            div [] [ text "No hand" ]
          viewLastTurn (Turning hand board lastTurn turns) =
            case lastTurn of 
              Just t -> div [] [ text $ "Last turn: " <> show t ]
              Nothing -> div [] [ text "No last turn" ]
          viewLastTurn _ =
            div [] []
