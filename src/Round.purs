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
import Prelude (bind, pure, show, unit, (#), ($), (<>), (>))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, text)

type Error = String

data State
  = Turning Hand Board (Maybe Turn) (List Turn)
  | Awaiting (Maybe Error) Player Board 

data Action 
  = PlayerSelect PitRef
  | ContinueTurn

type PuxUpdate eff = EffModel State Action (eff)

instance boardViewRound :: BoardView State Action where
  getBoard (Turning _ board _ _) = board
  getBoard (Awaiting _ _ board) = board

  getCurrentPlayer (Turning hand _ _ _) = Just hand.player 
  getCurrentPlayer (Awaiting _ player board) = Just player

  getHand (Turning hand _ _ _) = Just hand
  getHand _ = Nothing

  getTurn (Turning _ _ turn turns) = go turn (List.head turns)
    where go (Just Turn.Advance) Nothing = Just Turn.Advance 
          go (Just Turn.Advance) t = t 
          go Nothing t = t
          go t _ = t
  getTurn _ = Nothing

  getPitAction (Turning _ _ _ _) _ = Nothing 
  getPitAction (Awaiting _ _ _) ref = Just $ PlayerSelect ref

init :: Player -> Board -> State
init = Awaiting Nothing 

update :: forall eff. Action -> State -> Either Board (PuxUpdate eff)
update ContinueTurn (Turning hand board lastTurn nextTurns) = 
  case uncons nextTurns of 
    Nothing -> -- Turn over
      awaitOpponent hand board
      # noEffects
      # Right
    Just { head, tail } ->
      go head tail $ Turn.runTurn head (Tuple hand board)  
        where go head tail (Tuple hand' board') = 
                Turning hand' board' (Just head) tail 
                # withAnimateEffect (Just head) (List.head tail)
                # Right

update (PlayerSelect pitRef) (Awaiting _ player board) =
  case beginTurn player pitRef board of 
    Right state -> 
      state 
      # Right
    Left error -> 
      Awaiting (Just error) player board
      # noEffects
      # Right

update _ state =
  -- TODO: make this state transition impossible.
  noEffects state
  # Right

withAnimateEffect :: forall eff. Maybe Turn -> Maybe Turn -> State -> PuxUpdate eff
withAnimateEffect turn1 turn2 state = 
  { state: state 
  , effects: [ later' (Turn.turnDelay turn1 turn2) (pure ContinueTurn) ]
  }

beginTurn :: forall eff. Player -> PitRef -> Board -> Either Error (PuxUpdate eff)
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

beginTurn' :: forall eff. Player -> PitRef -> Board -> PuxUpdate eff
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
    , BoardView.view state
    , errorDiv state
    ]
    where errorDiv (Awaiting (Just error) _ _) =
            div [] [ text $ "ERROR: " <> error ]
          errorDiv _ = 
            div [] [] 
          heading (Turning hand board _ turns) =
            div [] [ text $ BoardView.viewPlayer hand.player 
                         <> " is sowing " 
                         <> "with " 
                         <> show hand.seeds 
                         <> " seeds in hand: "
                         <> show (length turns) <> " turns left"]
          heading (Awaiting _ player _) =
            div [] [ text $ "Awaiting turn by " <> BoardView.viewPlayer player ]
