-- / Game round
module App.Round where

import App.AI as AI
import App.Board as Board
import App.BoardView as BoardView
import App.Config as Config
import App.FixedMatrix72 as FM
import App.Hand as Hand
import App.Turn as Turn
import Data.List as List
import App.Board (Board, Player, PitRef, opponentOf)
import App.BoardView (class BoardView)
import App.Config (Config)
import App.Hand (Hand)
import App.Turn (Turn)
import Control.Monad.Aff (later')
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.List (List, length, uncons)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(..), fst)
import Prelude (bind, pure, show, unit, (#), ($), (<$>), (<<<), (<>), (==), (>))
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

update :: forall eff. Config -> Action -> State -> Either (Tuple Player Board) (PuxUpdate eff)
update config ContinueTurn (Turning hand board lastTurn nextTurns) =
  case uncons nextTurns of
    Nothing -> -- Turn over
      if allPitsEmptyFor (opponentOf hand.player) board
        then Tuple hand.player board -- Round over
             # Left
        else awaitOpponent hand board
             # noEffects
             # Right
    Just { head, tail } ->
      go head tail $ Turn.runTurn head (Tuple hand board)
        where go head tail (Tuple hand' board') =
                turning config hand' board' (Just head) tail
                # Right

update config (PlayerSelect pitRef) (Awaiting _ player board) =
  case beginTurn config player pitRef board of
    Right state ->
      state
      # Right
    Left error ->
      Awaiting (Just error) player board
      # noEffects
      # Right

update _ _ state =
  -- TODO: make this state transition impossible.
  noEffects state
  # Right

turning :: forall eff. Config -> Hand -> Board -> (Maybe Turn) -> (List Turn) -> PuxUpdate eff
turning config hand board turn rest =
  Turning hand board turn rest
  # withAnimateEffect
    where withAnimateEffect state =
            { state: state
            , effects: [ later' (Config.turnDelay config state) (pure ContinueTurn) ]
            }

beginTurn :: forall eff. Config -> Player -> PitRef -> Board -> Either Error (PuxUpdate eff)
beginTurn config player pitRef board = do
  _ <- verifyPlayer
  _ <- verifyPit
  pure $ beginTurn' config player pitRef board
    where verifyPlayer = if Board.belongsTo pitRef player
                            then Right unit
                            else Left "Cannot play opponent's pit"
          verifyPit = if Board.lookup pitRef board > 0
                        then Right unit
                        else Left "Cannot play from empty pit"

beginTurn' :: forall eff. Config -> Player -> PitRef -> Board -> PuxUpdate eff
beginTurn' config player pitRef board =
  turning config hand board Nothing rest
    where hand = Hand.init player pitRef
          rest = Turn.unfoldTurns $ Tuple hand board

allPitsEmptyFor :: Player -> Board -> Boolean
allPitsEmptyFor player = all ((==) 0) <<< FM.getRow player <<< _.cells

awaitOpponent :: Hand -> Board -> State
awaitOpponent hand = Awaiting Nothing opponent
  where opponent = opponentOf hand.player

-- View

view :: State -> Html Action
view state =
  div []
    [ heading state
    , BoardView.view state
    , errorDiv state
    , aiSuggestView state
    ]
    where errorDiv (Awaiting (Just error) _ _) =
            div [] [ text $ "ERROR: " <> error ]
          errorDiv _ =
            div [] []
          aiSuggestView (Awaiting _ player board) =
            div [] [ text $ "Suggested move: " <> show (fst <$> move) ]
              where move = AI.bestMove board player
          aiSuggestView _ =
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
