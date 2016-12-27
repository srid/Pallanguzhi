module App.Config where

import Pux.Html as H
-- import Pux.Html.Attributes as A
-- import Pux.Html.Events as E
import Pux.Html (Html)
import App.BoardView (class BoardView, getTurn)
import App.Turn (Turn(..))
import Data.Maybe (Maybe(..))
import Prelude (($))

type Config =
  { fastTurn :: Boolean
  }

init :: Config
init = { fastTurn: true }

turnDelay :: forall state action. BoardView state action
          => Config -> state -> Int
turnDelay config state =
  if config.fastTurn
    then 0
    else go $ getTurn state
    where go (Just Capture) = 500
          go (Just Lift) = 500
          go (Just Sow) = 50
          go _ = 100

view :: forall action. Config -> Html action
view config =
  H.div [] []
    -- TODO: UI for config toggling
    -- [ H.text $ "Enable fast turn:"
    -- , H.option [A.checked config.fastTurn] []
    -- ]
