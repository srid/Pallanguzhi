module App.Config where

import Pux.Html as H
import App.BoardView (class BoardView, getTurn)
import App.Turn (Turn(..))
import Data.Maybe (Maybe(..))
import Prelude (show, ($), (<>))
import Pux.Html (Html)

type Config =
  { fastTurn :: Boolean
  , demo :: Boolean
  }

init :: Config
init = { fastTurn: false
       , demo: true
       }

turnDelay :: forall state action. BoardView state action
          => Config -> state -> Int
turnDelay config state =
  if config.fastTurn
    then 0
    else go $ getTurn state
    where go (Just (Capture _)) = 500
          go (Just Lift) = 500
          go (Just Sow) = 50
          go _ = 100

view :: forall action. Config -> Html action
view config =
  H.div []
    [ H.text $ "[Game Config] fastTurn="
            <> show config.fastTurn
            <> "; demo="
            <> show config.demo ]
    -- TODO: UI for config toggling
    -- [ H.text $ "Enable fast turn:"
    -- , H.option [A.checked config.fastTurn] []
    -- ]
