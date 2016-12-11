module App.Layout where

import App.Game as Game
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude ((<$>), ($), (#))
import Pux.Html (Html, div, h1, text)
import Pux (EffModel, noEffects, mapEffects, mapState)
import DOM (DOM)

data Action
  = GameAction (Game.Action)
  | PageView Route

type State =
  { route :: Route
  , game :: Game.State 
  }

init :: State
init =
  { route: NotFound
  , game: Game.init 
  }

update :: Action -> State -> EffModel State Action (dom :: DOM)
update (PageView route) state =
  noEffects $ state { route = route }
update (GameAction action) state =
  Game.update action state.game 
  # mapEffects GameAction 
  # mapState state { game = _ }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text  "Pallanguzhi" ]
    , case state.route of
        Home ->
          GameAction <$> Game.view state.game
        NotFound ->
          NotFound.view state
    ]
