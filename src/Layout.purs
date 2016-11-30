module App.Layout where

import App.Game as Game
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude ((<$>))
import Pux.Html (Html, div, h1, text)

data Action
  = GameAction (Game.Action)
  | PageView Route

type State =
  { route :: Route
  , game :: Game.State }

init :: State
init =
  { route: NotFound
  , game: Game.init }

update :: Action -> State -> State
update (PageView route) state =
  state { route = route }
update (GameAction action) state =
  state { game = Game.update action state.game }

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
