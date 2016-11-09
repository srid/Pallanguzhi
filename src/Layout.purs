module App.Layout where

import App.Board as Board
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = Child (Board.Action)
  | PageView Route

type State =
  { route :: Route
  , board :: Board.State }

init :: State
init =
  { route: NotFound
  , board: Board.init }

update :: Action -> State -> State
update (PageView route) state =
  state { route = route }
update (Child action) state =
  state { board = Board.update action state.board }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text  "Pallanguzhi" ]
    , p [] [ text "Change src/Layout.purs and watch me hot-reload." ]
    , case state.route of
        Home ->
          map Child $ Board.view state.board
        NotFound ->
          NotFound.view state
    ]
