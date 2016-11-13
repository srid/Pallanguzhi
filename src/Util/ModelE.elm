module Util.ModelE exposing (..)

import Html exposing (Html)

type alias ModelE e model = Result (e, model) model

getModel : ModelE e model -> model
getModel r =
  case r of
    Ok model ->
      model
    Err (_, model) ->
      model

update : (msg -> model -> Result e (model, Cmd msg)) -> msg -> ModelE e model -> (ModelE e model, Cmd msg)
update update msg modelE =
  let 
    model = 
      getModel modelE
    r = 
      update msg model
  in 
    case r of
      Err error ->
        (Err (error, model), Cmd.none)
      Ok (model, cmd) ->
        (Ok model, cmd)

view : (model -> Maybe e -> Html msg) -> ModelE e model -> Html msg 
view view = asMaybe view

asMaybe : (model -> Maybe e -> a) -> ModelE e model -> a
asMaybe f v =
  case v of 
    Ok m ->
      f m Nothing
    Err (e, m) ->
      f m (Just e)