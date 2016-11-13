module Util.ModelE exposing (..)

import Html exposing (Html)
import Return
import Return exposing (Return)

type alias ModelE e model = Result (e, model) model
type alias ModelES model = ModelE String model

init : model -> ModelE e model
init = Ok

getModel : ModelE e model -> model
getModel r =
  case r of
    Ok model ->
      model
    Err (_, model) ->
      model

update : (msg -> model -> Result e (Return msg model))
      -> msg 
      -> ModelE e model 
      -> Return msg (ModelE e model)
update update msg modelE =
  let 
    model = getModel modelE
  in 
    case (update msg model) of
      Err e ->
        Return.singleton <| Err (e, model)
      Ok r ->
        Return.map Ok r

view : (model -> Maybe e -> Html msg) -> ModelE e model -> Html msg 
view view = asMaybe view

asMaybe : (model -> Maybe e -> a) -> ModelE e model -> a
asMaybe f v =
  case v of 
    Ok m ->
      f m Nothing
    Err (e, m) ->
      f m (Just e)