module Util.Diagram exposing (..)

import Svg
import Svg exposing (Svg)
import Svg.Attributes as S

type alias DrawF a = Int -> Int -> List (Svg a)

-- XXX: This way of invoking draw means we cannot resize diagrams. We could 
-- adapt the design but it might come at the cost of radius possibly being a float.
appendDrawF : DrawF a -> DrawF a -> DrawF a
appendDrawF f g x y =
  List.append (f x y) (g x y)

-- Diagram is a monoid, with empty and append.
-- It uses a bounding box (Int Int) which is not ideal (see moveX below)
-- but gets the job done.
type Diagram a = Diagram Int Int (DrawF a)

empty : Diagram a
empty =
  Diagram 0 0 <| (\_ _ -> [])

append : Diagram a -> Diagram a -> Diagram a
append = happend

draw : Int -> Int -> Diagram a -> List (Svg a)
draw x y (Diagram _ _ drawF) =
  drawF x y

width : Diagram a -> Int
width (Diagram w _ _) = w

height : Diagram a -> Int
height (Diagram _ h _) = h

text : List (Svg.Attribute a) -> String -> Diagram a
text attrs s = 
  -- XXX: text bounding box varies on font and text length!
  Diagram 1 1 (\x y ->
    Svg.text_ (attrs ++ 
               [ S.x <| toString x
               , S.y <| toString y]) [Svg.text s]
    |> singleton)

circle : List (Svg.Attribute a) -> Int -> Diagram a
circle attrs r =
  Diagram (r*2) (r*2) (\x y ->
    Svg.circle (attrs ++ 
                [ S.cx <| toString (x + r) 
                , S.cy <| toString (y + r)
                , S.r  <| toString r
                ]) []
    |> singleton)


rect : List (Svg.Attribute a) -> Int -> Int -> Diagram a
rect attrs w h =
  Diagram w h (\x y ->
    Svg.rect (attrs ++ 
              [ S.x <| toString x
              , S.y <| toString y
              , S.width <| toString w
              , S.height <| toString h
              ]) [] 
    |> singleton)

happend : Diagram a -> Diagram a -> Diagram a
happend (Diagram w1 h1 f1) (Diagram w2 h2 f2) = 
  Diagram (w1+w2) (max h1 h2) (\x y -> 
    List.append (f1 x y) (f2 (x + w1) y))

vappend : Diagram a -> Diagram a -> Diagram a
vappend (Diagram w1 h1 f1) (Diagram w2 h2 f2) = 
  Diagram (max w1 w2) (h1+h2) (\x y -> 
    List.append (f1 x y) (f2 x (y + h1)))

hspace : Int -> Diagram a
hspace w_ = empty |> \(Diagram w h f) -> Diagram (w+w_) h f

vspace : Int -> Diagram a
vspace h_ = empty |> \(Diagram w h f) -> Diagram w (h+h_) f

hfold : Int -> List (Diagram a) -> Diagram a
hfold space = List.foldl happend empty << List.intersperse (hspace space)
 
vfold : Int -> List (Diagram a) -> Diagram a
vfold space = List.foldl vappend empty << List.intersperse (vspace space)

stack : Diagram a -> Diagram a -> Diagram a
stack (Diagram w1 h1 f1) (Diagram w2 h2 f2) =
  Diagram (max w1 w2) (max h1 h2) <| appendDrawF f1 f2

-- XXX: This transformation should effect a change in the bounding boxes of 
-- the parent diagrams (created via append/fold), but it doesdn't. Use these
-- functions sparingly.
moveX : Int -> Diagram a -> Diagram a
moveX dx (Diagram w h f) =
  Diagram w h (\x y -> f (x+dx) y)

moveY : Int -> Diagram a -> Diagram a
moveY dy (Diagram w h f) =
  Diagram w h (\x y -> f x (y+dy))

move : Int -> Int -> Diagram a -> Diagram a
move dx dy (Diagram w h f) =
  Diagram w h (\x y -> f (x+dx) (y+dy))

singleton : a -> List a
singleton x = [x]

