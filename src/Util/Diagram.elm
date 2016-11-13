module Util.Diagram exposing (..)

import Svg
import Svg exposing (Svg)
import Svg.Attributes exposing (..)

-- Diagram is a monoid, with empty and append.
type Diagram a = Diagram Int Int (Int -> Int -> List (Svg a))

empty : Diagram a
empty =
  Diagram 0 0 <| (\_ _ -> [])

append : Diagram a -> Diagram a -> Diagram a
append = happend

draw : Int -> Int -> Diagram a -> List (Svg a)
draw x y (Diagram _ _ draw') =
  draw' x y

singleton : a -> List a
singleton x = [x]

circle : List (Svg.Attribute a) -> Int -> Diagram a
circle attrs r' =
  let
    draw r' x y = 
      Svg.circle (attrs ++ [ cx <| toString (x + r') 
                 , cy <| toString (y + r')
                 , r  <| toString r'
                 ]) []
      |> singleton
 in
    -- XXX: This way of invoking draw means we cannot resize diagrams. We could 
    -- adapt the design but it might come at the cost of radius possibly being a float.
    Diagram (r'*2) (r'*2) (draw r')

rect : Int -> Int -> Diagram a
rect w h =
  let 
    draw w h x' y' =
      Svg.rect [ x <| toString x'
               , y <| toString y'
               , width <| toString w
               , height <| toString h
               ] [] 
      |> singleton
  in 
    Diagram w h (draw w h)

happend : Diagram a -> Diagram a -> Diagram a
happend (Diagram w1 h1 f1) (Diagram w2 h2 f2) = 
  let 
    draw x y = 
      f1 x y `List.append` f2 (x + w1) y
  in 
    Diagram (w1+w2) (Basics.max h1 h2) draw

vappend : Diagram a -> Diagram a -> Diagram a
vappend (Diagram w1 h1 f1) (Diagram w2 h2 f2) = 
  let 
    draw x y = 
      f1 x y `List.append` f2 x (y + h1)
  in 
    Diagram (Basics.max w1 w2) (h1+h2) draw

happend' : Int -> Diagram a -> Diagram a -> Diagram a
happend' space = flip <| (flip happend) << moveX space

vappend' : Int -> Diagram a -> Diagram a -> Diagram a
vappend' space = flip <| (flip vappend) << moveY space

hfold : Int -> List (Diagram a) -> Diagram a
hfold space = List.foldl (happend' space) empty 
  
vfold : Int -> List (Diagram a) -> Diagram a
vfold space = List.foldl (vappend' space) empty 

moveX : Int -> Diagram a -> Diagram a
moveX dx (Diagram w h f) =
  Diagram w h (\x y -> f (x+dx) y)

moveY : Int -> Diagram a -> Diagram a
moveY dy (Diagram w h f) =
  Diagram w h (\x y -> f x (y+dy))