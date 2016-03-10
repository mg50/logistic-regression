module Display (display) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Array as A
import Dict as D
import List as L
import LogisticRegression as LR
import Util as U
import Debug

import Model exposing (..)

display : (Int, Int) -> State -> Element
display (w,h) state =
  let points = D.values state.points
      w' = toFloat w
      h' = toFloat h
      styleAxis = traced (solid black)
      xAxis = styleAxis <| segment (-w'/2, 0) (w'/2, 0)
      yAxis = styleAxis <| segment (0, -h'/2) (0, h'/2)
      dots = L.map toDot points
      origin = circle 5 |> filled black |> move (0,0)
  in collage w h <| (xAxis::yAxis::origin::dots) ++ lrBoundary w' h' points

lrBoundary : Float -> Float -> List Point -> List Form
lrBoundary w h points =
  let
    toResponse class = case class of
                         Blue -> LR.Zero
                         Green -> LR.One
    toObservation {location, class} =
      LR.Observation (U.locToVect location) (toResponse class)
    observations = L.map toObservation points
    results = LR.newtonRaphson observations -- LR.gradientDescent observations
    _ = Debug.log (toString results ++ " " ++ (toString points)) 0
  in
    case results of
      Just (w0::w1::w2::[]) -> [drawLine w h w0 w1 w2]
      _ -> []

drawLine w h w0 w1 w2 =
  let
    y x = -(w0 + w1*x) / w2
  in
    traced (dashed black) <| segment (-w, y -w) (w, y w)


toDot : Point -> Form
toDot p =
  circle 5 |> filled (getColor p) |> move p.location

getColor : Point -> Color
getColor p = case p.class of
               Blue -> blue
               Green -> green
