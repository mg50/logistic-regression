module Main where
import Model exposing (..)
import Input exposing (actions)
import Display exposing (display)
import Update exposing (update)
import Mouse
import Window
import Signal exposing (..)
import Graphics.Element exposing (..)
import List as L

state : Signal State
state = Signal.foldp update initialState actions

main : Signal Element
main = Signal.map2 display Window.dimensions state
