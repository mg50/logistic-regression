module Input (actions) where
import Model exposing (..)
import Mouse
import Window
import Keyboard
import Signal exposing (..)

actions : Signal Action
actions = mergeMany [mouseUp, mouseDown, mouseMove]

mouseUp = let down = filter not False Mouse.isDown
              class b = if b then Green else Blue
              makeAction loc shift = Mouseup <| Point loc (class shift)
          in Signal.map2 makeAction (sampleOn down currentLocation)
                                    (sampleOn down Keyboard.shift)

mouseDown = let down = filter identity True Mouse.isDown
            in Signal.map Mousedown (sampleOn down currentLocation)

mouseMove = Signal.map Mousemove currentLocation

currentLocation : Signal Location
currentLocation = Signal.map2 normalize dimensions' position'

dimensions' : Signal (Float, Float)
dimensions' = Signal.map toFloat2 Window.dimensions

position' : Signal (Float, Float)
position' = Signal.map toFloat2 Mouse.position

toFloat2 : (Int, Int) -> (Float, Float)
toFloat2 (x,y) = (toFloat x, toFloat y)

normalize : Location -> Location -> Location
normalize (w,h) (x,y) = let x' = x - w / 2
                            y' = h / 2 - y
                        in (x', y')
