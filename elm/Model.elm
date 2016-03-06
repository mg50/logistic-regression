module Model where
import Dict as D

type Class = Blue | Green

type alias Location = (Float, Float)
type alias Point = { location: Location, class: Class }
type alias State = { points: D.Dict Int Point
                   , draggingId: Maybe Int
                   , freshId: Int
                   }

type Action = Mouseup Point
            | Mousedown Location
            | Mousemove Location

initialState : State
initialState = State D.empty Nothing 0
