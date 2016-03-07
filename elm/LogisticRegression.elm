module LogisticRegression where
import List as L
import Debug as D
import LinearAlgebra exposing (..)
import Numeric exposing (Matrix)

type alias Vector = List Float
type alias Weights = List Float

type Response = Zero | One

type alias Observation = {input : Vector, output : Response}

newtonRaphson : List Observation -> Weights
newtonRaphson obs =
  let
    padObservation obs = { obs | input = 1 :: obs.input }
    paddedObservations = L.map padObservation obs
    n = dimensionOfData obs + 1
    initialWeights = L.repeat n 0
    go iterations obs weights =
      case newtonStep obs weights of
        Nothing -> weights
        Just step ->
          let
            stepSize = step :*: step
            weights' = weights :-: step
          in
            if iterations >= maxIterations then
              weights'
            else if stepSize <= threshold ^ 2 then
              weights'
            else
              go (iterations + 1) obs weights'
  in
    go 0 paddedObservations initialWeights

newtonStep : List Observation -> Weights -> Maybe Weights
newtonStep obs weights =
  let
    grad = gradient obs weights
    hess = hessian obs weights
  in
    case inverse hess of
      Nothing -> Nothing
      Just inv -> Just (inv ::*: grad)


hessian : List Observation -> Weights -> Matrix
hessian observations weights =
  let
    m = toFloat (L.length observations)
    d = dimensionOfData observations
    zeroMatrix = L.repeat d (L.repeat d 0)

    toOuter {input} =
      let
        s = sigmoid weights input
        outer = transpose [input] ::*:: [input]
      in
        (s * (1 - s)) .*:: outer

    matrix = L.foldl (::+::) zeroMatrix (L.map toOuter observations)
  in
    D.log (toString matrix) <| matrix

-- gradientDescent : List Observation -> Weights
-- gradientDescent obs =
--   let
--     padObservation obs = { obs | input = 1 :: obs.input }
--     paddedObservations = L.map padObservation obs
--     n = dimensionOfData obs + 1
--     initialWeights = L.repeat n 0
--     go iterations obs weights =
--       let
--         step = gradientDescentStep obs weights
--         stepSize = step :*: step
--         weights' = weights :+: step
--       in
--         if iterations >= maxIterations then
--           weights'
--         else if stepSize <= threshold ^ 2 then
--           weights'
--         else
--           go (iterations + 1) obs weights'
--   in
--     go 0 paddedObservations initialWeights

maxIterations : Int
maxIterations = 1000

threshold : Float
threshold = 1

gradientDescentStep : List Observation -> Weights -> Weights
gradientDescentStep obs weights = learningRate .*: gradient obs weights

gradient : List Observation -> Weights -> Vector
gradient obs weights =
  let
    m = toFloat (L.length obs)
    go {input, output} = (responseToFloat output - sigmoid input weights) .*: input
    zeroVector = L.repeat (dimensionOfData obs) 0
  in
    (L.foldr (:+:) zeroVector <| L.map go obs)

sigmoid : Vector -> Weights -> Float
sigmoid v weights =
  let
    dotProduct = v :*: weights
  in
    1 / (1 + e  ^ -dotProduct)

learningRate : Float
learningRate = 1

responseToFloat : Response -> Float
responseToFloat r = case r of
                      Zero -> 0
                      One -> 1

sum = L.foldr (+) 0

dimensionOfData : List Observation -> Int
dimensionOfData obs =
  case obs of
    {input,output}::_ -> L.length input
    _ -> 0
