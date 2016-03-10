module LogisticRegression where
import List as L
import LinearAlgebra exposing (..)
import Numeric exposing (Matrix)

type alias Vector = List Float
type alias Weights = List Float

type Response = Zero | One

type alias Observation = {input : Vector, output : Response}

newtonRaphson : List Observation -> Maybe Weights
newtonRaphson obs =
  let
    padObservation obs = { obs | input = 1 :: obs.input }
    paddedObservations = L.map padObservation obs
    n = dimensionOfData obs + 1
    initialWeights = L.repeat n 0
    go iterations obs weights =
      case newtonStep obs weights of
        Nothing -> Just weights
        Just step ->
          let
            stepSize = step :*: step
            weights' = weights :-: step
          in
            if iterations > maxIterations then
              Nothing
            else if stepSize <= threshold ^ 2 then
              Just weights'
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
  if L.length observations == 0 then
    [[]]
  else
    let
      designMatrix = L.map .input observations
      diagMatrix = designMatrix |> L.map (sigmoid weights)
                                |> L.map (\a -> a * (1 - a))
                                |> diagonalize
    in
      (transpose designMatrix) ::*:: diagMatrix ::*:: designMatrix

maxIterations : Int
maxIterations = 1000

threshold : Float
threshold = 0.01

gradientDescentStep : List Observation -> Weights -> Weights
gradientDescentStep obs weights = learningRate .*: gradient obs weights

gradient : List Observation -> Weights -> Vector
gradient obs weights =
  let
    m = toFloat (L.length obs)
    go {input, output} = (responseToFloat output - sigmoid weights input) .*: input
    zeroVector = L.repeat (dimensionOfData obs) 0
  in
    (L.foldr (:+:) zeroVector <| L.map go obs)

sigmoid : Weights -> Vector -> Float
sigmoid weights v =
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
