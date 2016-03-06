module LogisticRegression where
import List as L
import Debug as D

type alias Vector = List Float
type alias Weights = List Float

type Response = Zero | One

type alias Observation = {input : Vector, output : Response}

test =
  let
    obs = [ Observation [0,0] Zero
          , Observation [1,1] One
          , Observation [2, 2] One
          ]
  in
    gradientDescent obs

gradientDescent : List Observation -> Weights
gradientDescent obs =
  let
    padObservation obs = { obs | input = 1 :: obs.input }
    paddedObservations = L.map padObservation obs
    n = dimensionOfData obs + 1
    initialWeights = L.repeat n 0
    go iterations obs weights =
      let
        step = gradientDescentStep obs weights
        stepSize = step :*: step
        weights' = weights :+: step
      in
        if iterations >= maxIterations then
          weights'
        else if stepSize <= threshold ^ 2 then
          weights'
        else
          go (iterations + 1) obs weights'
  in
    go 0 paddedObservations initialWeights

maxIterations : Int
maxIterations = 10000

threshold : Float
threshold = 1

gradientDescentStep : List Observation -> Weights -> Weights
gradientDescentStep obs weights = learningRate .*: gradient obs weights

gradient : List Observation -> Weights -> Vector
gradient obs weights =
  let
    go {input, output} = (responseToFloat output - sigmoid input weights) .*: input
    zeroVector = L.repeat (dimensionOfData obs) 0
  in
    L.foldr (:+:) zeroVector <| L.map go obs

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

dotProduct  v1 v2 = sum

infixl 8 .*:
(.*:) : Float -> Vector -> Vector
(.*:) s v = L.map (\x -> x*s) v

infixl 7 :*:
(:*:) : Vector -> Vector -> Float
(:*:) v1 v2 = L.foldr (+) 0 <| L.map2 (*) v1 v2

infixl 6 :+:
(:+:) : Vector -> Vector -> Vector
(:+:) v1 v2 = L.map2 (+) v1 v2
