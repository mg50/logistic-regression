module LinearAlgebra (..) where
import List as L
import Numeric exposing (Vector, Matrix)

transpose : Matrix -> Matrix
transpose = Numeric.transpose

inverse : Matrix -> Maybe Matrix
inverse = Numeric.inverse

infixl 8 .*::
(.*::) s m = L.map (\v -> s .*: v) m

infixl 7 ::*::
(::*::) = Numeric.mmDot

infixl 7 ::*:
(::*:) : Matrix -> Vector -> Vector
(::*:) m v = L.map (\v' -> v' :*: v) m

infixl 8 .*:
(.*:) : Float -> Vector -> Vector
(.*:) s v = L.map (\x -> x*s) v

infixl 7 :*:
(:*:) : Vector -> Vector -> Float
(:*:) v1 v2 = L.foldr (+) 0 <| L.map2 (*) v1 v2

infixl 6 :+:
(:+:) : Vector -> Vector -> Vector
(:+:) v1 v2 = L.map2 (+) v1 v2

infixl 6 :-:
(:-:) : Vector -> Vector -> Vector
(:-:) v1 v2 = L.map2 (-) v1 v2

infixl 6 ::+::
(::+::) : Matrix -> Matrix -> Matrix
(::+::) = Numeric.mmPlus
