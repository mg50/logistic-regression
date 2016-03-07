module Numeric where
import Native.Numeric

type alias Vector = List Float
type alias Matrix = List (List Float)

inverse : Matrix -> Maybe Matrix
inverse = Native.Numeric.inverse

mmDot : Matrix -> Matrix -> Matrix
mmDot = Native.Numeric.mmDot

mmPlus : Matrix -> Matrix -> Matrix
mmPlus = Native.Numeric.mmPlus

transpose : Matrix -> Matrix
transpose = Native.Numeric.transpose
