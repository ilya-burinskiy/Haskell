import Data.Function

multSecond = g `on` h

g a b = a * b

h (a, b) = b
