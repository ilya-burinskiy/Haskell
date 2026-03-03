data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter cellWidth (Coord j i) =
    let jd = fromIntegral j
        id = fromIntegral i
     in Coord (jd + cellWidth / 2) (id + cellWidth / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell cellWidth (Coord x y) =
    let j = floor (x - cellWidth / 2)
        i = floor (y - cellWidth / 2)
     in Coord j i
