{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.Trail
import           Diagrams.Located
import           Diagrams.TrailLike
import           Diagrams.TwoD.Vector
import           Control.Applicative

resistorShape :: Diagram B
resistorShape = fromOffsets $ concat $ replicate 3 [V2 1 1, V2 1 (-1)]

circleSemicircles :: Diagram B
circleSemicircles = atPoints points (repeat spot) # rotateBy (1/2)
  where vectorList = ZipList $ replicate 7 $ V2 0 5
        angleList = ZipList [x / 12 | x <- [0 .. 6]]
        finalVectors = getZipList $ rotateBy <$> angleList <*> vectorList 
        points = fmap (p2 . unr2) finalVectors
        spot :: Diagram B = circle 1 # fc blue

starExercise :: Diagram B
starExercise = mconcat $ zipWith scale mult vectors
  where vectors :: [Diagram B] = map fromOffsets [ [1 *^ e (r @@ rad)] | r <- [tau/32, 2 * tau/32 .. tau]]
        mult :: [Double] = cycle [1,2,3]

vTriangle :: V2 Double -> V2 Double -> Diagram B
vTriangle a b = strokeTrail $ closeTrail $ trailFromVertices pointlist
   where pointlist = origin : map (p2 . unr2) [a, b]

makeParallelogram :: V2 Double -> V2 Double -> Diagram B
makeParallelogram a b = fromOffsets [a] # lc red
                      <> fromOffsets [b] # lc blue
                      <> strokeLocTrail (fromOffsets [b] `at` (origin .+^ a)) # lc red # dashingG [0.06,0.03] 0
                      <> strokeLocTrail (fromOffsets [a] `at` (origin .+^ b)) # lc blue # dashingG [0.06,0.03] 0
                      <> fromOffsets [(a ^+^ b)] # lc violet

pointGrid :: Double -> Double -> Diagram B
pointGrid i r = atPoints pointlist circlelist
  where gridCentre = p2 (i/2, i/2)
        pointlist = map p2 [(x, y) | x <- [0 .. i], y <- [0 .. i]]
        circlelist = map (createCircles gridCentre r) pointlist
        createCircles :: P2 Double -> Double -> P2 Double -> Diagram B
        createCircles circleCentre cradius point = if distance circleCentre point <= cradius
                                                   then circle 0.5 # fc yellow
                                                   else circle 0.5 # fc violet

diagram :: Diagram B
-- diagram = pointGrid 30 15
diagram = makeParallelogram unitX (unitX # rotateBy (1/8))
-- diagram = pentagon 1
--   # explodeTrail  -- generate a list of diagrams
--   # zipWith lc [orange, green, yellow, red, blue]
--   # mconcat # centerXY # pad 1.1
main :: IO ()
main = mainWith diagram
