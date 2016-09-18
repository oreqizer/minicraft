module Main where

-- Vector
-- ---

data Vector = Vector Float Float Float

addV :: Vector -> Vector -> Vector
addV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

subV :: Vector -> Vector -> Vector
subV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

mulV :: Vector -> Vector -> Vector
mulV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)

distV :: Vector -> Vector -> Float
distV v1 v2 = sqrt $ x^2 + y^2 + z^2
    where (Vector x y z) = v1 `subV` v2

-- Block
-- ---

blockTypes :: Int
blockTypes = 256

data Block = Block { bLocation :: Vector
                   , bName :: Text -- TODO
                   } deriving (Show)

-- main
-- ---

main :: IO ()
main = putStr "temp"
