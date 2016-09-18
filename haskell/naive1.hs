module Main where

import Data.Text

-- Vector

data Vector = Vector Float Float Float deriving (Show)

addV :: Vector -> Vector -> Vector
addV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

subV :: Vector -> Vector -> Vector
subV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

mulV :: Vector -> Vector -> Vector
mulV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)

distV :: Vector -> Vector -> Float
distV v1 v2 = sqrt $ x**2 + y**2 + z**2
    where (Vector x y z) = v1 `subV` v2

-- Block

blockTypes :: Int
blockTypes = 256

type BlockId = Int

data Block = Block { bLocation :: Vector
                   , bName :: Text
                   , bDurability :: Int
                   , bType :: BlockId
                   , bTextureId :: Int
                   , bBreakable :: Bool
                   , bVisible :: Bool
                   } deriving (Show)

-- Enitity

data EntityType = Zombie
                | Chicken
                | Creeper
                | Enderman
                deriving (Show)

data Entity = Entity { eLocation :: Vector
                     , eType :: EntityType
                     , eName :: Text
                     , eHp :: Int
                     , eSpeed :: Vector
                     } deriving (Show)

entitySpeed :: EntityType -> Vector
entitySpeed e = case e of
                    Zombie -> Vector 0.5 0 0.5  -- slow, can't fly
                    Chicken -> Vector 0.75 0.25 0.75  -- can fly a bit
                    Creeper -> Vector 0.75 0 0.75
                    Enderman -> Vector 1 1 1  -- does what he wants

entityHp :: EntityType -> Int
entityHp e = case e of
                Zombie -> 50
                Chicken -> 25
                Creeper -> 75
                Enderman -> 500

entityMove :: Entity -> Entity
entityMove e = e { eLocation = eLocation e `addV` change }
    where
        change =
            let rngVector = Vector 1 1 1
            in rngVector `mulV` eSpeed e

mkEntity :: EntityType -> Vector -> Entity
mkEntity typ loc = Entity { eLocation = loc
                          , eType = typ
                          , eName = pack $ show typ
                          , eHp = entityHp typ
                          , eSpeed = entitySpeed typ
                          }

-- Chunk

chunkBlocks :: Int
chunkBlocks = 65536

chunkEntities :: Int
chunkEntities = 1000

-- TODO: consider Data.Vector / Data.Sequence
-- see https://gist.github.com/bartavelle/c1aaf8a47158132ee12caf42449f9066
data Chunk = Chunk { cLocation :: Vector
                   , cBlocks :: [BlockId]
                   , cEntities :: [Entity]
                   } deriving (Show)

mkChunk :: Vector -> Chunk
mkChunk v = Chunk { cLocation = v
                  , cBlocks = [1..chunkBlocks :: BlockId]
                  , cEntities = fmap genEntity [1..chunkEntities]
                  }
    where
        genEntity n =
            let i = fromIntegral n
            in case n `mod` 3 of
                0 -> mkEntity Chicken (Vector i i i)
                1 -> mkEntity Zombie (Vector i i i)
                2 -> mkEntity Creeper (Vector i i i)
                _ -> mkEntity Enderman (Vector i i i)

processEntities :: Functor f => f Entity -> f Entity
processEntities = fmap entityMove

-- World

-- TODO

-- main

main :: IO ()
main = putStr "temp"
