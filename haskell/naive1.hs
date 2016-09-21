{-
General TODO:
- consider strict types, do research
- lists -> Vectors/Sequences
- String vs Text vs ByteString
- Linear package vs my impl
- LANGUAGE Generics for Control.DeepSeq
- just research this stuff at https://www.reddit.com/r/haskell/comments/52j2c9/performance_in_the_large_benchmark/
-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.Text
import Data.Time

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

data Block = Block { bLocation :: Vector  -- x, y, z within the chunk
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

data Entity = Entity { eLocation :: Vector  -- x, y, z within the chunk
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
            let rngVector = Vector 1 1 1  -- Complex movement AI
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
mkChunk v = Chunk { cLocation = v  -- x, y, z within the world
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

-- Game

gameChunks :: Int
gameChunks = 100

data Game = Game { gChunks :: [Chunk]
                 , gChunkCount :: Int
                 , gBlocks :: [Block]
                 , gPlayerLoc :: Vector
                 } deriving (Show)

loadWorld :: Game
loadWorld = Game { gChunks = fmap mkChunk vectorSeq
                 , gChunkCount = gameChunks
                 , gBlocks = fmap mkBlock [1..blockTypes]
                 , gPlayerLoc = Vector 0 0 0
                 }
    where
        vectorSeq = fmap (\i -> let f = fromIntegral i in Vector f f f) [1..gameChunks]
        mkBlock n = Block { bLocation = Vector 0 0 0
                          , bName = pack "Block"
                          , bDurability = 100
                          , bType = n :: BlockId
                          , bTextureId = n
                          , bBreakable = True
                          , bVisible = True
                          }

updateChunk :: Vector -> Chunk -> State Int Chunk
updateChunk pLoc c
    | cLocation c `distV` pLoc > 100 = fmap mkChunk newPos
    | otherwise = return (c { cEntities = processEntities $ cEntities c })
    where
        newPos :: State Int Vector
        newPos = do
            x <- get
            let x' = x + 1
            put x'
            let f = fromIntegral x
            return $ Vector f f f

loop :: Game -> Game
loop (Game chunks counter blocks playerLoc) = Game chunks' counter' blocks playerLoc'
    where
        playerLoc' = playerLoc `addV` Vector 0.1 0 0
        (chunks', counter') = runState (traverse (updateChunk playerLoc') chunks) counter

-- main

sleep16 :: NominalDiffTime -> IO ()
sleep16 t = do
    let wait = floor $ (0.016 - t) * 1000000
    threadDelay wait

gameLoop :: Int -> Game -> IO ()
gameLoop iter game = do
    now <- getCurrentTime
    let game' = loop game
    looped <- getCurrentTime
    let diff = diffUTCTime looped now
    sleep16 diff
    putStrLn $ show iter ++ ": " ++ (show $ diff)
    gameLoop (iter + 1) game'

main :: IO ()
main = do
    putStrLn "Loading World..."
    now <- getCurrentTime
    let game = loadWorld
    putStrLn $ show $ Prelude.length $ gChunks game
    loaded <- getCurrentTime
    putStrLn "FINISHED!"
    putStrLn $ "Load time: " ++ (show $ diffUTCTime loaded now)
    gameLoop 0 game
