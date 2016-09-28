{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

{-
General TODO:
- consider strict types, do research
- Text vs ByteString
- Linear package vs my impl
- just research this stuff at https://www.reddit.com/r/haskell/comments/52j2c9/performance_in_the_large_benchmark/
-}

module Main where

import Control.Concurrent (threadDelay)
import Control.DeepSeq
import Control.Exception.Base
import Control.Monad.State
import Data.Bits
import Data.Text
import Data.Time
import Data.Vector.Unboxed.Base (Unbox)
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB  -- Boxed
import GHC.Generics (Generic)


-- Vector

data Vector = Vector !Float !Float !Float deriving (Eq, Generic, Show)

data instance V.MVector s Vector = MV_Vector !Int !(V.MVector s Float)
data instance V.Vector    Vector =  V_Vector !Int !(V.Vector    Float)

instance Unbox Vector

instance GM.MVector V.MVector Vector where
    basicLength (MV_Vector n _) = n
    basicUnsafeSlice m n (MV_Vector _ v) = MV_Vector n (GM.basicUnsafeSlice (3*m) (3*n) v)
    basicOverlaps (MV_Vector _ v) (MV_Vector _ u) = GM.basicOverlaps v u
    basicUnsafeNew n = liftM (MV_Vector n) (GM.basicUnsafeNew (3*n))
    basicUnsafeRead (MV_Vector _ v) i = do
        let o = 3*i
        x <- GM.basicUnsafeRead v o
        y <- GM.basicUnsafeRead v (o+1)
        z <- GM.basicUnsafeRead v (o+2)
        return (Vector x y z)
    basicUnsafeWrite (MV_Vector _ v) i (Vector x y z) = do
        let o = 3*i
        GM.basicUnsafeWrite v o     x
        GM.basicUnsafeWrite v (o+1) y
        GM.basicUnsafeWrite v (o+2) z
    basicInitialize (MV_Vector _ v) = GM.basicInitialize v

instance G.Vector V.Vector Vector where
    basicUnsafeFreeze (MV_Vector n v) = liftM ( V_Vector n) (G.basicUnsafeFreeze v)
    basicUnsafeThaw   (V_Vector n v) = liftM (MV_Vector n) (G.basicUnsafeThaw   v)
    basicLength       (V_Vector n _) = n
    basicUnsafeSlice m n (V_Vector _ v) = V_Vector n (G.basicUnsafeSlice (3*m) (3*n) v)
    basicUnsafeIndexM (V_Vector _ v) i = do
        let o = 3*i
        x <- G.basicUnsafeIndexM v o
        y <- G.basicUnsafeIndexM v (o+1)
        z <- G.basicUnsafeIndexM v (o+2)
        return (Vector x y z)

instance NFData Vector

addV :: Vector -> Vector -> Vector
addV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

subV :: Vector -> Vector -> Vector
subV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)

mulV :: Vector -> Vector -> Vector
mulV (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 * x2) (y1 * y2) (z1 * z2)

distV :: Vector -> Vector -> Float
distV v1 v2 = sqrt $ x*x + y*y + z*z
    where (Vector x y z) = v1 `subV` v2

-- Block

blockTypes :: Int
blockTypes = 256

type BlockId = Int

data Block = Block { bLocation      :: !Vector  -- x, y, z within the chunk
                   , bName          :: !Text
                   , bDurability    :: !Int
                   , bType          :: !BlockId
                   , bTextureId     :: !Int
                   , bBreakable     :: !Bool
                   , bVisible       :: !Bool
                   } deriving (Eq, Generic, Show)

-- WHNF = NF for a strict datatype:
instance NFData Block where rnf Block{} = ()

-- Enitity

data EntityType = Zombie
                | Chicken
                | Creeper
                | Enderman
                deriving (Eq, Enum, Show)

data Entity = Entity { eLocation    :: !Vector  -- x, y, z within the chunk
                     , eType        :: !EntityType
                     , eName        :: !Text
                     , eHp          :: !Int
                     , eSpeed       :: !Vector
                     } deriving (Eq, Show)

data instance V.MVector s EntityType = MV_EntityType !Int !(V.MVector s Int)
data instance V.Vector    EntityType =  V_EntityType !Int !(V.Vector    Int)

instance Unbox EntityType

instance GM.MVector V.MVector EntityType where
    basicLength (MV_EntityType n _) = n
    basicUnsafeSlice m n (MV_EntityType _ v) = MV_EntityType n (GM.basicUnsafeSlice m n v)
    basicOverlaps (MV_EntityType _ v) (MV_EntityType _ u) = GM.basicOverlaps v u
    basicUnsafeNew n = liftM (MV_EntityType n) (GM.basicUnsafeNew n)
    basicUnsafeRead (MV_EntityType _ v) i = GM.basicUnsafeRead v i >>= return . toEnum
    basicUnsafeWrite (MV_EntityType _ v) i e = GM.basicUnsafeWrite v i $ fromEnum e
    basicInitialize (MV_EntityType _ v) = GM.basicInitialize v

instance G.Vector V.Vector EntityType where
    basicUnsafeFreeze (MV_EntityType n v) = liftM ( V_EntityType n) (G.basicUnsafeFreeze v)
    basicUnsafeThaw   (V_EntityType n v) = liftM (MV_EntityType n) (G.basicUnsafeThaw   v)
    basicLength       (V_EntityType n _) = n
    basicUnsafeSlice m n (V_EntityType _ v) = V_EntityType n (G.basicUnsafeSlice m n v)
    basicUnsafeIndexM (V_EntityType _ v) i = G.basicUnsafeIndexM v i >>= return . toEnum

data instance V.MVector s Entity = MV_Entity !Int !(V.MVector s (Float, Float, Float, EntityType, Int))
data instance V.Vector    Entity =  V_Entity !Int !(V.Vector    (Float, Float, Float, EntityType, Int))

instance Unbox Entity

instance GM.MVector V.MVector Entity where
    basicLength (MV_Entity n _) = n
    basicUnsafeSlice m n (MV_Entity _ v) = MV_Entity n (GM.basicUnsafeSlice m n v)
    basicOverlaps (MV_Entity _ v) (MV_Entity _ u) = GM.basicOverlaps v u
    basicUnsafeNew n = liftM (MV_Entity n) (GM.basicUnsafeNew n)
    basicUnsafeRead (MV_Entity _ v) i = do
        (x, y, z, typ, hp) <- GM.basicUnsafeRead v i
        return $ Entity (Vector x y z) typ (entityName typ) hp (entitySpeed typ)
    basicUnsafeWrite (MV_Entity _ v) i (Entity (Vector x y z) eType _ eHp _) = GM.basicUnsafeWrite v i (x, y, z, eType, eHp)
    basicInitialize (MV_Entity _ v) = GM.basicInitialize v

instance G.Vector V.Vector Entity where
    basicUnsafeFreeze (MV_Entity n v) = liftM ( V_Entity n) (G.basicUnsafeFreeze v)
    basicUnsafeThaw   (V_Entity n v) = liftM (MV_Entity n) (G.basicUnsafeThaw   v)
    basicLength       (V_Entity n _) = n
    basicUnsafeSlice m n (V_Entity _ v) = V_Entity n (G.basicUnsafeSlice m n v)
    basicUnsafeIndexM (V_Entity _ v) i = do
        (x, y, z, typ, hp) <- G.basicUnsafeIndexM v i
        return $ Entity (Vector x y z) typ (entityName typ) hp (entitySpeed typ)

entityName :: EntityType -> Text
entityName = pack . show

entitySpeed :: EntityType -> Vector
entitySpeed e = case e of
                    Zombie      -> Vector 0.5 0 0.5       -- slow, can't fly
                    Chicken     -> Vector 0.75 0.25 0.75  -- can fly a bit
                    Creeper     -> Vector 0.75 0 0.75
                    Enderman    -> Vector 1 1 1           -- does what he wants

entityHp :: EntityType -> Int
entityHp e = case e of
                Zombie      -> 50
                Chicken     -> 25
                Creeper     -> 75
                Enderman    -> 500

entityMove :: Entity -> Entity
entityMove e = e { eLocation = eLocation e `addV` change }
    where
        change =
            let rngVector = Vector 1 1 1  -- Complex movement AI
            in rngVector `mulV` eSpeed e

mkEntity :: EntityType -> Vector -> Entity
mkEntity typ loc = Entity { eLocation = loc
                          , eType = typ
                          , eName = entityName typ
                          , eHp = entityHp typ
                          , eSpeed = entitySpeed typ
                          }

-- Chunk

chunkBlocks :: Int
chunkBlocks = 65536

chunkEntities :: Int
chunkEntities = 1000

data Chunk = Chunk { cLocation  :: !Vector
                   , cBlocks    :: !(V.Vector BlockId)
                   , cEntities  :: !(V.Vector Entity)
                   } deriving (Eq, Show)

-- Don't need to force unboxed vecs:
instance NFData Chunk where rnf Chunk{} = ()

mkChunk :: Vector -> Chunk
mkChunk v = Chunk { cLocation = v  -- x, y, z within the world
                  , cBlocks = V.generate chunkBlocks fromIntegral
                  , cEntities = V.generate chunkEntities genEntity
                  }
    where
        genEntity n =
            let i = fromIntegral n
            in case n .&. 3 of
                0 -> mkEntity Chicken   (Vector i i i)
                1 -> mkEntity Zombie    (Vector i i i)
                2 -> mkEntity Creeper   (Vector i i i)
                _ -> mkEntity Enderman  (Vector i i i)

processEntities :: V.Vector Entity -> V.Vector Entity
processEntities = V.map entityMove

-- Game

gameChunks :: Int
gameChunks = 100

data Game = Game { gChunks      :: !(VB.Vector Chunk)
                 , gChunkCount  :: !Int
                 , gBlocks      :: !(VB.Vector Block)
                 , gPlayerLoc   :: !Vector
                 } deriving (Eq, Show)

-- Don't need to force unboxed vecs:
instance NFData Game where
  rnf Game{..} =
      rnf gBlocks `seq`
      rnf gChunks `seq`
      ()

loadWorld :: Game
loadWorld = Game { gChunks = fmap mkChunk vectorSeq
                 , gChunkCount = gameChunks
                 , gBlocks = VB.generate blockTypes mkBlock
                 , gPlayerLoc = Vector 0 0 0
                 }
    where
        vectorSeq = VB.generate gameChunks (\i -> let f = fromIntegral i in Vector f f f)
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
    if wait > 0 then threadDelay wait else return ()

gameLoop :: Int -> Game -> IO ()
gameLoop 0 _ = return ()
gameLoop iter game = do
    start <- getCurrentTime
    let game' = loop game
    evaluate $ rnf game
    end <- getCurrentTime
    let diff = diffUTCTime end start
    sleep16 diff
    putStrLn $ show (100 - iter) ++ ": " ++ (show $ diff)
    gameLoop (iter - 1) game'

main :: IO ()
main = do
    putStrLn "Loading World..."
    start <- getCurrentTime
    let game = loadWorld
    evaluate $ rnf game
    end <- getCurrentTime
    putStrLn "FINISHED!"
    putStrLn $ "Load time: " ++ (show $ diffUTCTime end start)
    gameLoop 100 game
