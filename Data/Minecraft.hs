module Data.Minecraft where

import Data.Text

data EntityId = Player      -- 0x3F
              | Villager    -- 0x0F
              | Chicken     -- 0x10
              | Creeper     -- 0x21
              | Arrow       -- 0x50
              | FallingSand -- 0x42
              | DroppedItem -- 0x40
              | OtherEntity Int

data WoolColor = White     | Orange | Magenta | LightBlue
               | Yellow    | Lime   | Pink    | Gray
               | LightGray | Cyan   | Purple  | Blue
               | Brown     | Green  | Red     | Black
               deriving (Show, Enum, Eq, Ord)

data Position = Position { x :: Float
                         , y :: Float
                         , z :: Float
                         }

data Rotation = Rotation { yaw :: Float
                         , pitch :: Float
                         }

-- 20 ticks = 1 second
type Ticks = Int

-- All entities
class Entity a where
  id :: a -> EntityId
  pos :: a -> Position
  motion :: a -> Position
  rotation :: a -> Rotation
  fallDistance :: a -> Float
  fire :: a -> Ticks
  air :: a -> Ticks
  onGround :: a -> Bool

-- Items and mobs
class Entity a => HurtableEntity a where
  health :: a -> Int

-- Yes, mobs can be hurt.
class HurtableEntity a => MobEntity a where
  attackTime :: a -> Ticks
  deathTime :: a -> Ticks
  hurtTime :: a -> Int

-- Sheep and item entities
class Entity a => AgedEntity a where
  age :: a -> Ticks

-- Just sheep (don't shear creepers)
class AgedEntity a => WoolyEntity a where
  sheared :: a -> Bool
  color :: a -> WoolColor

class (AgedEntity a, HurtableEntity a) => ItemEntity a

data GameType = Survival | Creative
              deriving (Show, Enum, Eq)

data Dimension = Overworld
               deriving (Show, Enum, Eq)

data Chunk

-- This is kind of important.
newtype Generator = Generator {
  generation :: World -> Int -> Int -> Chunk
  }

data World = World { dimension :: Dimension
                   , gameType :: GameType
                   , generator :: Generator
                   , lastPlayed :: Integer
                   , levelName :: Text
                   , limitedWorldOrigin :: Maybe Position
                   , _platform :: Int -- unknown, android/ios/raspi?
                   , seed :: Integer
                   , _size :: Integer -- appears to be 0
                   , _storageVersion :: Int -- 4?
                   , worldSpawn :: Position
                   , time :: Ticks
                   , spawnMobs :: Bool
                   }
