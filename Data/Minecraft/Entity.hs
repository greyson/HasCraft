module Data.Minecraft.Entity where

import Data.Minecraft.Common

data Entity = Entity { id :: EntityId
                     , position :: Position
                     , motion :: Position
                     , rotation :: Rotation
                     , fallDistance :: Float
                     , fire :: Ticks
                     , air :: Ticks
                     , onGround :: Bool

                       -- A place to put polymorphic info
                     , facets :: [EntityFacets]
                     }

data EntityFacets = Damagable { health :: Int }
                  | Fightable { attackTime :: Ticks
                              , deathTime :: Ticks
                              , hurtTime :: Ticks
                              }
                  | Aging { age :: Ticks }
                  | Wooly { sheared :: Bool
                          , color :: Color
                          }

data EntityId = Player      -- 0x3F
              | Villager    -- 0x0F
              | Chicken     -- 0x10
              | Creeper     -- 0x21
              | Arrow       -- 0x50
              | FallingSand -- 0x42
              | DroppedItem -- 0x40
              | OtherEntity Int
