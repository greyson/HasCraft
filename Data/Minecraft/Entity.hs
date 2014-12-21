module Data.Minecraft.Entity where

import Prelude hiding (id)

import Control.Applicative ( (<$>) )
import Data.Maybe (catMaybes)
import Data.Minecraft.Common
import Data.NBT.MCPE (NBT, (</>))
import Data.NBT.MCPE ( NBTPayload(..) )

import qualified Data.NBT.MCPE as NBT
import qualified Data.Text as T

data Entity = Entity { id :: EntityId
                     , position :: Position
                     , motion :: Position
                     , rotation :: Rotation
                     , fallDistance :: Float
                     , fire :: Ticks
                     , air :: Ticks
                     , onGround :: Bool

                       -- A place to put polymorphic info
                     , facets :: [EntityFacet]
                     }
              deriving (Show, Eq)

data EntityFacet = Damagable { health :: Int }
                 | Fightable { attackTime :: Ticks
                             , deathTime :: Ticks
                             , hurtTime :: Ticks }
                 | Aging { age :: Ticks }
                 | Wooly { sheared :: Bool
                         , color :: Color }
                 | Playable { bed :: Position
                            , spawn :: Position
                            , sleepTimer :: Ticks
                            , sleeping :: Bool
                              -- TODO: inventory/armor
                            , name :: Maybe T.Text }
                 deriving (Show, Ord, Eq)

nonEntity id position =
  Entity { id = id
         , position = position
         , motion = Position 0 0 0 -- not moving yet
         , rotation = Rotation 0 0 -- not rotating yet
         , fallDistance = 0 -- not falling yet
         , fire = -20 -- not on fire yet
         , air = 300 -- not drowning yet
         , onGround = True
         , facets = []
         }

passiveMobFacets life = [ Damagable { health = life }
                        , Aging { age = 0 }
                        , Fightable { attackTime = 0
                                    , deathTime = 0
                                    , hurtTime = 0
                                    }
                        ]


create Chicken position =
  (nonEntity Chicken position) { facets = passiveMobFacets 4 }

create Sheep position =
  let sheepFacets = (Wooly False White):passiveMobFacets 8
  in (nonEntity Sheep position) { facets = sheepFacets }

data EntityId = Unknown Int
                -- npc
              | Villager

                -- passive
              | Chicken
              | Cow
              | Pig
              | Sheep
              | Wolf
              | Mooshroom

                -- hostile
              | Zombie
              | Creeper
              | Skeleton
              | Spider
              | ZombiePigman
              | Slime
              | Enderman
              | Silverfish

                -- pc
              | Player

                -- drops
              | DroppedItem

                -- blocks
              | PrimedTNT
              | FallingBlock

                -- immobile and projectile
              | ShotArrow
              | ThrownSnowball
              | ThrownEgg
              | Painting
              | Minecart
              | OtherEntity Int
              deriving (Show, Ord, Eq)

instance Enum EntityId where
  toEnum e = case e of
    0x40 -> DroppedItem
    0x41 -> PrimedTNT
    0x42 -> FallingBlock
    0x50 -> ShotArrow
    0x51 -> ThrownSnowball
    0x52 -> ThrownEgg
    0x53 -> Painting
    0x54 -> Minecart
    0x27 -> Silverfish
    0x26 -> Enderman
    0x25 -> Slime
    0x24 -> ZombiePigman
    0x23 -> Spider
    0x22 -> Skeleton
    0x21 -> Creeper
    0x20 -> Zombie
    0x0a -> Chicken
    0x0b -> Cow
    0x0c -> Pig
    0x0d -> Sheep
    0x0e -> Wolf
    0x10 -> Mooshroom
    0x0f -> Villager
    0x3f -> Player
    other -> Unknown other

  fromEnum i = case i of
    DroppedItem -> 0x40
    PrimedTNT -> 0x41
    FallingBlock -> 0x42
    ShotArrow -> 0x50
    ThrownSnowball -> 0x51
    ThrownEgg -> 0x52
    Painting -> 0x53
    Minecart -> 0x54
    Silverfish -> 0x27
    Enderman -> 0x26
    Slime -> 0x25
    ZombiePigman -> 0x24
    Spider -> 0x23
    Skeleton -> 0x22
    Creeper -> 0x21
    Zombie -> 0x20
    Chicken -> 0x0a
    Cow -> 0x0b
    Pig -> 0x0c
    Sheep -> 0x0d
    Wolf -> 0x0e
    Mooshroom -> 0x10
    Villager -> 0x0f
    Player -> 0x3f
    (Unknown i) -> i


fromNBT :: NBT -> Maybe Entity
fromNBT nbt = do
  _id <- NBT.asIntegral <$> nbt </> "id"
  _pos <- NBT.asPosition <$> nbt </> "Pos"
  _mot <- NBT.asPosition <$> nbt </> "Motion"
  _rot <- NBT.asRotation <$> nbt </> "Rotation"
  _fall <- NBT.asFloat <$> nbt </> "FallDistance"
  _fire <- NBT.asIntegral <$> nbt </> "Fire"
  _air <- NBT.asIntegral <$> nbt </> "Air"
  _ground <- (==1) . NBT.asIntegral <$> nbt </> "OnGround"

  return $ (nonEntity (toEnum $ fromIntegral _id) _pos) {
               motion = _mot,
               rotation = _rot,
               fallDistance = _fall,
               fire = _fire,
               air = _air,
               onGround = _ground,
               facets = readFacets nbt }

readFacets :: NBT -> [EntityFacet]
readFacets nbt =
  catMaybes $ map (\f -> f nbt) [ readDamagable
                               , readAging
                               , readFightable
                               , readWooly
                               , readPlayable
                               ]

readDamagable nbt = do
  _health <- NBT.asIntegral <$> nbt </> "Health"
  return $ Damagable { health = _health }

readAging nbt = do
  _age <- NBT.asIntegral <$> nbt </> "Age"
  return $ Aging { age = _age }

readFightable nbt = do
  _att <- NBT.asIntegral <$> nbt </> "AttackTime"
  _dea <- NBT.asIntegral <$> nbt </> "DeathTime"
  _hur <- NBT.asIntegral <$> nbt </> "HurtTime"
  return $ Fightable { attackTime = _att
                     , deathTime = _dea
                     , hurtTime = _hur }

readWooly nbt = do
  _shear <- (==1) . NBT.asIntegral <$> nbt </> "Sheared"
  _color <- toEnum . NBT.asIntegral <$> nbt </> "Color"
  return $ Wooly { sheared = _shear
                 , color = _color }

readPlayable nbt = do
  _bed <- positional nbt "BedPosition"
  _spawn <- positional nbt "Spawn"
  _timer <- NBT.asIntegral <$> nbt </> "SleepTimer"
  _sleep <- (==1) . NBT.asIntegral <$> nbt </> "Sleeping"

  return Playable { bed = _bed
                  , spawn = _spawn
                  , sleepTimer = _timer
                  , sleeping = _sleep
                  , name = (\(StringTag s) -> s) <$> nbt </> "Name"
                  }

positional nbt name = do
  let coord c = fromIntegral . NBT.asIntegral <$> nbt </> (name `T.snoc` c)
  _x <- coord 'X'
  _y <- coord 'Y'
  _z <- coord 'Z'
  return $ Position _x _y _z
