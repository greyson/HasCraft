module Data.Minecraft.Entity where

import Data.Minecraft.Common

import Data.Map ( Map(..), (!) )

import qualified Data.Map as Map


data AttrName = AttrId | AttrPos | AttrMotion | AttrRotation
              | AttrFallDist | AttrFire | AttrAir | AttrGround

              | AttrHealth

              | AttackTime | DeathTime | HurtTime

              | Age

              | Sheared | WoolColor
              deriving (Ord, Show, Eq)

data AttrValue = BoolAttr Bool
               | ColorAttr Color
               | FloatAttr Float
               | IdAttr EntityId
               | IntAttr Int
               | PosAttr Position
               | RotatAttr Rotation
               | TickAttr Ticks

type Attributes = Map AttrName AttrValue

getBool (BoolAttr b) = b
getColor (ColorAttr w) = w
getFloat (FloatAttr f) = f
getId (IdAttr ei) = ei
getInt (IntAttr i) = i
getPosition (PosAttr p) = p
getRotation (RotatAttr r) = r
getTicks (TickAttr t) = t

-- implementation of all entities
data EntityBase = EntityBase { _attr :: Attributes }

instance Attributed EntityBase where
  attributes = _attr

class Attributed a where
  attributes :: a -> Attributes

  attr :: AttrName -> (AttrValue -> b) -> a -> b
  attr k f a = f $ (attributes a) ! k

-- All entity types
class Attributed a => Entity a where
  id :: a -> EntityId
  position :: a -> Position
  motion :: a -> Position
  rotation :: a -> Rotation
  fallDistance :: a -> Float
  fire :: a -> Ticks
  air :: a -> Ticks
  onGround :: a -> Bool

  -- Defaults (with Attributed, may be different.
  id = attr AttrId getId
  position = attr AttrPos getPosition
  motion = attr AttrMotion getPosition
  rotation = attr AttrRotation getRotation
  fallDistance = attr AttrFallDist getFloat
  fire = attr AttrFire getTicks
  air = attr AttrAir getTicks
  onGround = attr AttrGround getBool


-- Items and mobs
class Entity a => HurtableEntity a where
  health :: a -> Int
  health = attr AttrHealth getInt

-- Yes, mobs can be hurt.
class HurtableEntity a => MobEntity a where
  attackTime :: a -> Ticks
  deathTime :: a -> Ticks
  hurtTime :: a -> Ticks -- Guessing at type based on the name

  attackTime = attr AttackTime getTicks
  deathTime = attr DeathTime getTicks
  hurtTime = attr HurtTime getTicks

-- Sheep and item entities
class Entity a => AgedEntity a where
  age :: a -> Ticks

  age = attr Age getTicks

-- Just sheep (don't shear creepers)
class (AgedEntity a, MobEntity a) => WoolyEntity a where
  sheared :: a -> Bool
  color :: a -> Color

  sheared = attr Sheared getBool
  color = attr WoolColor getColor

class (AgedEntity a, HurtableEntity a) => ItemEntity a

type Sheep = EntityBase

instance WoolyEntity Sheep

data EntityId = Player      -- 0x3F
              | Villager    -- 0x0F
              | Chicken     -- 0x10
              | Creeper     -- 0x21
              | Arrow       -- 0x50
              | FallingSand -- 0x42
              | DroppedItem -- 0x40
              | OtherEntity Int
