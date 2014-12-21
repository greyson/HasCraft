module Data.Minecraft.Chunk where

import Data.Array.ST

import Data.Minecraft.Entity

data BlockType = Air | Stone | GrassBlock | Dirt | Cobblestone
               | WoodenPlank | Sapling | Bedrock | Water | StationaryWater
               | Lava | StationaryLava | Sand | Gravel | GoldOre
               | IronOre | CoalOre | Wood | Leaves | Sponge
               | Glass | LapisOre | LapisBlock | Sandstone | Bed
               | PoweredRail | Cobweb | TallGrass | DeadBush | Wool
               | Dandelion | Poppy | BrownMushroom | RedMushroom | GoldBlock
               | IronBlock | DoubleStoneSlab | StoneSlab | BrickBlock | TNT
               | Bookshelf | MossStone | Obsidian | Torch | Fire
               | MonsterSpawner | OakStairs | Chest | DiamondOre | DiamondBlock
               | CraftingTable | Seeds | Farmland | Furnace | BurningFurnace
               | SignPost | WoodenDoor | Ladder | Rail | CobblestoneStairs
               | WallSign | IronDoor | RedstoneOre | GlowingRedstoneOre | SnowCover
               | Ice | Snow | Cactus | Clay | SugarCane
               | Fence | Pumpkin | Netherrack | Glowstone | JackOLantern
               | CakeBlock | InvisibleBedrock | Trapdoor | StoneBrick | HugeBrownMushroom
               | HugeRedMushroom | IronBars | GlassPane | Melon | PumpkinStem
               | MelonStem | Vines | FenceGate | BrickStairs | StoneBrickStairs
               | Mycellium | LilyPad | NetherBrick | NetherBrickStairs | EndPortalFrame
               | EndStone | Cocoa | SandstoneStairs | EmeraldOre | EmeraldBlock
               | SpruceStairs | BirchStairs | JungleStairs | CobblestoneWall | Carrots
               | Potato | QuartzBlock | QuartzStairs | WoodDoubleSlab | WoodSlab
               | DarkOakStairs | HayBlock | Carpet | HardClay | CoalBlock
               | PackedIce | Podzol | Beetroot | StoneCutter | GlowingObsidian
               | NetherReactor | UpdateGameBE | UpdateGameLE | NameBlock | Unknown Int



data Terrain = Terrain

data Chunk = Chunk { terrain :: Terrain
                   , entities :: Entity a => [a]
                   }