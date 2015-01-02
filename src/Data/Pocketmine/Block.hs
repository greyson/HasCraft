module Data.Pocketmine.Block
       ( BlockType(..) ) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)

import qualified Data.Map as M

{-
 - Yes, I know most of this code is terribly inefficient doing linear lookups
 - for everything. However! I'm currently in the "make it work" phase, which comes
 - before the "make it beautiful" phase, which comes before the "make it fast" phase.
 -
 - So it's ugly too.
 -
 - I know.
 -}

data BlockType = Air | Stone | GrassBlock | Dirt | Cobblestone
               | OakPlank | Sapling | Bedrock | Water | StationaryWater
               | Lava | StationaryLava | Sand | Gravel | GoldOre
               | IronOre | CoalOre | Wood | Leaves | Sponge
               | Glass | LapisOre | LapisBlock | Sandstone | Bed
               | PoweredRail | Cobweb | TallGrass | DeadBush | Wool
               | Dandelion | Poppy | BrownMushroom | RedMushroom | GoldBlock
               | IronBlock | DoubleStoneSlab | StoneSlab | BrickBlock | TNT
               | Bookshelf | MossStone | Obsidian | Torch | Fire
               | MonsterSpawner | OakStairs | Chest | DiamondOre | DiamondBlock
               | CraftingTable | Seeds | Farmland | Furnace | BurningFurnace
               | SignPost | OakDoor | Ladder | Rail | CobblestoneStairs
               | WallSign | IronDoor | RedstoneOre | GlowingRedstoneOre | SnowCover
               | Ice | Snow | Cactus | Clay | SugarCane
               | Fence | Pumpkin | Netherrack | Glowstone | JackOLantern
               | CakeBlock | InvisibleBedrock | Trapdoor | StoneBrick | HugeBrownMushroom
               | HugeRedMushroom | IronBars | GlassPane | Melon | PumpkinStem
               | MelonStem | Vines | FenceGate | BrickStairs | StoneBrickStairs
               | Mycellium | LilyPad | NetherBrick | NetherBrickStairs | EndPortalFrame
               | EndStone | Cocoa | SandstoneStairs | EmeraldOre | EmeraldBlock
               | SpruceStairs | BirchStairs | JungleStairs | CobblestoneWall | Carrots
               | Potato | QuartzBlock | QuartzStairs | OakDoubleSlab | OakSlab
               | StainedClay | AcaciaStairs
               | DarkOakStairs | HayBlock | Carpet | HardClay | CoalBlock
               | PackedIce | LargeFern | Podzol | Beetroot | StoneCutter | GlowingObsidian
               | NetherReactor | UpdateGameBE | UpdateGameLE | NameBlock | Unknown Int
               deriving (Show, Eq, Ord)

instance Enum BlockType where
  fromEnum (Unknown i) = i
  fromEnum e = fromJust $ M.lookup e enumMap
  toEnum i = maybe (Unknown i) id $ M.lookup i idMap

enumMap = M.fromList blockIds
idMap = M.fromList (map swap blockIds)

blockIds = [(Air,                0x00),
            (Stone,              0x01),
            (GrassBlock,         0x02),
            (Dirt,               0x03),
            (Cobblestone,        0x04),
            (OakPlank,           0x05),
            (Sapling,            0x06),
            (Bedrock,            0x07),
            (Water,              0x08),
            (StationaryWater,    0x09),
            (Lava,               0x0a),
            (StationaryLava,     0x0b),
            (Sand,               0x0c),
            (Gravel,             0x0d),
            (GoldOre,            0x0e),
            (IronOre,            0x0f),
            (CoalOre,            0x10),
            (Wood,               0x11),
            (Leaves,             0x12),
            (Sponge,             0x13),
            (Glass,              0x14),
            (LapisOre,           0x15),
            (LapisBlock,         0x16),
            (Sandstone,          0x18),
            (Bed,                0x1a),
            (PoweredRail,        0x1b),
            (Cobweb,             0x1e),
            (TallGrass,          0x1f),
            (DeadBush,           0x20),
            (Wool,               0x23),
            (Dandelion,          0x25),
            (Poppy,              0x26),
            (BrownMushroom,      0x27),
            (RedMushroom,        0x28),
            (GoldBlock,          0x29),
            (IronBlock,          0x2a),
            (DoubleStoneSlab,    0x2b),
            (StoneSlab,          0x2c),
            (BrickBlock,         0x2d),
            (TNT,                0x2e),
            (Bookshelf,          0x2f),
            (MossStone,          0x30),
            (Obsidian,           0x31),
            (Torch,              0x32),
            (Fire,               0x33),
            (MonsterSpawner,     0x34),
            (OakStairs,          0x35),
            (Chest,              0x36),
            (DiamondOre,         0x38),
            (DiamondBlock,       0x39),
            (CraftingTable,      0x3a),
            (Seeds,              0x3b),
            (Farmland,           0x3c),
            (Furnace,            0x3d),
            (BurningFurnace,     0x3e),
            (SignPost,           0x3f),
            (OakDoor,            0x40),
            (Ladder,             0x41),
            (Rail,               0x42),
            (CobblestoneStairs,  0x43),
            (WallSign,           0x44),
            (IronDoor,           0x47),
            (RedstoneOre,        0x49),
            (GlowingRedstoneOre, 0x4a),
            (SnowCover,          0x4e),
            (Ice,                0x4f),
            (Snow,               0x50),
            (Cactus,             0x51),
            (Clay,               0x52),
            (SugarCane,          0x53),
            (Fence,              0x55),
            (Pumpkin,            0x56),
            (Netherrack,         0x57),
            (Glowstone,          0x59),
            (JackOLantern,       0x5b),
            (CakeBlock,          0x5c),
            (InvisibleBedrock,   0x5f),
            (Trapdoor,           0x60),
            (StoneBrick,         0x62),
            (HugeBrownMushroom,  0x63),
            (HugeRedMushroom,    0x64),
            (IronBars,           0x65),
            (GlassPane,          0x66),
            (Melon,              0x67),
            (PumpkinStem,        0x68),
            (MelonStem,          0x69),
            (Vines,              0x6a),
            (FenceGate,          0x6b),
            (BrickStairs,        0x6c),
            (StoneBrickStairs,   0x6d),
            (Mycellium,          0x6e),
            (LilyPad,            0x6f),
            (NetherBrick,        0x70),
            (NetherBrickStairs,  0x72),
            (EndPortalFrame,     0x78),
            (EndStone,           0x79),
            (Cocoa,              0x7f),
            (SandstoneStairs,    0x80),
            (EmeraldOre,         0x81),
            (EmeraldBlock,       0x85),
            (SpruceStairs,       0x86),
            (BirchStairs,        0x87),
            (JungleStairs,       0x88),
            (CobblestoneWall,    0x89),
            (Carrots,            0x8d),
            (Potato,             0x8e),
            (QuartzBlock,        0x9b),
            (QuartzStairs,       0x9c),
            (OakDoubleSlab,      0x9d),
            (OakSlab,            0x9e),
            (StainedClay,        0x9f),
            (AcaciaStairs,       0xa3),
            (DarkOakStairs,      0xa4),
            (HayBlock,           0xaa),
            (Carpet,             0xab),
            (HardClay,           0xac),
            (CoalBlock,          0xad),
            (PackedIce,          0xae),
            (LargeFern,          0xaf),
            (Podzol,             0xf3),
            (Beetroot,           0xf4),
            (StoneCutter,        0xf5),
            (GlowingObsidian,    0xf6),
            (NetherReactor,      0xf7),
            (UpdateGameBE,       0xf8),
            (UpdateGameLE,       0xf9),
            (NameBlock,          0xff)]
