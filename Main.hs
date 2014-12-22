module Main where

import Control.Applicative ( (<$>) )
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Database.LevelDB (runResourceT)

import Data.NBT.MCPE
import Database.MCPE
import Data.Minecraft.Entity
import Data.Minecraft.Chunk
import Data.Minecraft.Block

-- for interactive
import qualified Data.Map as M
import qualified Data.Text as T

path = "My World/db"

-- DB.open ::

getPlayer :: IO (Maybe NBT)
getPlayer = runResourceT $ open path >>= getNbt LocalPlayer

getChunk0Terrain db = loadChunk 0 0 db

getBlockColumn x z ter =
  map (\y -> getBlock x y z ter) [0..127]

getChunkLayer y ter =
  map (\(x,z) -> getBlock x y z ter) $ cartProd [0..15] [0..15]
  where
    cartProd is js = [(j,i) | i <- is, j <- js]

layerToAscii y ter =
  splitN 16 $ map blockAscii $ getChunkLayer y ter

splitN n list
  | null list = []
  | otherwise =
    let (line, rest) = splitAt n list
    in line:splitN n rest

layer n = runResourceT $ do
  db <- open path
  -- remember to load the chunk only once; re-fetching is terribly slow.
  chunk0terrain <- terrain <$> getChunk0Terrain db

  liftIO $ putStrLn $ unlines (layerToAscii n chunk0terrain)
  liftIO $ putStrLn $ unlines (layerToAscii (n+1) chunk0terrain)

blockAscii bt =
  case M.lookup bt asciiBlock of
    Nothing -> '?'
    Just c -> c

asciiBlock = M.fromList [(Air,                ' '), (Stone,              '#'),
                         (GrassBlock,         '.'), (Dirt,               ','),
                         (Cobblestone,        '%'), (OakPlank,           '='),
                         (Sapling,            '!'), (Bedrock,            '&'),
                         (Water,              '~'), (StationaryWater,    '-'),
                         (Lava,               'x'), (StationaryLava ,    'X'),
                         (Sand,               'o'), (Gravel,             ':'),
                         (GoldOre,            '$'), (IronOre,            '@'),
                         (CoalOre,            'b'), (Wood,               '|'),
                         (Leaves,             '^'), (Sponge,             '¶'),
                         (Glass,              'O'), (LapisOre,           'B'),
                         (LapisBlock,         '/'), (Sandstone,          '/'),
                         (Bed,                '/'), (PoweredRail,        '/'),
                         (Cobweb,             '/'), (TallGrass,          '/'),
                         (DeadBush,           '/'), (Wool,               '/'),
                         (Dandelion,          '/'), (Poppy,              '/'),
                         (BrownMushroom,      '/'), (RedMushroom,        '/'),
                         (GoldBlock,          '/'), (IronBlock,          '/'),
                         (DoubleStoneSlab,    '/'), (StoneSlab,          '/'),
                         (BrickBlock,         '/'), (TNT,                '/'),
                         (Bookshelf,          '/'), (MossStone,          '/'),
                         (Obsidian,           '/'), (Torch,              '/'),
                         (Fire,               '/'), (MonsterSpawner,     '/'),
                         (OakStairs,          '/'), (Chest,              '/'),
                         (DiamondOre,         '/'), (DiamondBlock,       '/'),
                         (CraftingTable,      '/'), (Seeds,              '/'),
                         (Farmland,           '/'), (Furnace,            '/'),
                         (BurningFurnace,     '/'), (SignPost,           '/'),
                         (OakDoor,            '/'), (Ladder,             '/'),
                         (Rail,               '/'), (CobblestoneStairs,  '/'),
                         (WallSign,           '/'), (IronDoor,           '/'),
                         (RedstoneOre,        '/'), (GlowingRedstoneOre, '/'),
                         (SnowCover,          '/'), (Ice,                '/'),
                         (Snow,               '/'), (Cactus,             '/'),
                         (Clay,               '/'), (SugarCane,          '/'),
                         (Fence,              '/'), (Pumpkin,            '/'),
                         (Netherrack,         '/'), (Glowstone,          '/'),
                         (JackOLantern,       '/'), (CakeBlock,          '/'),
                         (InvisibleBedrock,   '/'), (Trapdoor,           '/'),
                         (StoneBrick,         '/'), (HugeBrownMushroom,  '/'),
                         (HugeRedMushroom,    '/'), (IronBars,           '/'),
                         (GlassPane,          '/'), (Melon,              '/'),
                         (PumpkinStem,        '/'), (MelonStem,          '/'),
                         (Vines,              '/'), (FenceGate,          '/'),
                         (BrickStairs,        '/'), (StoneBrickStairs,   '/'),
                         (Mycellium,          '/'), (LilyPad,            '/'),
                         (NetherBrick,        '/'), (NetherBrickStairs,  '/'),
                         (EndPortalFrame,     '/'), (EndStone,           '/'),
                         (Cocoa,              '/'), (SandstoneStairs,    '/'),
                         (EmeraldOre,         '/'), (EmeraldBlock,       '/'),
                         (SpruceStairs,       '/'), (BirchStairs,        '/'),
                         (JungleStairs,       '/'), (CobblestoneWall,    '/'),
                         (Carrots,            '/'), (Potato,             '/'),
                         (QuartzBlock,        '/'), (QuartzStairs,       '/'),
                         (OakDoubleSlab,      '/'), (OakSlab,            '/'),
                         (StainedClay,        '/'), (AcaciaStairs,       '/'),
                         (DarkOakStairs,      '/'), (HayBlock,           '/'),
                         (Carpet,             '/'), (HardClay,           '/'),
                         (CoalBlock,          '/'), (PackedIce,          '/'),
                         (Podzol,             '/'), (Beetroot,           '/'),
                         (StoneCutter,        '/'), (GlowingObsidian,    '/'),
                         (NetherReactor,      '/'), (UpdateGameBE,       '/'),
                         (UpdateGameLE,       '/'), (NameBlock,          '/')]
