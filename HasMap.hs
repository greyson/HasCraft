{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Applicative ( (<$>) )
import Control.Monad (liftM, ap)
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource
import UI.NCurses
import System.Environment (getArgs)

import qualified Control.Applicative as A
import qualified Data.Map as M
import qualified Data.ByteString as B

import Data.Minecraft.Block
import Data.Minecraft.Chunk
import Database.MCPE

main = getArgs >>= \[x, z] ->
  runResourceT $ transResourceT runCurses $ test (read x) (read z)

instance MonadThrow Curses where
  throwM = throwM

instance MonadBase IO Curses where liftBase = liftIO


path = "My World/db"

getTopLayer x z = do
  db <- open path
  splitN 16 . chunkTopLayer <$> loadChunk x z db

test :: Int -> Int -> ResourceT Curses ()
test x z = do
  db <- open path
  topLayer <- splitN 16 . chunkTopLayer <$> (loadChunk x z db)

  w <- lift defaultWindow
  lift $ renderChunkLayer 5 20 topLayer
  lift $ getEvent w Nothing
  return ()

renderChunkLayer :: Int -> Int -> [[BlockType]] -> Curses ()
renderChunkLayer x y layer = do
  w <- defaultWindow
  updateWindow w $ do
    setColor defaultColorID
    moveCursor (fromIntegral x) (fromIntegral y)

    let drawBlockLine (no, line) =
          moveCursor no (fromIntegral x) >>
          drawString (concatMap blockAnsi (reverse line))

    sequence $ map drawBlockLine (zip [(fromIntegral y)..] layer)
  render

splitN n list
  | null list = []
  | otherwise =
    let (line, rest) = splitAt n list
    in line:splitN n rest

chunkBlockType :: Chunk -> [BlockType]
chunkBlockType = map (toEnum . fromIntegral) . B.unpack . terrain

chunkTopLayer chunk =
  map (head . dropWhile (== Air) . reverse) $ splitN 128 (chunkBlockType chunk)

-- Borrowed from original escape-code implementation

blockAnsi bt =
  case M.lookup bt ansiBlock of
    Nothing -> "??"
    Just s -> s

ansiBlock = M.fromList
   [(Air,                "  "), (Stone,             "##"),
    (GrassBlock,         "  "), (Dirt,              ",,"),
    (Cobblestone,        "%%"), (OakPlank,          "=="),
    (Sapling,            "!!"), (Bedrock,           "&&"),
    (Water,              "  "), (StationaryWater,   "~~"),
    (Lava,               "xx"), (StationaryLava ,   "XX"),
    (Sand,               "oo"), (Gravel,            "::"),
    (GoldOre,            "$$"), (IronOre,           "@@"),
    (CoalOre,            "bb"), (Wood,              "||"),
    (Leaves,             "^^"), (Sponge,            "¶¶"),
    (Glass,              "OO"), (LapisOre,          "BB"),
    (LapisBlock,         "//"), (Sandstone,         "//"),
    (Bed,                "//"), (PoweredRail,       "//"),
    (Cobweb,             "//"), (TallGrass,         "//"),
    (DeadBush,           "//"), (Wool,              "//"),
    (Dandelion,          "//"), (Poppy,             "//"),
    (BrownMushroom,      "//"), (RedMushroom,       "//"),
    (GoldBlock,          "//"), (IronBlock,         "//"),
    (DoubleStoneSlab,    "//"), (StoneSlab,         "//"),
    (BrickBlock,         "//"), (TNT,               "//"),
    (Bookshelf,          "//"), (MossStone,         "//"),
    (Obsidian,           "//"), (Torch,             "//"),
    (Fire,               "//"), (MonsterSpawner,    "//"),
    (OakStairs,          "//"), (Chest,             "//"),
    (DiamondOre,         "//"), (DiamondBlock,      "//"),
    (CraftingTable,      "//"), (Seeds,             "//"),
    (Farmland,           "//"), (Furnace,           "//"),
    (BurningFurnace,     "//"), (SignPost,          "//"),
    (OakDoor,            "//"), (Ladder,            "//"),
    (Rail,               "//"), (CobblestoneStairs, "//"),
    (WallSign,           "//"), (IronDoor,          "//"),
    (RedstoneOre,        "//"), (GlowingRedstoneOre,"//"),
    (SnowCover,          "//"), (Ice,               "//"),
    (Snow,               "//"), (Cactus,            "//"),
    (Clay,               "//"), (SugarCane,         "||"),
    (Fence,              "//"), (Pumpkin,           "//"),
    (Netherrack,         "//"), (Glowstone,         "//"),
    (JackOLantern,       "//"), (CakeBlock,         "//"),
    (InvisibleBedrock,   "//"), (Trapdoor,          "//"),
    (StoneBrick,         "//"), (HugeBrownMushroom, "//"),
    (HugeRedMushroom,    "//"), (IronBars,          "//"),
    (GlassPane,          "//"), (Melon,             "//"),
    (PumpkinStem,        "//"), (MelonStem,         "//"),
    (Vines,              "//"), (FenceGate,         "//"),
    (BrickStairs,        "//"), (StoneBrickStairs,  "//"),
    (Mycellium,          "//"), (LilyPad,           "//"),
    (NetherBrick,        "//"), (NetherBrickStairs, "//"),
    (EndPortalFrame,     "//"), (EndStone,          "//"),
    (Cocoa,              "//"), (SandstoneStairs,   "//"),
    (EmeraldOre,         "//"), (EmeraldBlock,      "//"),
    (SpruceStairs,       "//"), (BirchStairs,       "//"),
    (JungleStairs,       "//"), (CobblestoneWall,   "//"),
    (Carrots,            "//"), (Potato,            "//"),
    (QuartzBlock,        "//"), (QuartzStairs,      "//"),
    (OakDoubleSlab,      "//"), (OakSlab,           "//"),
    (StainedClay,        "//"), (AcaciaStairs,      "//"),
    (DarkOakStairs,      "//"), (HayBlock,          "//"),
    (Carpet,             "//"), (HardClay,          "//"),
    (CoalBlock,          "//"), (PackedIce,         "//"),
    (Podzol,             "//"), (Beetroot,          "//"),
    (StoneCutter,        "//"), (GlowingObsidian,   "//"),
    (NetherReactor,      "//"), (UpdateGameBE,      "//"),
    (UpdateGameLE,       "//"), (NameBlock,         "//")]
