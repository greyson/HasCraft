module Main where

import Control.Applicative ( (<$>) )
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Database.LevelDB (runResourceT)

import Data.NBT.MCPE
import Database.MCPE
import Data.Minecraft.Entity
import Data.Minecraft.Chunk
import Data.Minecraft.Block
import System.Console.ANSI
import System.Environment

import qualified System.Console.Terminal.Size as Term

-- for interactive
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T

path = "My World/db"

main = getArgs >>= \[x, z] -> topView (read x) (read z)

splitN n list
  | null list = []
  | otherwise =
    let (line, rest) = splitAt n list
    in line:splitN n rest

chunkBlockType :: Chunk -> [BlockType]
chunkBlockType = map (toEnum . fromIntegral) . B.unpack . terrain

chunkTopLayer chunk =
  map (head . dropWhile (== Air) . reverse) $ splitN 128 (chunkBlockType chunk)

topView x z = runResourceT $ do
  db <- open path

  -- Get the size of the screen to which we're rendering
  window <- liftIO $ Term.size
  let (w, h) = case window of
        Nothing -> (80,24) -- standard
        Just win -> (Term.width win, Term.height win)

  -- Collect chunks which will be used on this screen (add one for safety for now)
  let ewChunks = [z `div` 16 -1 .. (z + w) `div` 16 +1]
  let nsChunks = [x `div` 16 -1 .. (x + h) `div` 16 +1]

  chunks <- sequence $ [loadChunk i j db | i <- nsChunks, j <- ewChunks]
  liftIO $ putStr $ concatMap (placeChunk x z w h) chunks ++ setSGRCode []

placeChunk _ _ _ _ Ungenerated = ""
placeChunk x z width height chunk =
  let wLeft = width - west chunk - (z) -16
      wTop  = north chunk - (x)

  in renderChunk wLeft wTop width height chunk

-- Render a chunk starting at the top left indicated; Do not fill
-- beyond the width or height of the window given.

renderChunk :: Int -> Int -> Int -> Int -> Chunk -> String
renderChunk left top width height chunk
  | top  <= (-16) || top > height = ""
  | left <= (-16) || left > width = ""
  | otherwise =
    let right = left + 16
        bottom = top + 16

        xmin = max 0 left
        xmax = min right width
        xs = [xmin .. xmax]
        ymin = max 0 top
        ymax = min height bottom
        ys = [ymin .. ymax]

        padTop = if top < 0    then (-top)    else 0
        padBot = if bottom > height then bottom - height else 0
        padLft = if left < 0   then (-left)   else 0
        padRht = if right > width  then right - width  else 0

        ydrop = padTop
        ykeep = 16 - padTop - padBot

        xdrop = padLft
        xkeep = 16 - padLft - padRht

        -- Reorder the tops to read left -> right (clean cursor loading)
        fullGrid = splitN 16 $ chunkTopLayer chunk
        cropped = take ykeep $ drop ydrop $ map (take xkeep . drop xdrop . reverse) $ fullGrid

        -- Add line numbers
        lined = zip [ymin..] cropped

        -- A helper to get us to the next line position
        nextLine =
          cursorDownLineCode 1 ++
          setCursorColumnCode xmin

        -- finally define our line renderer
        renderLine blks = concat $ map colorize blks

     in setCursorPositionCode ymin xmin ++
        intercalate nextLine (map renderLine cropped)

blockAnsi bt =
  case M.lookup bt ansiBlock of
    Nothing -> setSGRCode [SetColor Background Vivid Red
                          ,SetColor Foreground Vivid White] ++ "?"
    Just s -> s

colorize bt = setSGRCode (hue bt) ++ blockAnsi bt

hue StationaryWater = hue Water
hue Water = [SetColor Background Vivid Blue,
                  SetColor Foreground Vivid White]

hue Cobblestone = hue Stone
hue Stone = [SetColor Background Dull White,
             SetColor Foreground Dull Black]

hue Sand = [SetColor Background Vivid Yellow,
            SetColor Foreground Dull Yellow]

hue GrassBlock = [SetColor Background Dull Green,
                  SetColor Foreground Vivid Green]

hue SugarCane = [SetColor Foreground Vivid Green]

hue bt = []

ansiBlock = M.fromList
   [(Air,                " "), (Stone,             "#"),
    (GrassBlock,         " "), (Dirt,              ","),
    (Cobblestone,        "%"), (OakPlank,          "="),
    (Sapling,            "!"), (Bedrock,           "&"),
    (Water,              " "), (StationaryWater,   "~"),
    (Lava,               "x"), (StationaryLava ,   "X"),
    (Sand,               "o"), (Gravel,            ":"),
    (GoldOre,            "$"), (IronOre,           "@"),
    (CoalOre,            "b"), (Wood,              "|"),
    (Leaves,             "^"), (Sponge,            "¶"),
    (Glass,              "O"), (LapisOre,          "B"),
    (LapisBlock,         "/"), (Sandstone,         "/"),
    (Bed,                "/"), (PoweredRail,       "/"),
    (Cobweb,             "/"), (TallGrass,         "/"),
    (DeadBush,           "/"), (Wool,              "/"),
    (Dandelion,          "/"), (Poppy,             "/"),
    (BrownMushroom,      "/"), (RedMushroom,       "/"),
    (GoldBlock,          "/"), (IronBlock,         "/"),
    (DoubleStoneSlab,    "/"), (StoneSlab,         "/"),
    (BrickBlock,         "/"), (TNT,               "/"),
    (Bookshelf,          "/"), (MossStone,         "/"),
    (Obsidian,           "/"), (Torch,             "/"),
    (Fire,               "/"), (MonsterSpawner,    "/"),
    (OakStairs,          "/"), (Chest,             "/"),
    (DiamondOre,         "/"), (DiamondBlock,      "/"),
    (CraftingTable,      "/"), (Seeds,             "/"),
    (Farmland,           "/"), (Furnace,           "/"),
    (BurningFurnace,     "/"), (SignPost,          "/"),
    (OakDoor,            "/"), (Ladder,            "/"),
    (Rail,               "/"), (CobblestoneStairs, "/"),
    (WallSign,           "/"), (IronDoor,          "/"),
    (RedstoneOre,        "/"), (GlowingRedstoneOre,"/"),
    (SnowCover,          "/"), (Ice,               "/"),
    (Snow,               "/"), (Cactus,            "/"),
    (Clay,               "/"), (SugarCane,         "|"),
    (Fence,              "/"), (Pumpkin,           "/"),
    (Netherrack,         "/"), (Glowstone,         "/"),
    (JackOLantern,       "/"), (CakeBlock,         "/"),
    (InvisibleBedrock,   "/"), (Trapdoor,          "/"),
    (StoneBrick,         "/"), (HugeBrownMushroom, "/"),
    (HugeRedMushroom,    "/"), (IronBars,          "/"),
    (GlassPane,          "/"), (Melon,             "/"),
    (PumpkinStem,        "/"), (MelonStem,         "/"),
    (Vines,              "/"), (FenceGate,         "/"),
    (BrickStairs,        "/"), (StoneBrickStairs,  "/"),
    (Mycellium,          "/"), (LilyPad,           "/"),
    (NetherBrick,        "/"), (NetherBrickStairs, "/"),
    (EndPortalFrame,     "/"), (EndStone,          "/"),
    (Cocoa,              "/"), (SandstoneStairs,   "/"),
    (EmeraldOre,         "/"), (EmeraldBlock,      "/"),
    (SpruceStairs,       "/"), (BirchStairs,       "/"),
    (JungleStairs,       "/"), (CobblestoneWall,   "/"),
    (Carrots,            "/"), (Potato,            "/"),
    (QuartzBlock,        "/"), (QuartzStairs,      "/"),
    (OakDoubleSlab,      "/"), (OakSlab,           "/"),
    (StainedClay,        "/"), (AcaciaStairs,      "/"),
    (DarkOakStairs,      "/"), (HayBlock,          "/"),
    (Carpet,             "/"), (HardClay,          "/"),
    (CoalBlock,          "/"), (PackedIce,         "/"),
    (Podzol,             "/"), (Beetroot,          "/"),
    (StoneCutter,        "/"), (GlowingObsidian,   "/"),
    (NetherReactor,      "/"), (UpdateGameBE,      "/"),
    (UpdateGameLE,       "/"), (NameBlock,         "/")]
