{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Applicative ( (<$>) )
import Control.Exception (catch)
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

path = "My World/db"

testLoadChunk :: Integer -> Integer -> RecursesIO Chunk
testLoadChunk x z = do
  open path >>= loadChunk x z

testRenderChunk topBlock leftBlock chunkX chunkZ  = do
  -- window related
  (rows, cols) <- lift $ screenSize
  let placeChunk = chunkPlacer topBlock leftBlock (fromIntegral (rows-1)) (fromIntegral cols)

  -- chunk related
  w <- lift $ defaultWindow
  chunk <- open path >>= loadChunk chunkX chunkZ
  lift $ updateWindow w (clearScreen rows cols)
  lift $ updateWindow w $ mapM renderToUpdate $ placeChunk chunk
  lift $ render

clearScreen rows cols = do
  let clearLine row = do
        moveCursor row 0
        drawString $ replicate (fromIntegral cols - (if row == (rows-1) then 1 else 0)) ' '
  setColor defaultColorID
  mapM clearLine (take (fromIntegral rows) [0..])
  moveCursor 0 0

-- The following combines the resource monad needed by LevelDB and the
-- Curses monad.

type RecursesIO a = ResourceT Curses a

runRecurses :: RecursesIO a -> IO a
runRecurses = runResourceT . transResourceT runCurses

runRecursesAndWait action = runRecurses $ do
  w <- lift defaultWindow
  action
  lift $ getEvent w Nothing

instance MonadThrow Curses where
  throwM = throwM

instance MonadBase IO Curses where liftBase = liftIO

-- Now onto the code

{-| Build a function which crops and places any chunk into the right
    part of the screen given by `rows` and `cols`
-}

data RenderInstructions = MoveCursor Integer Integer
                        | DrawString String

instance Show RenderInstructions where
  show (MoveCursor row col) = "moveCursor " ++ show row ++ " " ++ show col
  show (DrawString st) = "drawString " ++ show st

renderToUpdate (MoveCursor row col) = moveCursor row col
renderToUpdate (DrawString st) = drawString st

-- The ncurses package has a bad habit of using Integer instead of Int
-- for rows/columns I will use Int for those when passing them around,
-- but translate to Integer where it makes sense, or where I
-- transition to the ncurses library
chunkPlacer :: Integer -> Integer -> Int -> Int -> Chunk -> [RenderInstructions]
chunkPlacer topBlock leftBlock rows cols =
  let topBlockOffset = -(topBlock `mod` 16)
      leftBlockOffset = -(leftBlock `mod` 16)

      drawChunk Ungenerated{} = []
      drawChunk chunk =
        let nsSpan = fromIntegral rows
            ewSpan = fromIntegral cols
            chunkTopOffset  = north chunk - topBlock
            chunkLeftOffset = leftBlock - east chunk - 15

            dropRows = -(min 0 chunkTopOffset)
            dropCols = -(min 0 chunkLeftOffset)

            -- each line starts at this column
            column = chunkLeftOffset + dropCols

            -- crop a chunk to fit on the screen
            cropChunk chunk =
              let keepRows = min 16 (nsSpan - chunkTopOffset) - dropRows
                  keepCols = min 16 (ewSpan - chunkLeftOffset) - dropCols

                  topLayer = chunkTopLayer chunk

                  sliceCols = take (fromIntegral keepCols) . drop (fromIntegral dropCols)
                  sliceRows = take (fromIntegral keepRows) . drop (fromIntegral dropRows)
              in sliceRows $ map sliceCols topLayer

            -- Draw a single (line number, line) pair
            drawTerrainLine (row, line) =
              [MoveCursor row column,
               DrawString $ concatMap blockAnsi line]

            cropped = cropChunk chunk
            zipped = zip [(chunkTopOffset + dropRows)..] cropped
        in concatMap drawTerrainLine zipped
  in drawChunk

-- Utility functions

slice sDrop sKeep lol =
  take sKeep $ drop sDrop lol

splitN :: Int -> [a] -> [[a]]
splitN n list
  | null list = []
  | otherwise =
    let (line, rest) = splitAt n list
    in line:splitN n rest

chunkBlockType :: Chunk -> [BlockType]
chunkBlockType = map (toEnum . fromIntegral) . B.unpack . terrain

chunkTopLayer :: Chunk -> [[BlockType]]
chunkTopLayer chunk =
  let columns = splitN 128 (chunkBlockType chunk)
      topBlocks = map (head . dropWhile (== Air) . reverse) columns
   in map reverse $ splitN 16 topBlocks

-- Borrowed from original escape-code implementation

blockAnsi bt =
  case M.lookup bt ansiBlock of
    Nothing -> "?"
    Just s -> s

ansiBlock = M.fromList
   [(Air,                " "), (Stone,             "#"),
    (GrassBlock,         "."), (Dirt,              ","),
    (Cobblestone,        "%"), (OakPlank,          "="),
    (Sapling,            "!"), (Bedrock,           "&"),
    (Water,              "~"), (StationaryWater,   "~"),
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
