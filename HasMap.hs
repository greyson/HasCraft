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

data AppState = AppState { worldX :: Integer
                         , worldZ :: Integer
                         , database :: DB
                         }

main = runRecurses $ do
  [x, z] <- liftIO getArgs
  db <- open path

  mainLoop AppState { worldX = read x
                    , worldZ = read z
                    , database = db
                    }

mainLoop (state@AppState { worldX = x, worldZ = z, database = db }) = do
  renderScreen x z db
  w <- lift $ defaultWindow
  ev <- lift $ getEvent w Nothing
  case ev of
   Just (EventCharacter 'q') -> return ()
   Just ev -> mainLoop (handleEvent state ev)
   Nothing -> mainLoop state

handleEvent state (EventSpecialKey KeyDownArrow) =
  state { worldX = worldX state + 1 }
handleEvent state (EventSpecialKey KeyUpArrow) =
  state { worldX = worldX state - 1 }
handleEvent state (EventSpecialKey KeyRightArrow) =
  state { worldZ = worldZ state - 1 }
handleEvent state (EventSpecialKey KeyLeftArrow) =
  state { worldZ = worldZ state + 1 }
handleEvent state _ = state

renderScreen topBlock leftBlock db = do
  -- window related
  (allRows, cols) <- lift $ screenSize
  let rows = allRows -1

  -- chunk preparation
  let placeChunk = chunkPlacer topBlock leftBlock (fromIntegral rows) (fromIntegral cols)
      chunksCoords = visibleChunks topBlock leftBlock rows cols

  let loadChunkCoord db (x, z) = loadChunk x z db

  -- chunk related
  w <- lift $ defaultWindow
  chunks <- sequence (map (loadChunkCoord db) chunksCoords)
  let renderInstructions = concat $ map placeChunk chunks
  lift $ updateWindow w $ mapM renderToUpdate $ renderInstructions
  lift $ render

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

data RenderInstruction = MoveCursor Integer Integer
                       | DrawString String

instance Show RenderInstruction where
  show (MoveCursor row col) = "moveCursor " ++ show row ++ " " ++ show col
  show (DrawString st) = "drawString " ++ show st

renderToUpdate (MoveCursor row col) = moveCursor row col
renderToUpdate (DrawString st) = drawString st

-- The ncurses package has a bad habit of using Integer instead of Int
-- for rows/columns I will use Int for those when passing them around,
-- but translate to Integer where it makes sense, or where I
-- transition to the ncurses library
chunkPlacer :: Integer -> Integer -> Int -> Int -> Chunk -> [RenderInstruction]
chunkPlacer topBlock leftBlock rows cols =
  let topBlockOffset = -(topBlock `mod` 16)
      leftBlockOffset = -(leftBlock `mod` 16)

      nsSpan = fromIntegral rows
      ewSpan = fromIntegral cols

      drawChunk Ungenerated{} = []
      drawChunk chunk =
        let chunkTopOffset  = north chunk - topBlock
            chunkLeftOffset = leftBlock - east chunk - 15

            dropRows = -(min 0 chunkTopOffset)
            dropCols = -(min 0 chunkLeftOffset)

            -- each line starts at this column
            column = chunkLeftOffset + dropCols

            -- crop a chunk to fit on the screen
            keepRows = min 16 (nsSpan - chunkTopOffset) - dropRows
            keepCols = min 16 (ewSpan - chunkLeftOffset) - dropCols

            topLayer = chunkTopLayer chunk

            sliceCols = take (fromIntegral keepCols) . drop (fromIntegral dropCols)
            sliceRows = take (fromIntegral keepRows) . drop (fromIntegral dropRows)
            cropped = sliceRows $ map sliceCols topLayer

            zipped = zip [(chunkTopOffset + dropRows)..] cropped

            -- Draw a single (line number, line) pair
            drawTerrainLine (row, line) =
              [ MoveCursor row column
              , DrawString $ concatMap blockAnsi line
              ]
        in concatMap drawTerrainLine zipped
  in drawChunk

visibleChunks topBlock leftBlock rows cols =
  let ewBlocks = fromIntegral cols
      nsBlocks = fromIntegral rows
      bottomBlock = topBlock + (nsBlocks -1)
      rightBlock = leftBlock - (ewBlocks -1)

      topChunk = topBlock `div` 16
      rightChunk = (rightBlock) `div` 16
      bottomChunk = bottomBlock `div` 16
      leftChunk = leftBlock `div` 16

      nsChunks = [topChunk..bottomChunk]
      ewChunks = [rightChunk..leftChunk]

  in [(x,z) | x <- nsChunks, z <- ewChunks]

-- Utility functions

slice sDrop sKeep lol =
  take sKeep $ drop sDrop lol

splitN :: Int -> [a] -> [[a]]
splitN n list
  | null list = []
  | otherwise =
    let (line, rest) = splitAt n list
    in line:splitN n rest

splitNBS :: Int -> Terrain -> [Terrain]
splitNBS n bs
  | B.null bs = []
  | otherwise =
      let (line, rest) = B.splitAt n bs
      in line:splitNBS n rest

chunkBlockType :: Chunk -> [BlockType]
chunkBlockType = map (toEnum . fromIntegral) . B.unpack . terrain

chunkTopLayer :: Chunk -> [[BlockType]]
chunkTopLayer chunk =
  map reverse $ splitN 16 $ map chunkTopLayer' $ splitNBS 128 $ terrain chunk

air = fromIntegral (fromEnum Air)

chunkTopLayer' :: Terrain -> BlockType
chunkTopLayer' bs =
  toEnum $ fromIntegral $ B.head $ B.dropWhile (== air) $ B.reverse bs

clearScreen rows cols = do
  let clearLine row = do
        moveCursor row 0
        drawString $ replicate (fromIntegral cols - (if row == (rows-1) then 1 else 0)) ' '
  setColor defaultColorID
  mapM clearLine (take (fromIntegral rows) [0..])
  moveCursor 0 0

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
