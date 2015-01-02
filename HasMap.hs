{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
import Control.Applicative ( (<$>) )
import Control.Exception (catch)
import Control.Monad (liftM, ap, void)
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource
import Data.Maybe (fromJust)
import UI.NCurses
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Control.Applicative as A
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Pocketmine.NBT (readDat, (</>), NBTPayload(..))
import Data.Pocketmine.Block
import Data.Pocketmine.Chunk
import Database.Pocketmine

data AppState = AppState { worldX :: Integer
                         , worldZ :: Integer
                         , database :: DB
                         }

centerCoordinatesFromDat worldDat resolution =
  let IntTag x = fromJust $ worldDat </> "SpawnX"
      IntTag z = fromJust $ worldDat </> "SpawnZ"
  in centerCoordinates (fromIntegral x) (fromIntegral z) resolution

centerCoordinates :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
centerCoordinates x z (rows, cols) =
  (x - (cols `div` 4), z + (rows `div` 2))

main = runRecurses $ do
  (world:args) <- liftIO getArgs
  worldDat <- readDat <$> liftIO (BL.readFile (world ++ "/level.dat"))

  (x, z) <- lift screenSize >>= case args of
    [] -> return . centerCoordinatesFromDat worldDat
    [sX, sZ] -> return . centerCoordinates (read sX) (read sZ)

  db <- open (world ++ "/db")

  mainLoop AppState { worldX = x
                    , worldZ = z
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
  state { worldX = worldX state + 5 }
handleEvent state (EventSpecialKey KeyUpArrow) =
  state { worldX = worldX state - 5 }
handleEvent state (EventSpecialKey KeyRightArrow) =
  state { worldZ = worldZ state - 5 }
handleEvent state (EventSpecialKey KeyLeftArrow) =
  state { worldZ = worldZ state + 5 }
handleEvent state _ = state

renderScreen topBlock leftBlock db = do
  -- window related
  (allRows, cols) <- lift $ screenSize
  let rows = allRows -1
  painter <- lift $ makePainter

  -- chunk preparation
  let placeChunk = chunkPlacer painter topBlock leftBlock rows cols
      chunksCoords = visibleChunks topBlock leftBlock rows cols

  let loadChunkCoord db (x, z) = loadChunk x z db

  -- chunk related
  w <- lift $ defaultWindow
  chunks <- sequence (map (loadChunkCoord db) chunksCoords)
  lift $ updateWindow w $ mapM placeChunk chunks
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

emptyLayer = replicate 16 (replicate 16 Air)

{-| Build a function which crops and places any chunk into the right
    part of the screen given by `rows` and `cols`
-}

chunkPlacer :: Painter -> Integer -> Integer -> Integer -> Integer -> Chunk -> Update [()]
chunkPlacer painter topBlock leftBlock rows cols =
  let topBlockOffset = -(topBlock `mod` 16)
      leftBlockOffset = -(leftBlock `mod` 16)

      nsSpan = rows
      ewSpan = cols `div` 2

      drawChunk chunk =
        let chunkTopOffset  = north chunk - topBlock
            chunkLeftOffset = leftBlock - east chunk - 15

            dropRows = -(min 0 chunkTopOffset)
            dropCols = -(min 0 chunkLeftOffset)

            -- each line starts at this column
            column = (chunkLeftOffset + dropCols) * 2

            -- crop a chunk to fit on the screen
            keepRows = min 16 (nsSpan - chunkTopOffset) - dropRows
            keepCols = min 16 (ewSpan - chunkLeftOffset) - dropCols

            topLayer = case chunk of
                            Ungenerated{} -> emptyLayer
                            other -> chunkTopLayer chunk

            sliceCols = take (fromIntegral keepCols) . drop (fromIntegral dropCols)
            sliceRows = take (fromIntegral keepRows) . drop (fromIntegral dropRows)
            cropped = sliceRows $ map sliceCols topLayer

            zipped = zip [(chunkTopOffset + dropRows)..] cropped

            drawBlock block = do
              painter block

            -- Draw a single (line number, line) pair
            drawTerrainLine (row, line) = do
              moveCursor row column
              void $ mapM drawBlock line
        in sequence $ map drawTerrainLine zipped
  in drawChunk

visibleChunks topBlock leftBlock rows cols =
  let ewBlocks = fromIntegral cols `div` 2
      nsBlocks = fromIntegral rows
      bottomBlock = topBlock + (nsBlocks -1)
      rightBlock = leftBlock - (ewBlocks -1)

      topChunk = topBlock `div` 16
      rightChunk = rightBlock `div` 16
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

chunkTopLayer :: Chunk -> [[BlockType]]
chunkTopLayer chunk =
  map reverse $ splitN 16 $ map chunkTopLayer' $ splitNBS 128 $ terrain chunk

air = fromIntegral (fromEnum Air)

chunkTopLayer' :: Terrain -> BlockType
chunkTopLayer' bs =
  toEnum $ fromIntegral $ B.head $ B.dropWhile (== air) $ B.reverse bs

-- Borrowed from original escape-code implementation

type Painter = BlockType -> Update ()

makePainter :: Curses (BlockType -> Update ())
makePainter = do
  let colorEntries (id, ((f, b), blocks)) = do
        cid <- newColorID f b id
        return $ map (\(bt, c, attr) -> (bt, (cid, c, attr))) blocks
  table <- (M.fromList . concat) <$> (mapM colorEntries (zip [1..] colors))

  let applyColor (Unknown x) = do
        setColor defaultColorID
        setAttributes []
        drawString $ printf "%02x" x
      applyColor bt = do
        let (colorid, c, atr) = maybe (defaultColorID, '?', []) id $ M.lookup bt table
        setColor colorid
        setAttributes atr
        drawString [c, c]
  return applyColor

-- ((foreground, background), [(blocktype, [attributes])]
colors =
  [ ((ColorYellow, ColorRed),    [(NameBlock, '?', [AttributeBold]) ])
  , ((ColorBlack, ColorBlack),   [(Air, ' ', []) ])
  , ((ColorBlack, ColorWhite),   [(Stone, ' ', [])
                                 ,(Gravel, ':', [])
                                 ,(Cobblestone, '%', []) ])
  , ((ColorGreen, ColorGreen),   [(GrassBlock, ' ', [AttributeBold, AttributeReverse])
                                 ,(Leaves, '^', [AttributeBold, AttributeReverse])
                                 ,(SugarCane, '|', [AttributeBold, AttributeReverse])
                                 ,(TallGrass, '/', [AttributeBold, AttributeReverse])
                                 ,(LargeFern, 'Y', [AttributeBold, AttributeReverse]) ])
  , ((ColorBlue, ColorBlue),     [(Water, '~', [AttributeBold])
                                 ,(StationaryWater, ' ', []) ])
  , ((ColorBlack, ColorYellow),  [(Dirt, ' ', [AttributeBold])
                                 ,(Torch, '¡', [AttributeReverse, AttributeBold])
                                 ,(OakPlank, '=', [AttributeBold]) ])
  , ((ColorYellow, ColorYellow), [(Sand, ' ', [AttributeReverse, AttributeBold])
                                 ,(Pumpkin, '☺', [AttributeReverse, AttributeBold]) ])
  , ((ColorYellow, ColorRed),    [(Lava, '~', [])
                                 ,(StationaryLava, ' ', []) ])
  , ((ColorWhite, ColorBlack),   [(SnowCover, ' ', [AttributeBold, AttributeReverse])
                                 ,(Snow, '#', [AttributeBold, AttributeReverse]) ])
  ]

ansiBlock = M.fromList
   [(Sapling,            '!'), (Bedrock,            '&'),
    (GoldOre,            '$'), (IronOre,            '@'),
    (CoalOre,            'b'), (Wood,               '|'),
    (Sponge,             '¶'),
    (Glass,              'O'), (LapisOre,           'B'),
    (LapisBlock,         '/'), (Sandstone,          '/'),
    (Bed,                '/'), (PoweredRail,        '/'),
    (Cobweb,             '/'),
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
    (Clay,               '/'), (SugarCane,          '|'),
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
