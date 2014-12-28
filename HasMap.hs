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

main = getArgs >>= \[x, z] ->
  runResourceT $ transResourceT runCurses $ test (read x) (read z)

instance MonadThrow Curses where
  throwM = throwM

instance MonadBase IO Curses where liftBase = liftIO

path = "My World/db"

getTopLayer x z = do
  db <- open path
  splitN 16 . chunkTopLayer <$> loadChunk x z db

runRecurses = runResourceT . transResourceT runCurses

testLoadChunk :: Int -> Int -> ResourceT Curses Chunk
testLoadChunk x z = do
  open path >>= loadChunk x z


test :: Int -> Int -> ResourceT Curses ()
test topBlock leftBlock = do
  db <- open path

  w <- lift defaultWindow
  (screenRows, screenCols) <- lift screenSize

  liftIO $ putStrLn ("Screen is " ++ show screenCols ++ "x" ++ show screenRows)

  let rows = fromIntegral $ screenRows
      cols = fromIntegral $ screenCols

      ewBlockWindow = fromIntegral $ (screenCols `div` 2)
      nsBlockWindow = fromIntegral $ (screenRows `div` 2)

      bottomBlock = topBlock  + (nsBlockWindow -1)
      rightBlock  = leftBlock - (ewBlockWindow -1)

      topChunk    = topBlock `div` 16
      rightChunk  = (rightBlock + 15) `div` 16
      bottomChunk = bottomBlock `div` 16
      leftChunk   = leftBlock `div` 16

      topBlockOffset = -(topBlock `mod` 16)
      leftBlockOffset = -((leftBlock +1) `mod` 16)

      nsChunks = zip [0..] [topChunk..bottomChunk]
      ewChunks = zip [0..] $ reverse [rightChunk..leftChunk]

      nsChunksWOff = map (\(n, ch) -> (16*n + topBlockOffset, ch)) nsChunks
      ewChunksWOff = map (\(n, ch) -> (16*n + leftBlockOffset, ch)) ewChunks

      chunks = [(i, j) | i <- nsChunksWOff, j <- ewChunksWOff]

  sequence $ map (makeChunkRender rows cols db w) chunks
  lift render
  lift $ getEvent w Nothing
  return ()

{-| Build a function which crops and places any chunk into the right
    part of the screen given by `rows` and `cols`
-}

chunkPlacer topBlock leftBlock rows cols =
  let ewBlockWindow = fromIntegral $ (cols `div` 2)
      nsBlockWindow = fromIntegral $ (rows `div` 2)

      topBlockOffset = -(topBlock `mod` 16)
      leftBlockOffset = -(leftBlock `mod` 16)

      drawChunk Ungenerated{} = "UNGENERATED"
      drawChunk chunk =
        let chunkTopOffset  = north chunk - topBlock
            chunkLeftOffset = leftBlock - east chunk - 15

            dropRows = -(min 0 chunkTopOffset)
            dropCols = -(min 0 chunkLeftOffset)

            startColumn = chunkLeftOffset + dropCols

            -- crop a chunk to fit on the screen
            cropChunk chunk =
              let keepRows = min 16 (rows - chunkTopOffset) - dropRows
                  keepCols = min 16 (cols - chunkLeftOffset) - dropCols

                  topLayer = chunkTopLayer chunk
              in slice dropRows keepRows $ map (slice dropCols keepCols) topLayer

            -- Draw a single (line number, line) pair
            drawTerrainLine (row, line) =
              ("@(col " ++ show startColumn ++ ", row " ++ show row ++ ")"
               ++ concatMap blockAnsi line)

            cropped = cropChunk chunk
            zipped = zip [(chunkTopOffset + dropRows)..] cropped
        in unlines (map drawTerrainLine zipped)
  in drawChunk

placeChunk' :: Int -> Int -> Int -> Int -> Chunk -> Update ()
placeChunk' _ _ _ _ Ungenerated {} = return ()
placeChunk' x z width height chunk =
  let wLeft = width - (west chunk) - z - 16
      wTop  = north chunk - x
   in renderChunk wLeft wTop width height chunk

renderChunk :: Int -> Int -> Int -> Int -> Chunk -> Update ()
renderChunk left top width height chunk
  | top  <= (-16) || top > height = return ()
  | left <= (-16) || left > width = return ()
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

        -- Crop the chunk to fit on screen
        cropped = take ykeep $ drop ydrop $ map (take xkeep . drop xdrop) $ chunkTopLayer chunk

        -- Add line numbers
        lined = zip [ymin..] cropped

        -- define our line renderer
        renderLine (lineNo, blks) = renderLineAt (fromIntegral lineNo) (fromIntegral xmin) blks

     in sequence (map renderLine lined) >> return ()

renderBlock blk = drawString $ blockAnsi blk

renderLineAt :: Integer -> Integer -> [BlockType] -> Update [()]
renderLineAt row col blks = do
  moveCursor row col
  sequence $ map renderBlock blks

renderChunkAt' :: Int -> Int -> [[BlockType]] -> Update ()
renderChunkAt' x y layer = do
    setColor defaultColorID
    moveCursor (fromIntegral x) (fromIntegral y)

    -- here is where we magnify the x again
    let drawBlockLine (no, line) =
          moveCursor no (fromIntegral (x *2)) >>
          drawString (concatMap blockAnsi line)

    sequence $ map drawBlockLine (zip [(fromIntegral y)..] layer)
    return ()


-- Utility functions

makeChunkRender rows cols db w ((nsOff, nsChunk), (ewOff, ewChunk)) =
  let rowDrop = -(min 0 nsOff)
      rowKeep = min 16 (rows - nsOff) - rowDrop
      colDrop = -(min 0 ewOff)
      colKeep = min 16 (cols - ewOff) - colDrop

      rowPos = max 0 nsOff
      colPos = max 0 ewOff

      doUpdate (line, blks) = do
        moveCursor (fromIntegral line) (fromIntegral colPos)
        drawString $ concatMap blockAnsi (slice colDrop colKeep blks)
   in do
    layerWLines <- (slice rowDrop rowKeep . zip [nsOff..] . chunkTopLayer) <$>
                   loadChunk nsChunk ewChunk db
    lift $ updateWindow w $ sequence $ map doUpdate layerWLines

slice sDrop sKeep lol =
  take sKeep $ drop sDrop lol

chunkMapping topBlock leftBlock screenRows screenCols =
  let ewBlockWindow = fromIntegral $ screenCols
      nsBlockWindow = fromIntegral $ screenRows

      bottomBlock = topBlock  + nsBlockWindow
      rightBlock   = leftBlock - ewBlockWindow

      topChunk    = (topBlock -15) `quot` 16
      rightChunk  = (rightBlock -15) `quot` 16
      bottomChunk = (bottomBlock +15) `quot` 16
      leftChunk   = (leftBlock +15) `quot` 16

      topBlockOffset = topBlock `rem` 16
      rightBlockOffset = rightBlock `rem` 16
      leftBlockOffset = (leftBlock -15) `rem` 16
      bottomBlockOffset = (bottomBlock -15) `rem` 16

   in 42

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
