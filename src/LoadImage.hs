module LoadImage where

{- BEGIN Dependencies -}

--Error handling
import Control.Exception ( catch
                         , SomeException
                         )

import Control.Concurrent ( forkIO )
import Control.Monad ( when )

--State variables
import Data.IORef

--GTK bindings
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.PixbufAnimation

--System buffer, for yanking filename
import System.Hclip ( setClipboard )


import System.Environment (getArgs)

--Working with filesystem
import System.Directory ( getCurrentDirectory
                        , getDirectoryContents
                        , doesFileExist
                        , doesDirectoryExist
                        )

import qualified Filesystem.Path as FS
import qualified Filesystem.Path.CurrentOS as FS

--Some list processing
import Data.List ( sort, delete )
import Data.Maybe ( fromMaybe )

--Random shuffle
import System.Random
import System.Random.Shuffle ( shuffle' )

{- END Dependencies -}

data Position = Position 
    { files :: [ FilePath ] -- List of files
    , ix_shuffle :: [ Int ] -- Shuffle of indexes
    , ix_pos :: Int         -- Index of file to display. Refers to `files`
    , ix_rand :: Int        -- Current position in random list. Refers to `ix_shuffle`
    , mask :: [ Bool ]      -- Mask of file correctness
    }

data WSize = WSize Int Int

{- Function which determines which file will be next -}
type Shift = Position -> Position


{- Handler of resize event -}
{- If image is still, it will be ineffectively resized, animation will be left to default handler -}
redrawImage :: Image -> IORef Position -> IO Bool 
redrawImage img ref = do
    imageType <- get img imageStorageType
    dynResize imageType
  where dynResize ImagePixbuf = do -- If image is still, process it by yourself
            forkIO $ postGUIAsync $ nextImg id ref img
            return True
        dynResize _ = do -- Leave other types of content to default handler
            return False
            

nextImg :: Shift -> IORef Position -> Image -> IO ()
nextImg shift ref img = do
    p <- readIORef ref
    let pos @ Position { files = fs
                       , ix_shuffle = ixs
                       , ix_pos = ixp
                       , mask = m } = shift p
    if (or m) -- If there is any correct file
    then do
        writeIORef ref pos -- Update position
        let qq = (mod ixp $ length fs )
        if (m !! qq == True) -- If new requested file is correct too
        then do
            let ff = fs !! qq
            catch ( loadImage img ff )
                  ( \e -> do
                    print ( e :: SomeException )
                    uncheck ref -- Mark file as incorrect
                    nameCopy ref >>= setClipboard -- Copy its name to buffer
                    nextImg shift ref img -- Try to load next image with same method
                  )
        else do
            nextImg shift ref img -- Just load next file
    else do
        putStrLn "No files in the list"

{- Marks correctness mask of current file (by index) as False -}
uncheck :: IORef Position -> IO ()
uncheck p = do
    pp <- readIORef p
    let Position fs _ ix _ m = pp
    let ix2 = mod ix (length fs)
    let (a,b) = splitAt ix2 m
    let newMask = a ++ False : ( tail b )
    writeIORef p ( pp { mask = newMask } )

nextRan :: Shift
nextRan pos = pos { ix_rand = foo
                  , ix_pos = ix_shuffle pos !! safeIndex
                  }
    where foo = ( ix_rand pos ) + 1
          safeIndex = ( mod foo . length . files $ pos )

prevRan :: Shift
prevRan pos = pos { ix_rand = foo
                  , ix_pos = ix_shuffle pos !! safeIndex
                  }
        where foo = ( ix_rand pos ) - 1
              safeIndex = ( mod foo . length . files $ pos )

nextSeq :: Shift
nextSeq pos = pos { ix_pos = ( ix_pos pos ) + 1 }

prevSeq :: Shift
prevSeq pos = pos { ix_pos = ( ix_pos pos ) - 1 }


{-Loads specified file to specified Image widged-}
loadImage :: Image -> FilePath -> IO ()
loadImage w f = do
    due <- doesDirectoryExist f
    if (due)
    then
        error $ f ++ " is a directory, skipping"
    else do
        a <- pixbufAnimationNewFromFile f
        que <- pixbufAnimationIsStaticImage a
        if (que)
        then do
            WSize ww wh <- getWinSize w
            newP <- pixbufNewFromFileAtSize f ww wh
            imageSetFromPixbuf w newP
        else do
            imageSetFromAnimation w a

getWinSize :: Image -> IO WSize
getWinSize w = do
    pre_ol <- widgetGetParent w
    let ol = fromMaybe
            (error "This error should not be here #1")
            pre_ol
    pre_win <- widgetGetParent ol
    let win = fromMaybe
            (error "This error should not be here #2")
            pre_win
    (w,h) <- windowGetSize (castToWindow win)
    return $ WSize w h

{-Constructs list of files-}
fileList :: IO [ FilePath ]
fileList = do
    args <- getArgs
    if ( length args == 0 )
    then do
        r <- getCurrentDirectory >>= getDirectoryContents
        let cleanup = delete "." . delete ".."
        return $ cleanup r
    else do
        getCont args

getCont :: [ FilePath ] -> IO [ FilePath ]
getCont [f] = do
    q <- doesDirectoryExist f
    if q
    then do
        c <- getDirectoryContents f
        let ff = FS.decodeString f
        let cleanup = delete "." . delete ".."
        let cc = map ( FS.encodeString . FS.append ff . FS.decodeString )
                     ( cleanup c )
        getCont $ cleanup cc
    else
        return [f]
getCont other = return other

initFileList :: IO ( IORef Position )
initFileList = do
    files' <- fileList
    let files = sort files'
    g <- newStdGen
    let l = length files
    let s = shuffle' [0..(l-1)] l g
    let h = head s
    newIORef $ Position files s h 0 (take l $ repeat True)

nameCopy :: IORef Position -> IO String
nameCopy p = do
    dir <- getCurrentDirectory
    pp <- readIORef p
    let i = mod (ix_pos pp) (length $ files pp)
    let f = files pp !! i
    return $ FS.encodeString $ FS.append (FS.decodeString dir) (FS.decodeString f)
