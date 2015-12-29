module LoadImage where

import Shifts

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

{- WindowSize width height -}
data WindowSize = WindowSize Int Int

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
                    extractName ref >>= setClipboard -- Copy its name to buffer
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

{-Loads specified file to specified Image widged-}
loadImage :: Image -> FilePath -> IO ()
loadImage imageWidget fileToDisplay = do
    isDirectory <- doesDirectoryExist fileToDisplay
    if ( isDirectory == True )
    then
        error $ fileToDisplay ++ " is a directory, skipping"
    else do
        loadedAnimation <- pixbufAnimationNewFromFile fileToDisplay
        isStaticImage <- pixbufAnimationIsStaticImage loadedAnimation
        if ( isStaticImage == True )
        then do
            {- Reload image as pixbuf. Yes, it's ineffective -}
            WindowSize windowWidth windowHeight <- getWindowSize imageWidget
            loadedPixbuf <- pixbufNewFromFileAtSize
                                fileToDisplay
                                windowWidth
                                windowHeight
            imageSetFromPixbuf imageWidget loadedPixbuf
        else do
            imageSetFromAnimation imageWidget loadedAnimation

{- get size of window hosting image widget. Non-general -}
getWindowSize :: Image -> IO WindowSize
getWindowSize imageWidget = do
    justOverlay <- widgetGetParent imageWidget
    let overlay = fromMaybe
            ( error "This error should not be here #1" )
            justOverlay
    justWindow <- widgetGetParent overlay
    let window = fromMaybe
            ( error "This error should not be here #2" )
            justWindow
    ( windowWidth , windowHeight ) <- windowGetSize ( castToWindow window )
    return $ WindowSize windowWidth windowHeight

{-Constructs list of files-}
filesFromArgs :: IO [ FilePath ]
filesFromArgs = do
    args <- getArgs
    if ( args == [] )
    then do
        filesList <- getCurrentDirectory >>= getDirectoryContents
        let cleanup = delete "." . delete ".."
        return $ cleanup filesList
    else do
        processSingleFile args

processSingleFile :: [ FilePath ] -> IO [ FilePath ]
processSingleFile [ singleFile ] = do
    que <- doesDirectoryExist singleFile
    if ( que == True )
    then do
        dirContents <- getDirectoryContents singleFile
        let dirName = FS.decodeString singleFile
        let cleanup = delete "." . delete ".."
        let filePaths = map ( FS.encodeString . FS.append dirName . FS.decodeString )
                            ( cleanup dirContents )
        processSingleFile filePaths
    else
        return [ singleFile ]
processSingleFile manyFiles = return manyFiles

{- Initialises Position IORef from scratch -}
initFileList :: IO ( IORef Position )
initFileList = do
    rawFileList <- filesFromArgs
    randomGen   <- newStdGen
    let sortedFileList = sort rawFileList
    let filesNumber    = length sortedFileList
    let randomIndices  = shuffle' ( take filesNumber [ 0 .. ] )
                                    filesNumber
                                    randomGen
    newIORef $ Position { files = sortedFileList
                        , ix_shuffle = randomIndices
                        , ix_pos = head randomIndices
                        , ix_rand = 0
                        , mask = take filesNumber $ repeat True
                        }

{- Extract name of current file from IORef -}
extractName :: IORef Position -> IO String
extractName iorefPosition = do
    currentDir   <- getCurrentDirectory
    position     <- readIORef iorefPosition
    let fileList  = files position
    let safeIndex = mod ( ix_pos position ) ( length fileList )
    let fileName  = fileList !! safeIndex
    return $ FS.encodeString $ FS.append ( FS.decodeString currentDir )
                                         ( FS.decodeString fileName )
