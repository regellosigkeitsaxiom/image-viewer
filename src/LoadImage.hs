module LoadImage where

import Shifts

--Error handling
import Control.Exception ( catch
                         , SomeException
                         )

import Control.Concurrent ( forkIO )
import Control.Monad ( when )

--State variables
import Data.IORef ( IORef
                  , readIORef
                  , atomicWriteIORef
                  )

--GTK bindings
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.PixbufAnimation
import Graphics.UI.Gtk.Gdk.Pixbuf

--System buffer, for yanking filename
import System.Hclip ( setClipboard )

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

{- WindowSize width height -}
data WindowSize = WindowSize Int Int
data Zoom = ZoomFit | ZoomFill | ZoomWidth Int
instance Show Zoom where
  show ZoomFit = "Fit"
  show ZoomFill = "Fill"
  show ( ZoomWidth x ) = "Width (+" ++ show x ++ ")"

nextZoom :: Zoom -> Zoom
nextZoom ZoomFill = ZoomFit
nextZoom ZoomFit = ZoomWidth 0
nextZoom (ZoomWidth _) = ZoomFill

{- Handler of resize event -}
{- If image is still, it will be ineffectively resized, animation will be left to default handler -}
redrawImage :: Image -> IORef Position -> IORef Zoom -> IO Bool
redrawImage imageWidget iorefPosition zoom = do
    imageType <- get imageWidget imageStorageType
    dynamicResize imageType
  where dynamicResize ImagePixbuf = do
            forkIO $ postGUIAsync $ nextImage id iorefPosition imageWidget zoom
            return True
        dynamicResize _ = do
            return False

{- This one needs to be split -}
nextImage :: Shift -> IORef Position -> Image -> IORef Zoom -> IO ()
nextImage shift iorefPosition imageWidget zoom = do
    position <- readIORef iorefPosition
    let newPosition = shift position
    if ( or $ mask newPosition ) -- If there is any correct file
    then do
        atomicWriteIORef iorefPosition newPosition -- Update position
        guardedLoadImage shift iorefPosition imageWidget zoom
    else do
        putStrLn "No files in the list"

guardedLoadImage :: Shift -> IORef Position -> Image -> IORef Zoom -> IO ()
guardedLoadImage shift iorefPosition imageWidget zoom = do
    position <- readIORef iorefPosition
    let safeIndex = mod ( ix_pos position ) ( length $ files position )
    let fileToLoad = files position !! safeIndex
    if ( mask position !! safeIndex == False )
    then do
        nextImage shift iorefPosition imageWidget zoom
    else do
        catch ( loadImage imageWidget fileToLoad zoom )
              ( \e -> do
                print ( e :: SomeException )
                uncheck iorefPosition
                extractFullName iorefPosition >>= setClipboard
                nextImage shift iorefPosition imageWidget zoom
              )

{- Marks correctness mask of current file (by index) as False -}
uncheck :: IORef Position -> IO ()
uncheck iorefPosition = do
    position <- readIORef iorefPosition
    --let Position fs _ ix _ m = pp
    let safeIndex = mod ( ix_pos position ) ( length $ files position )
    {- Replacing target element with False -}
    let (a,b) = splitAt safeIndex ( mask position )
    let newMask = a ++ False : ( tail b )
    atomicWriteIORef iorefPosition
               position { mask = newMask }

{-Loads specified file to specified Image widged-}
loadImage :: Image -> FilePath -> IORef Zoom -> IO ()
loadImage imageWidget fileToDisplay zoom = do
    isDirectory <- doesDirectoryExist fileToDisplay
    if ( isDirectory == True )
    then
        error $ fileToDisplay ++ " is a directory, skipping"
    else do
        loadCorrectImage imageWidget fileToDisplay zoom

loadCorrectImage :: Image -> FilePath -> IORef Zoom -> IO ()
loadCorrectImage imageWidget fileToDisplay zoom = do
    loadedAnimation <- pixbufAnimationNewFromFile fileToDisplay
    isStaticImage <- pixbufAnimationIsStaticImage loadedAnimation
    if ( isStaticImage == False )
    then do
        imageSetFromAnimation imageWidget loadedAnimation
    else do
        {- Reload image as pixbuf. Yes, it's efficiency is poor -}
        zoomState <- readIORef zoom
        sourceWidth <- pixbufAnimationGetWidth loadedAnimation
        sourceHeight <- pixbufAnimationGetHeight loadedAnimation
        WindowSize windowWidth windowHeight <- getWindowSize imageWidget
        case zoomState of
          ZoomWidth offset_ -> do
            loadedPixbuf <- pixbufNewFromFile fileToDisplay
            let scale = getScale windowWidth windowHeight sourceWidth sourceHeight
            offset <- fixZoom zoom offset_ $ ceiling $ ß sourceHeight * scale - ß windowHeight
            pixbufScale loadedPixbuf loadedPixbuf 0 0 ( ceiling $ ß sourceWidth * scale ) ( ceiling $ ß sourceHeight * scale )
              0 ( ß offset ) scale scale InterpBilinear
            imageSetFromPixbuf imageWidget loadedPixbuf
          _ -> do
            loadedPixbuf <- pixbufNewFromFileAtSize
                            fileToDisplay
                            ( resultWidth zoomState windowWidth sourceWidth )
                            ( resultHeight zoomState windowHeight sourceHeight )
            imageSetFromPixbuf imageWidget loadedPixbuf
    where
    getScale :: Int -> Int -> Int -> Int -> Double
    getScale wW wH sW sH = ß (min wW sW) / ß sW
    fixZoom :: IORef Zoom -> Int -> Int -> IO Int
    fixZoom zoom offset maxHeight = do
      let offset_ = min 0 $ fromIntegral $ max offset $ negate maxHeight
      atomicWriteIORef zoom $ ZoomWidth offset_
      return offset_
    resultWidth :: Zoom -> Int -> Int -> Int
    resultWidth ZoomFit windowWidth sourceWidth = min windowWidth sourceWidth
    resultWidth ZoomFill windowWidth _ = windowWidth
    resultWidth (ZoomWidth _) windowWidth sourceWidth = min windowWidth sourceWidth
    resultHeight :: Zoom -> Int -> Int -> Int
    resultHeight ZoomFit windowHeight sourceHeight = min windowHeight sourceHeight
    resultHeight ZoomFill windowHeight _ = windowHeight
    resultHeight (ZoomWidth _) windowHeight sourceHeight = min windowHeight sourceHeight

ß = fromIntegral

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

{- Extract name of current file from IORef -}
extractFullName :: IORef Position -> IO String
extractFullName iorefPosition = do
    currentDir   <- getCurrentDirectory
    position     <- readIORef iorefPosition
    let fileList  = files position
    let safeIndex = mod ( ix_pos position ) ( length fileList )
    let fileName  = fileList !! safeIndex
    return $ FS.encodeString $ FS.append ( FS.decodeString currentDir )
                                         ( FS.decodeString fileName )
