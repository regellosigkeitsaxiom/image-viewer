module FileList where

import Shifts

import System.Random ( newStdGen )
import System.Random.Shuffle ( shuffle' )
import System.Environment ( getArgs )
import Data.IORef ( IORef
                  , newIORef
                  )

import qualified Filesystem.Path as FS
import qualified Filesystem.Path.CurrentOS as FS
import Data.List ( delete
                 , sort
                 )
import System.Directory ( getCurrentDirectory
                        , getDirectoryContents
                        , doesDirectoryExist
                        )


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
    let sortedFileList = reverse $ sort rawFileList
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

