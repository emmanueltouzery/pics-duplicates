{-# LANGUAGE OverloadedStrings, OverloadedLabels,
             LambdaCase, NoMonomorphismRestriction, TypeApplications #-}

import Relude

import System.Environment

import Control.Exception.Safe
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.Directory as Dir
import qualified Data.Set as Set
import System.FilePath.Posix (takeFileName)
import Data.List (partition)

import Path
import Path.IO

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as Gdk
import Data.GI.Base (new, AttrOp((:=)))

main :: IO ()
main = getArgs >>= \case
    [picsFolder, hashesFile] -> do
      pics <- parseAbsDir =<< Dir.canonicalizePath picsFolder
      candidates <- parseAbsFile =<< Dir.canonicalizePath hashesFile
      curDir <- getCurrentDir
      let targetDir = curDir </> dirname pics
      createDirIfMissing False targetDir
      handleImages pics candidates targetDir
    _ -> do
      putStrLn "parameters: <pictures folder> <hashes file>"
      exitFailure

imageInfo :: Path Abs File -> (Path Abs File->IO (Path Abs File))
          -> Path Abs File -> IO Gtk.VBox
imageInfo origPic imagePickedHandler path = do
  (_, pWidth, pHeight) <- Gdk.pixbufGetFileInfo (toFilePath path)
  scaledPb <- Gdk.pixbufNewFromFileAtSize (toFilePath path) 150 150

  -- scaledPb is not 150x150 but 150x100 or 100x150 or whatever,
  -- depending on the picture aspect ratio. That means the picture
  -- height will be different on different screens, which is annoying.
  -- create a new pixbuf of 150x150 and copy the one I got in that
  -- one so I always have the same size.
  colorspace <- Gdk.pixbufGetColorspace scaledPb
  bits <- Gdk.get scaledPb #bitsPerSample
  Just squarePb <- Gdk.pixbufNew colorspace True bits 150 150
  Gdk.pixbufFill squarePb 0x00000000
  w <- Gdk.pixbufGetWidth scaledPb
  h <- Gdk.pixbufGetHeight scaledPb
  Gdk.pixbufCopyArea scaledPb 0 0
    w h squarePb ((150 - w) `div` 2) ((150 - h) `div` 2)

  image <- new Gtk.Image [ #pixbuf := squarePb ]

  box <- new Gtk.VBox []
  #packStart box image False False 0

  buttons <- new Gtk.HBox []
  #packStart box buttons False False 0

  let folderName = T.dropEnd 1 $ T.pack $ toFilePath $ dirname $ parent path
  let labelMsg = folderName <> " / " <> show pWidth <> "x" <> show pHeight
  button <- new Gtk.Button [ #label := labelMsg ]
  Gtk.on button #clicked (void $ imagePickedHandler path)
  #add buttons button

  editBtn <- new Gtk.Button [ #label := "🎨" ]
  Gtk.on editBtn #clicked (void $ imagePickedHandler path >>=
                           spawnCommand . (\f -> "gimp \"" <> toFilePath origPic
                                            <> "\" \"" <> toFilePath f <> "\""))
  #add buttons editBtn

  pure box

type AvailableImages = Map Text [Path Abs File]

hashFilename :: Path Rel File -> Text
hashFilename = T.toLower . T.pack . toFilePath

getAvailableImages :: Path Abs File -> Set Text -> IO AvailableImages
getAvailableImages hashFile picNamesToCheck = do
  -- tailDef to remove the first file which has some meta-info
  let nonEmptyOpDef op def = maybe def op . nonEmpty
  let headDef = nonEmptyOpDef head
  let tailDef = nonEmptyOpDef tail
  fileLines <- tailDef [] . T.lines <$> T.readFile (toFilePath hashFile)
  let filenames = headDef ""  . T.splitOn "\t" <$> fileLines
  let fname pic = T.pack $ takeFileName $ T.unpack $ T.toLower pic
  let filteredNames = filter (\k -> Set.member (fname k) picNamesToCheck) filenames
  filteredFiles <- traverse (parseAbsFile . T.unpack) filteredNames
  let pairs = map (\f -> (hashFilename $ filename f, f)) filteredFiles
  pure $ foldr (\(k,v) sofar -> Map.insertWith (++) k [v] sofar) Map.empty pairs

addImageInfo :: Path Abs File -> Gtk.FlowBox
             -> (Path Abs File->IO (Path Abs File)) -> Path Abs File -> IO ()
addImageInfo origPic grid imagePickedHandler pic =
  tryAny (imageInfo origPic imagePickedHandler pic) >>= \case
    Right info -> #add grid info
    Left err -> error ("*** Error handling picture "
                       <> show pic <> ": " <> show err)

addPage :: Int -> Int -> Gtk.Assistant -> Gtk.Button -> AvailableImages
        -> Path Abs Dir -> Path Abs File -> IO ()
addPage pageIndex pageCount win closeBtn availableImages targetDir pic = do
  grid <- new Gtk.FlowBox []

  let imagePickedHandler imgPath = do
        let targetFile = targetDir </> filename pic
        copyFile imgPath targetFile
        if pageIndex < pageCount
          then #nextPage win
          else do
            dlg <- new Gtk.MessageDialog [ #useHeaderBar := 1
                                         , #buttons := Gtk.ButtonsTypeClose
                                         , #text := "Done!" ]
            Gtk.dialogRun dlg
            Gtk.widgetDestroy dlg
            Gtk.set closeBtn [ #sensitive := True ]

        pure targetFile

  addImageInfo pic grid imagePickedHandler pic
  let candidates = Map.findWithDefault [] (hashFilename $ filename pic) availableImages
  putStrLn ("Found " <> show (length candidates) <> " candidates")
  forM_ candidates (addImageInfo pic grid imagePickedHandler)

  vbox <- new Gtk.VBox []
  label <- new Gtk.Label [ #label := "<big><b>Image " <> show pageIndex
                                     <> "/" <> show pageCount <> "</b></big>"
                         , #singleLineMode := True, #useMarkup := True ]
  #packStart vbox label False False 0
  #packStart vbox grid False False 0

  scrolledWindow <- new Gtk.ScrolledWindow []
  #add scrolledWindow vbox

  #appendPage win scrolledWindow
  #setPageType win scrolledWindow Gtk.AssistantPageTypeCustom

handleImages :: Path Abs Dir -> Path Abs File -> Path Abs Dir -> IO ()
handleImages imgFolder hashesFile targetDir = do
  pics <- snd <$> listDir imgFolder
  availableImages <- getAvailableImages hashesFile
                       (Set.fromList $ hashFilename . filename <$> pics)
  
  Gtk.init Nothing
  win <- new Gtk.Assistant []
  Gtk.on win #destroy Gtk.mainQuit

  closeBtn <- do
    btn <- new Gtk.Button [ #label := "Close", #sensitive := False ]
    Gtk.on btn #clicked exitSuccess
    pure btn
  #addActionWidget win closeBtn

  let isVideo pic = (T.toLower . T.pack . fileExtension) pic `elem` [".avi", ".mp4", ".mov", ".3gp"]
  let (videos, photos) = partition isVideo pics
  forM_ videos $ \video -> copyFile video (targetDir </> filename video)
  forM_ (zip photos [1..]) $ \(pic, idx) -> 
        addPage idx (length photos) win closeBtn availableImages targetDir pic

  #showAll win
  Gtk.main
