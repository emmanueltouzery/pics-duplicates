{-# LANGUAGE OverloadedStrings, OverloadedLabels, LambdaCase, NoMonomorphismRestriction #-}

import Relude

import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.Directory as Dir

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

imageInfo :: (Path Abs File->IO ()) -> Path Abs File -> IO Gtk.VBox
imageInfo imagePickedHandler path = do
  pixbuf <- Gdk.pixbufNewFromFile (toFilePath path)
  (pWidth, pHeight) <- (,) <$> Gtk.get pixbuf #width <*> Gtk.get pixbuf #height
  scaledPb <- fromJust <$> Gdk.pixbufScaleSimple pixbuf 150 150 Gdk.InterpTypeBilinear

  image <- new Gtk.Image [ #pixbuf := scaledPb]

  box <- new Gtk.VBox []
  #add box image

  let folderName = T.dropEnd 1 $ T.pack $ toFilePath $ dirname $ parent path
  let labelMsg = folderName <> " / " <> show pWidth <> "x" <> show pHeight
  button <- new Gtk.Button [ #label := labelMsg ]
  Gtk.on button #clicked (imagePickedHandler path)
  #add box button

  pure box

type AvailableImages = Map Text [Path Abs File]

hashFilename :: Path Rel File -> Text
hashFilename = T.toLower . T.pack . toFilePath

getAvailableImages :: Path Abs File -> IO AvailableImages
getAvailableImages hashFile = do
  -- tailDef to remove the first file which has some meta-info
  let nonEmptyOpDef op def = maybe def op . nonEmpty
  let headDef = nonEmptyOpDef head
  let tailDef = nonEmptyOpDef tail
  fileLines <- tailDef [] . T.lines <$> T.readFile (toFilePath hashFile)
  filenames <- traverse (parseAbsFile . T.unpack) $
               headDef ""  . T.splitOn "\t"
               <$> fileLines
  let pairs = map (\f -> (hashFilename $ filename f, f)) filenames
  pure $ foldr (\(k,v) sofar -> Map.insertWith (++) k [v] sofar) Map.empty pairs


addPage :: Int -> Int -> Gtk.Assistant -> AvailableImages -> Path Abs Dir -> Path Abs File -> IO ()
addPage pageIndex pageCount win availableImages targetDir pic = do
  grid <- new Gtk.Grid []

  let imagePickedHandler imgPath = do
        print (toFilePath imgPath)
        copyFile imgPath (targetDir </> filename pic)
        if pageIndex < pageCount
          then #nextPage win
          else exitSuccess

  #add grid =<< imageInfo imagePickedHandler pic
  let candidates = Map.findWithDefault [] (hashFilename $ filename pic) availableImages
  putStrLn ("Found " <> show (length candidates) <> " candidates")
  forM_ candidates (#add grid <=< imageInfo imagePickedHandler)

  vbox <- new Gtk.VBox []
  label <- new Gtk.Label [ #label := "<big><b>Image " <> show pageIndex
                                     <> "/" <> show pageCount <> "</b></big>"
                         , #singleLineMode := True, #useMarkup := True ]
  #packStart vbox label False False 0
  #add vbox grid

  void (#appendPage win vbox)

handleImages :: Path Abs Dir -> Path Abs File -> Path Abs Dir -> IO ()
handleImages imgFolder hashesFile targetDir = do
  pics <- snd <$> listDir imgFolder
  availableImages <- getAvailableImages hashesFile
  
  Gtk.init Nothing
  win <- new Gtk.Assistant []
  Gtk.on win #destroy Gtk.mainQuit

  forM_ (zip pics [1..]) $ \(pic, idx) -> 
    addPage idx (length pics) win availableImages targetDir pic

  #showAll win
  Gtk.main
