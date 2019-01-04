{-# LANGUAGE OverloadedStrings, OverloadedLabels, LambdaCase, ScopedTypeVariables #-}

import System.Environment
import System.Exit

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.Directory as Dir

import Safe

import Path
import Path.IO

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as Gdk
import Data.GI.Base

main :: IO ()
main = do
  getArgs >>= \case
    [picsFolder, hashesFile] -> do
      pics <- parseAbsDir =<< Dir.canonicalizePath picsFolder
      candidates <- parseAbsFile =<< Dir.canonicalizePath hashesFile
      handleImages pics candidates
    otherwise -> do
      putStrLn "parameters: <pictures folder> <hashes file>"
      exitWith (ExitFailure 1)

data ImageInfoType = ImageInfoReference | ImageInfoCandidate deriving (Eq, Show)

imageInfo :: ImageInfoType -> Path a File -> IO Gtk.VBox
imageInfo imageInfoType path = do
  pixbuf <- Gdk.pixbufNewFromFile (toFilePath path)
  (pWidth, pHeight) <- (,) <$> get pixbuf #width <*> get pixbuf #height
  scaledPb <- fromJust <$> Gdk.pixbufScaleSimple pixbuf 150 150 Gdk.InterpTypeBilinear

  image <- new Gtk.Image [ #pixbuf := scaledPb]

  box <- new Gtk.VBox []
  #add box image

  when (imageInfoType == ImageInfoCandidate) $ do
      let labelMsg = toFilePath (dirname (parent path)) <> " / "
                     <> show pWidth <> "x" <> show pHeight
      button <- new Gtk.Button [ #label := T.pack labelMsg ]
      on button #clicked $ do
          set button [ #sensitive := False]
          putStrLn (toFilePath path)
          exitSuccess
      #add box button

  pure box

getAvailableImages :: Path Abs File -> IO (Map (Path Rel File) [Path Abs File])
getAvailableImages hashFile = do
  -- tailDef to remove the first file which has some meta-info
  fileLines <- tailDef [] <$> T.lines <$> T.readFile (toFilePath hashFile)
  filenames <- traverse (parseAbsFile . T.unpack) $
               headDef ""  . T.splitOn "\t"
               <$> fileLines
  let pairs = map (\f -> (filename f, f)) filenames
  pure $ foldr (\(k,v) sofar -> Map.insertWith (++) k [v] sofar) Map.empty pairs

handleImages :: Path Abs Dir -> Path Abs File -> IO ()
handleImages imgFolder hashesFile = do
  pics <- snd <$> listDir imgFolder
  availableImages <- getAvailableImages hashesFile
  
  Gtk.init Nothing
  -- win <- new Gtk.Window [ #title := "Hi there" ]
  win <- new Gtk.Assistant []
  on win #destroy Gtk.mainQuit

  grid <- new Gtk.Grid []

  let pic = head pics
  print $ filename pic

  #add grid =<< imageInfo ImageInfoReference pic
  let candidates = Map.findWithDefault [] (filename pic) availableImages
  print candidates
  forM_ candidates (#add grid <=< imageInfo ImageInfoCandidate)

  #appendPage win grid

  #showAll win
  Gtk.main
