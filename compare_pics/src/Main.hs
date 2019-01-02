{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import System.Environment
import System.Exit

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as Gdk
import Data.GI.Base

main :: IO ()
main = do
  args <- getArgs
  if length args < 1
    then do
      putStrLn "please pass paths to images as parameters"
      exitWith (ExitFailure 1)
    else handleImages args

folderName :: Text -> Text
folderName path = case elements of
      [] -> ""
      _ -> last $ init elements
  where elements = T.splitOn "/" path

data ImageInfoType = ImageInfoReference | ImageInfoCandidate deriving (Eq, Show)

imageInfo :: ImageInfoType -> String -> IO Gtk.VBox
imageInfo imageInfoType path = do
  pixbuf <- Gdk.pixbufNewFromFile path
  (pWidth, pHeight) <- (,) <$> get pixbuf #width <*> get pixbuf #height
  scaledPb <- fromJust <$> Gdk.pixbufScaleSimple pixbuf 150 150 Gdk.InterpTypeBilinear

  image <- new Gtk.Image [ #pixbuf := scaledPb]

  box <- new Gtk.VBox []
  #add box image

  when (imageInfoType == ImageInfoCandidate) $ do
      let tShow = T.pack . show
      button <- new Gtk.Button [ #label := folderName (T.pack path) <> " / "
                                 <> tShow pWidth <> "x" <> tShow pHeight ]
      on button #clicked $ do
          set button [ #sensitive := False]
          putStrLn path
          exitSuccess
      #add box button

  pure box

handleImages :: [String] -> IO ()
handleImages imgs = do
  Gtk.init Nothing
  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit

  grid <- new Gtk.Grid []
  #add win grid

  #add grid =<< imageInfo ImageInfoReference (head imgs)
  forM_ (tail imgs) (#add grid <=< imageInfo ImageInfoCandidate)

  #showAll win
  Gtk.main
