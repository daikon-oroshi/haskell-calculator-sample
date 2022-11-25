{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Lib

main :: IO ()
main = do
    Gtk.init Nothing
    win <- new Gtk.Window [ #title := "calculator" ]
    on win #destroy Gtk.mainQuit
    button <- new Gtk.Button [ #label := "押して" ]
    on button #clicked (set button [ #sensitive := False, #label := "OK" ] )
    #add win button
    #showAll win
    Gtk.main