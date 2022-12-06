{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base ( on, AttrOp((:=)), new )
import qualified GI.Gtk as Gtk.Object
import qualified Data.Text as T


main :: IO ()
main = do
    _ <- Gtk.init Nothing
    win <- new Gtk.Window [ #title := "calculator" ]
    _ <- on win #destroy Gtk.mainQuit

    root_box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    text_view <- new Gtk.Entry [ #maxLength := 11, #editable := False]
    #add root_box text_view

    keys_box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]

    num_keys_box <- numKeysBox
    op_keys_box <- operationKeysBox

    #add keys_box num_keys_box
    #add keys_box op_keys_box
    #add root_box keys_box

    #add win root_box
    #showAll win
    Gtk.main

numKey :: Int -> IO Gtk.Object.Button
numKey a = do
    num_btn <- new Gtk.Button [ #label := T.pack (show a) ]
    _ <- on num_btn #clicked (print a)
    return num_btn

numKeysBox :: IO Gtk.Object.Box
numKeysBox = do
    numkeys_box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    row_1 <- new Gtk.HButtonBox []
    mapM_ (\x -> do
            btn <- numKey x
            #add row_1 btn
        ) [1, 2, 3]
    #add numkeys_box row_1

    row_2 <- new Gtk.HButtonBox []
    mapM_ (\x -> do
            btn <- numKey x
            #add row_2 btn
        ) [4, 5, 6]
    #add numkeys_box row_2

    row_3 <- new Gtk.HButtonBox []
    mapM_ (\x -> do
            btn <- numKey x
            #add row_3 btn
        ) [7, 8, 9]
    #add numkeys_box row_3

    row_4 <- new Gtk.HButtonBox []
    zerozero_btn <- new Gtk.Button [ #label := "00" ]
    _ <- on zerozero_btn #clicked (putStrLn "00")
    #add row_4 zerozero_btn

    zero_btn <- new Gtk.Button [ #label := "0" ]
    _ <- on zero_btn #clicked (putStrLn "0")
    #add row_4 zero_btn

    dot_btn <- new Gtk.Button [ #label := "." ]
    _ <- on dot_btn #clicked (putStrLn ".")
    #add row_4 dot_btn

    #add numkeys_box row_4

    return numkeys_box

operationKeysBox :: IO Gtk.Object.Box
operationKeysBox = do
    operations_box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    row_1 <- new Gtk.HButtonBox []
    percent_btn <- new Gtk.Button [ #label := "%" ]
    _ <- on percent_btn #clicked (putStrLn "%")
    #add row_1 percent_btn
    ac_btn <- new Gtk.Button [ #label := "AC" ]
    _ <- on percent_btn #clicked (putStrLn "AC")
    #add row_1 ac_btn
    #add operations_box row_1

    row_2 <- new Gtk.HButtonBox []
    plus_btn <- new Gtk.Button [ #label := "+" ]
    _ <- on plus_btn #clicked (putStrLn "+")
    #add row_2 plus_btn
    minus_btn <- new Gtk.Button [ #label := "-" ]
    _ <- on minus_btn #clicked (putStrLn "-")
    #add row_2 minus_btn
    #add operations_box row_2

    row_3 <- new Gtk.HButtonBox []
    prod_btn <- new Gtk.Button [ #label := "×" ]
    _ <- on prod_btn #clicked (putStrLn "×")
    #add row_3 prod_btn
    div_btn <- new Gtk.Button [ #label := "÷" ]
    _ <- on percent_btn #clicked (putStrLn "÷")
    #add row_3 div_btn
    #add operations_box row_3

    row_4 <- new Gtk.HButtonBox []
    equal_btn <- new Gtk.Button [ #label := "=" ]
    _ <- on equal_btn #clicked (putStrLn "=")
    #add row_4 equal_btn
    #add operations_box row_4

    return operations_box
