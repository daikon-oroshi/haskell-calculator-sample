{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Gui (
    runGui
) where

import qualified GI.Gtk as Gtk
import Data.GI.Base ( on, AttrOp((:=)), new )
import qualified GI.Gtk as Gtk.Object
import qualified Data.Text as T
import qualified DispValue as Dv
import Context (setEntry)
import DispValue (zeroDispVal)
import Data.IORef

runGui :: IO ()
runGui = do
    _ <- Gtk.init Nothing
    win <- new Gtk.Window [ #title := "calculator" ]
    _ <- on win #destroy Gtk.mainQuit

    root_box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    text_view <- new Gtk.Entry [ #maxLength := 11, #editable := False]
    #add root_box text_view
    buffer <- Gtk.getEntryBuffer text_view

    keys_box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    op_keys_box <- operationKeysBox

    ref <- newIORef zeroDispVal
    num_keys_box <- numKeysBox buffer ref

    #add keys_box num_keys_box
    #add keys_box op_keys_box
    #add root_box keys_box
    #add win root_box
    #showAll win
    Gtk.main

numKeyAction :: Gtk.EntryBuffer ->  IORef Dv.DispVal -> Int -> IO ()
numKeyAction buffer dvRef a = do
    modifyIORef dvRef (`Dv.addNumber` a)
    setEntry buffer dvRef

numKey :: Gtk.EntryBuffer -> IORef Dv.DispVal -> Int -> IO Gtk.Object.Button
numKey buffer dvRef a = do
    num_btn <- new Gtk.Button [ #label := T.pack (show a) ]
    _ <- Gtk.onButtonClicked num_btn $ numKeyAction buffer dvRef a
    return num_btn

numKeyList :: Gtk.EntryBuffer -> IORef Dv.DispVal -> [Int]-> IO [Gtk.Object.Button]
numKeyList _ _ [] = return []
numKeyList buffer dv (x:xs) = do
    b <- numKey buffer dv x
    bs <- numKeyList buffer dv xs
    return $ b:bs

numKeysBox :: Gtk.EntryBuffer -> IORef Dv.DispVal -> IO Gtk.Object.Box
numKeysBox buffer dvRef = do
    row1_btns <- numKeyList buffer dvRef [1, 2, 3]
    row2_btns <- numKeyList buffer dvRef [4, 5, 6]
    row3_btns <- numKeyList buffer dvRef [7, 8, 9]

    numkeys_box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    row_1 <- new Gtk.HButtonBox []
    mapM_ (#add row_1) row1_btns
    #add numkeys_box row_1

    row_2 <- new Gtk.HButtonBox []
    mapM_ (#add row_2) row2_btns
    #add numkeys_box row_2

    row_3 <- new Gtk.HButtonBox []
    mapM_ (#add row_3) row3_btns
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
