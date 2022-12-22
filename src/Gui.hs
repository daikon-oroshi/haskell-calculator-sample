{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Gui (
    runGui
) where

import qualified GI.Gtk as Gtk
import Data.GI.Base ( on, AttrOp((:=)), new )
import qualified GI.Gtk as Gtk.Object
import Data.GI.Base.Overloading ( IsDescendantOf )
import qualified Data.Text as T
import Calc.CalcState as Cs
import Data.IORef
import DispValue (DispVal)

runGui :: IO ()
runGui = do
    _ <- Gtk.init Nothing
    win <- new Gtk.Window [ #title := "calculator" ]
    _ <- on win #destroy Gtk.mainQuit

    root_box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    text_view <- new Gtk.Entry [ #maxLength := 11, #editable := False]
    _ <- Gtk.entrySetAlignment text_view 1
    #add root_box text_view
    buffer <- Gtk.getEntryBuffer text_view

    keys_box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    ref <- newIORef initialState
    setEntry buffer ref

    op_keys_box <- operationKeysBox buffer ref
    num_keys_box <- numKeysBox buffer ref

    #add keys_box num_keys_box
    #add keys_box op_keys_box
    #add root_box keys_box
    #add win root_box
    #showAll win
    Gtk.main

onClickDigit :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> Int -> IO ()
onClickDigit buffer csRef a = do
    modifyIORef csRef (
            \cs -> Cs.actionDigit (Cs.csStep cs) cs a
        )
    setEntry buffer csRef

onClickZeroZero :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO ()
onClickZeroZero buffer csRef = do
    modifyIORef csRef (
            \cs -> Cs.actionZeroZero (Cs.csStep cs) cs
        )
    setEntry buffer csRef

onClickDot :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO ()
onClickDot buffer csRef = do
    modifyIORef csRef (
            \cs -> Cs.actionDot (Cs.csStep cs) cs
        )
    setEntry buffer csRef

onClickAc :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO ()
onClickAc buffer csRef = do
    modifyIORef csRef (
            \cs -> Cs.actionAc (Cs.csStep cs) cs
        )
    setEntry buffer csRef

onClickC :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO ()
onClickC buffer csRef = do
    modifyIORef csRef (
            \cs -> Cs.actionC (Cs.csStep cs) cs
        )
    setEntry buffer csRef

onClickOperation :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> Cs.Operation -> IO ()
onClickOperation buffer csRef op = do
    modifyIORef csRef (
            \cs -> Cs.actionOperation (Cs.csStep cs) cs op
        )
    setEntry buffer csRef

onClickEq :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO ()
onClickEq buffer csRef = do
    modifyIORef csRef (
            \cs -> Cs.actionEq (Cs.csStep cs) cs
        )
    setEntry buffer csRef

onClickPm :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO ()
onClickPm buffer csRef = do
    modifyIORef csRef (
            \cs -> Cs.actionPm (Cs.csStep cs) cs
        )
    setEntry buffer csRef

numKey :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> Int -> IO Gtk.Object.Button
numKey buffer csRef a = do
    num_btn <- new Gtk.Button [ #label := T.pack (show a) ]
    _ <- on num_btn #clicked $ onClickDigit buffer csRef a
    return num_btn

numKeyList :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> [Int]-> IO [Gtk.Object.Button]
numKeyList _ _ [] = return []
numKeyList buffer dv (x:xs) = do
    b <- numKey buffer dv x
    bs <- numKeyList buffer dv xs
    return $ b:bs

numKeysBox :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO Gtk.Object.Box
numKeysBox buffer csRef = do
    row1_btns <- numKeyList buffer csRef [1, 2, 3]
    row2_btns <- numKeyList buffer csRef [4, 5, 6]
    row3_btns <- numKeyList buffer csRef [7, 8, 9]

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
    _ <- on zerozero_btn #clicked $ onClickZeroZero buffer csRef
    #add row_4 zerozero_btn

    zero_btn <- numKey buffer csRef 0
    #add row_4 zero_btn

    dot_btn <- new Gtk.Button [ #label := "." ]
    _ <- on dot_btn #clicked $ onClickDot buffer csRef
    #add row_4 dot_btn

    #add numkeys_box row_4
    return numkeys_box

operationKeysBox :: Gtk.EntryBuffer -> IORef (Cs.CalcState DispVal) -> IO Gtk.Object.Box
operationKeysBox buffer csRef = do
    operations_box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    row_1 <- new Gtk.HButtonBox []
    c_btn <- new Gtk.Button [ #label := "C" ]
    _ <- on c_btn #clicked $ onClickC buffer csRef
    #add row_1 c_btn

    ac_btn <- new Gtk.Button [ #label := "AC" ]
    _ <- on ac_btn #clicked $ onClickAc buffer csRef
    #add row_1 ac_btn
    #add operations_box row_1

    row_2 <- new Gtk.HButtonBox []
    plus_btn <- new Gtk.Button [ #label := "+" ]
    _ <- on plus_btn #clicked $ onClickOperation buffer csRef Cs.Plus
    #add row_2 plus_btn

    minus_btn <- new Gtk.Button [ #label := "-" ]
    _ <- on minus_btn #clicked $ onClickOperation buffer csRef Cs.Sub
    #add row_2 minus_btn
    #add operations_box row_2

    row_3 <- new Gtk.HButtonBox []
    prod_btn <- new Gtk.Button [ #label := "×" ]
    _ <- on prod_btn #clicked $ onClickOperation buffer csRef Cs.Prod
    #add row_3 prod_btn
    div_btn <- new Gtk.Button [ #label := "÷" ]
    _ <- on div_btn #clicked $ onClickOperation buffer csRef Cs.Div
    #add row_3 div_btn
    #add operations_box row_3

    row_4 <- new Gtk.HButtonBox []

    pm_btn <- new Gtk.Button [ #label := "±" ]
    _ <- on pm_btn #clicked $ onClickPm buffer csRef
    #add row_4 pm_btn

    equal_btn <- new Gtk.Button [ #label := "=" ]
    _ <- on equal_btn #clicked $ onClickEq buffer csRef
    #add row_4 equal_btn

    #add operations_box row_4

    return operations_box

setEntry :: (
    IsDescendantOf Gtk.Object.EntryBuffer o,
    Gtk.Object.GObject o
    ) => o -> IORef (Cs.CalcState DispVal) -> IO ()
setEntry buffer csRef = do
    cs <- readIORef csRef
    Gtk.setEntryBufferText buffer $ T.pack $ show (Cs.csCurrentVal cs)