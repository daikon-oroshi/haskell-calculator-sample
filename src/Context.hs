module Context(
    setEntry
) where
import qualified DispValue as Dv
import Data.GI.Base.Overloading ( IsDescendantOf )
import qualified GI.Gtk as Gtk.Object
import qualified GI.Gtk as Gtk
import qualified Data.Text as T

import Data.IORef

setEntry :: (
    IsDescendantOf Gtk.Object.EntryBuffer o,
    Gtk.Object.GObject o
    ) => o -> IORef Dv.DispVal -> IO ()
setEntry buffer dvRef = do
    dv <- readIORef dvRef
    Gtk.setEntryBufferText buffer $ T.pack $ show dv
