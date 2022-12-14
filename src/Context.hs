module CalcState(

) where
import Control.Monad.State
import qualified DispValue as Dv
import Data.GI.Base.Overloading ( IsDescendantOf )
import Control.Monad.IO.Class ( MonadIO )
import qualified GI.Gtk as Gtk.Object
import qualified GI.Gtk as Gtk
import qualified Data.Text as T

setEntry :: (
    IsDescendantOf Gtk.Object.EntryBuffer o,
    MonadIO m, Gtk.Object.GObject o
    ) => o -> Dv.DispVal -> m ()
setEntry buffer dv = Gtk.setEntryBufferText buffer $ T.pack $ show dv
