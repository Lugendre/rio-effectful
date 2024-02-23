module EIO.Prelude.Renames (yieldThread) where

import Effectful (Eff, (:>))
import Effectful.Concurrent

yieldThread :: (Concurrent :> es) => Eff es ()
yieldThread = yield
