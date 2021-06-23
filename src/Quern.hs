
module Quern
    (
    quit
    --  startRenderThread
    --, awaitRenderThread
    --, RenderState
    --, RenderThread
    -- * Re-exports

    , module Q
    ) where

import Quern.Types as Q
import Quern.Window as Q
import Quern.Logging as Q
import Quern.Render.GL as Q
import Quern.Physics.AxisAlignedBox as Q

import qualified SDL.Init as SDL

--import System.FilePath.Posix ((</>))
--import System.Exit
--import System.IO

quit :: IO ()
quit = SDL.quit
