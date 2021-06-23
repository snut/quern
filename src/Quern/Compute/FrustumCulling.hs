module Quern.Compute.FrustumCulling
  ( loadCullingShader
  ) where

import Control.Monad.IO.Class
import Quern.Logging
import Quern.Render.Shader

loadCullingShader :: (HasLogging env, MonadReader env m, MonadIO m) => m (Either String Program)
loadCullingShader  = loadKernelFromFile "./data/shaders/frustum_culling.comp"
  
