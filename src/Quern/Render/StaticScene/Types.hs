
{-# Language TemplateHaskell #-}

module Quern.Render.StaticScene.Types where


import Control.Lens
import Data.IORef
--import Graphics.GL.Core45
import Graphics.GL.Types
import Linear
import Quern.Render.Target
--import Quern.Render.Texture
import Quern.Render.Shader
import Quern.Render.StaticScene.SceneStorage
import Quern.Render.Particles.CpuParticles

data StaticSceneDebug = StaticSceneDebug
  { _sceneDebugVB :: !GLuint
  , _sceneDebugIB :: !GLuint
  , _sceneDebugVAO :: !GLuint
  , _sceneDebugAtomics :: !GLuint
  , _sceneDebugRenderProg :: !GLuint
  } deriving (Eq, Ord, Show)
makeLenses ''StaticSceneDebug

-- | All the targets and textures that must be resized
-- or recreated when the window resolution changes.
data SceneTargets = SceneTargets
  { _mainTarget :: !Target
  , _backfaceTarget :: !Target
  , _preTarget :: !Target
  , _refractionTexture :: !Texture
  , _ssaoTexture :: !Texture
  , _guiPlane :: !Texture -- really shouldn't be here, but it does need to be resized
  } deriving (Eq, Ord, Show)
makeLenses ''SceneTargets

-- | Pass collects some indirect draw data and shader programs
-- to draw the pass and possibly shadows.
-- Meshes, instances and materials are stored in the scene itself,
-- but could be moved to per-pass data if they require different vertex data
-- or similar.
data ScenePass = ScenePass
  { _passDraws :: !StaticIndirectDrawStorage
  , _passProgram :: !Program
  , _passShadowProgram :: !(Maybe Program)
  , _passCullProgram :: !(Maybe Program)
  }
makeLenses ''ScenePass


-- | A scene is just a collection of storage buffers, program, and some uniforms
data StaticScene = StaticScene
  { _sceneEnvCubemap :: !Texture
  , _sceneMeshes :: !StaticMeshStorage
  , _sceneMaterials :: !MaterialStorage
  , _sceneInstances :: !InstanceStorage

  -- depth pre-pass
  , _scenePrePassOpaque :: !ScenePass
  , _scenePrePassMasked :: !ScenePass
  -- basic opaque meshes
  , _sceneOpaquePass :: !ScenePass
  -- foliage:
  --  alpha-tested, vertex animation
  , _sceneFoliagePass :: !ScenePass
  -- transparency:
  --  render opaques to a target
  --  copy and downres for refraction
  --  maybe: render back face depth + normal into additional buffer
  , _sceneTransparentBackfacePass :: !ScenePass
  , _sceneTransparentPass :: !ScenePass
  -- additive:
  --   no depth write
  , _sceneAdditivePass :: !ScenePass

  , _sceneCpuParticles :: !CpuParticleSystem

  , _sceneDebug :: !(Maybe StaticSceneDebug)

  -- shadow target for sun
  , _sceneShadowTarget :: !Target

  -- sunlight
  , _sceneSunViewProjection :: !(M44 Float)
  , _sceneSunInverseView :: !(M44 Float)
  , _sceneSunColour :: !(V3 Float)

  -- for PBR shaders
  , _sceneSplitSumLUT :: !Texture

  , _sceneTargets :: !(IORef SceneTargets)
  , _sceneDownresProgram :: !Program
  , _sceneDownresTextureProgram :: !Program
  , _sceneSsaoProgram :: !Program
  -- tonemap and present
  , _scenePresentProgram :: !Program
  }
makeLenses ''StaticScene



class HasStaticScene a where
  staticScene :: Lens' a StaticScene
