
// per-view constants

layout(shared) uniform viewBlock {
  mat4 viewProjectionMat;
  mat4 inverseViewMat;

  uniform samplerCube cubeEnvironmentMap;
  uniform sampler2D splitSumLUT;

  int debugMode;

  vec2 zNearFar;
  vec4 time;
} view;
