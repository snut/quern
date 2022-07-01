
#define PCF_SHADOWS 1
#define ESM_SHADOWS 0

layout(shared) uniform lightBlock {
  mat4 sunlightViewProjection;
  mat4 sunlightInverseView;

  #if PCF_SHADOWS
  uniform sampler2DShadow sunlightShadow;
  #else
  uniform sampler2D sunlightShadow;
  #endif
} lighting;
