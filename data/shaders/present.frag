

#ifdef MULTISAMPLE
layout(location = 0, binding=0) uniform sampler2DMS scene;
#else
layout(location = 0, binding=0) uniform sampler2D scene;
#endif

layout(location = 1, binding=1) uniform sampler2D gui;
layout(location = 15) uniform int debugMode;

in vec2 uv;
layout(location = 0) out vec4 fragmentOut;

vec3 colour_saturate(vec3 src, float a) {
  float luma = dot(src, vec3(0.333, 0.334, 0.333));
  float maxC = max(max(0.00001, src.x), max(src.y, src.z));
  vec3 chroma = src / maxC;
  chroma = pow(chroma, vec3(max(0, a)));
  return (luma * chroma);
}

vec3 tonemap_exposure(vec3 sceneRaw) {
    float exposure = 1.9;
    return vec3(1) - exp(-sceneRaw * exposure);
}

vec3 tonemap_reinhard(vec3 sceneRaw) {
  return sceneRaw / (sceneRaw+1);
}

vec3 tonemap_gamma(vec3 sceneRaw) {
    return pow(sceneRaw, vec3(1/2.2));
}

vec3 tonemap_aces(const vec3 sceneRaw) {
  // Narkowicz 2015, "ACES filmic tone mapping curve"
  const float a = 2.51;
  const float b = 0.03;
  const float c = 2.43;
  const float d = 0.59;
  const float e = 0.14;
  return (sceneRaw * (a*sceneRaw+b)) / (sceneRaw*(c*sceneRaw+d)+e);
}

// very crude approx sRGB transfer function baked in
vec3 tonemap_mobile_sRGB(const vec3 sceneRaw) {
  return sceneRaw / (sceneRaw + 0.155) * 1.019;
}

float vignette() {
  vec2 d = uv * 2 - 1;
  d *= vec2(1.4, 1);
  float f = dot(d,d) - 0.8;
  return clamp(1.0 - f * 0.125, 0.0, 1.0);
}

void main() {
#ifdef MULTISAMPLE
  ivec2 texel =  ivec2(gl_FragCoord.xy);
  vec4 raw0 = texelFetch(scene, texel, 0);
  vec4 raw1 = texelFetch(scene, texel, 1);
  vec4 sceneRaw = (raw0+raw1) * 0.5;
#else
  vec4 sceneRaw = textureLod(scene, uv, 0);
#endif

  vec3 tonemapped = tonemap_aces(sceneRaw.xyz);
  //tonemapped = colour_saturate(tonemapped, 1.5);

  if (debugMode > 0 && debugMode < 12) {
    fragmentOut = sceneRaw;
  } else {
    float v = vignette();
    fragmentOut = vec4(tonemapped * v, sceneRaw.w);
  }

#if 0
  vec4 guiPix = textureLod(gui, uv, 0);
  fragmentOut = mix(fragmentOut, guiPix, guiPix.w);
#endif
}
