/*
#extension GL_ARB_bindless_texture : enable
#include <block_view.glsl>
#include <block_lighting.glsl>
*/

#include <utility.glsl>

layout(location = 0) out vec4 fragOut;

in vec2 particleUv;
in vec3 worldPos;
in vec3 pclTan;
in vec3 pclBitan;
in vec3 pclNrm;
in float erodeFactor;
in vec3 pclClr;

layout(location = 2, binding = 0) uniform sampler2D atlas;
layout(location = 3, binding = 1) uniform sampler2DShadow sunlightShadow;
layout(location = 4, binding = 2) uniform samplerCube cubeEnvironmentMap;

layout(location = 8) uniform mat4 sunlightViewProjection;
layout(location = 9) uniform mat4 sunlightInverseView;
layout(location = 10) uniform mat4 inverseView;

void main() {

  vec3 cameraPos = vec3(inverseView[3].xyz);
  vec3 e = worldPos - cameraPos;
  vec3 v = -normalize(e);

  vec4 tx = texture2D(atlas, particleUv.xy);
  float alphaLo = mix(0.01, 1.0, erodeFactor);
  float alphaHi = mix(0.3, 1.0, erodeFactor);
  float alpha = smoothstep(alphaLo, alphaHi, tx.a) * 0.5;

  mat3 tbn = mat3(pclTan, pclBitan, pclNrm);
  vec3 texN = toLinear(tx.xyz) * 2 - 1;
  vec3 n =  normalize( tbn * texN );

  vec3 r = 2.0 * dot(v, n) * n - v;

  vec4 shadowH = sunlightViewProjection * vec4(worldPos,1);
  vec3 shadowNDC = shadowH.xyz / shadowH.w;
  shadowNDC.xy = shadowNDC.xy * 0.5 + 0.5;

  float shadow = textureLod(sunlightShadow, shadowNDC, 0);

  shadow = mix(shadow, 1.0, 0.25);


  vec3 baseColour = pclClr;
  vec3 sunLightColour = vec3(6.0, 5.0, 5.5);

  vec3 l = sunlightInverseView[2].xyz;
  float lambert = saturate(dot(n, l));

  float roughness = 0.4;
  vec3 surfSpecular = vec3(0.04, 0.05, 0.04);
  vec3 ao = vec3(1,1,1);


  vec3 specular = shadow * specBRDF(roughness, n, surfSpecular, ao, lambert, l, v, true) * sunLightColour;
  vec3 diffuse =
    shadow * sunLightColour * baseColour * lambert / pi +
    baseColour * sampleEnv(cubeEnvironmentMap, 1, n) / pi;
  vec3 env = sampleEnv(cubeEnvironmentMap, roughness, r) * saturate(0.25 + schlick(dot(v, n)));


  fragOut = vec4(alpha * (diffuse + specular + env), alpha);
  //fragOut = vec4( (n * 0.5 + 0.5) * alpha, alpha );
}
