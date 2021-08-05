#extension GL_ARB_bindless_texture : enable



#include <utility.glsl>

struct material {
  vec4 baseColour;
  vec3 normal;
  float tintMask;
  float ao;
  float roughness;
  float metallic;
  float height;
  vec3 subColour;
  float emissiveMask;
};

struct surface {
  material mtl;
  vec3 albedo;
  vec3 specular;
  mat3 geoTBN;
  vec3 emissive;
  vec3 subsurface;
  vec3 ao3;
  mat3x3 tbn;
};

surface makeSurface(material mtl, mat3x3 tbn, float nrmSign, vec3 vtxColour) {
  surface result;
  result.mtl = mtl;
  result.geoTBN = tbn;

  result.tbn[2] = normalize(tbn * mtl.normal);
  result.tbn[0] = normalize(cross(tbn[1], result.tbn[2]));
  result.tbn[1] = normalize(cross(result.tbn[2], result.tbn[0]));
  result.tbn[0] *= nrmSign;

  result.albedo     = mix(mtl.baseColour.xyz, vec3(0.0), mtl.metallic);
  result.albedo *= vtxColour.rgb;

  #ifdef TRANSPARENCY
  vec3 nonMetalSpec = vec3( mix(0.25, 0.05, mtl.baseColour.w) );
  #else
  vec3 nonMetalSpec = vec3(0.04);
  #endif
  result.specular   = mix(nonMetalSpec, mtl.baseColour.xyz, mtl.metallic);
  result.emissive   = mix(vec3(0.0), mtl.subColour.xyz, mtl.emissiveMask);
  result.subsurface = mix(mtl.subColour, vec3(0.0), mtl.emissiveMask);

  result.ao3 = vec3(1.0);

  return result;
}

//naughty dog tech art notes: procedural wetness for surfaces
float clampRange(float x, float l, float u) {
  return saturate((x-l) / (u-l));
}

void wetten(inout surface surf, float wetness) {
  wetness = saturate( wetness * (1 - surf.mtl.metallic) );
  if(wetness > 0) {
    vec3 clrSq = surf.albedo * surf.albedo;
    float porosity = surf.mtl.roughness * (1 - surf.mtl.metallic);
    surf.albedo = mix( surf.albedo, clrSq, clampRange(wetness, 0, 0.35) * porosity );
    surf.specular = mix( surf.specular, vec3(0.25), clampRange(wetness, 0.25, 0.5) * (1 - surf.mtl.metallic) );
    surf.mtl.roughness = mix(surf.mtl.roughness, 0.02, clampRange(wetness, 0.2, 1));
    surf.mtl.ao = mix(surf.mtl.ao, 1.0, clampRange(wetness, 0.45, 0.95));
    surf.ao3 = mix(surf.ao3, vec3(1.0), clampRange(wetness, 0.45, 0.95));
    surf.mtl.normal = mix(surf.mtl.normal, vec3(0,0,1), clampRange(wetness, 0.45, 0.95));
    surf.tbn[2] = mix(surf.tbn[2], surf.geoTBN[2], clampRange(wetness, 0.45, 0.95));
  }
}

struct materialData {
  uvec2 baseColour;
  uvec2 normalMap;
  uvec2 ARMH;
  uvec2 subsurfaceEmissive;
};



// Kaplanyan 2016, stable specular highlights
// Tokuyoshi 2017, error reduction and simplification for shading anti-aliasing
// Tokuyoshi and Kaplanyan 2019, improved geometric specular antialiasing
float perceptualToRoughness(float r) { return r*r; }
float roughnessToPerceptual(float r) { return sqrt(r); }
float normalFiltering(float perceptRoughness, const vec3 normal) {
  const float specularAAVariance = 0.25;
  const float specularAAThreshold = 0.18;
  vec3 du = dFdx(normal);
  vec3 dv = dFdy(normal);
  float variance = specularAAVariance * (dot(du,du) + dot(dv,dv));
  float roughness = perceptualToRoughness(perceptRoughness);
  float kernelRough = min(2.0 * variance, specularAAThreshold);
  float r = saturate(roughness*roughness + kernelRough);
  return roughnessToPerceptual(sqrt(r));
}

material makeMaterial(materialData mtl, const vec2 uv) {
  material result;
  result.baseColour = toLinear( texture(sampler2D(mtl.baseColour), uv) );
  vec4 tsn = texture(sampler2D(mtl.normalMap), uv);
  result.normal = tsn.xyz * 2 - 1;
  vec4 armh = texture(sampler2D(mtl.ARMH), uv);
  result.ao = armh.x;
  result.roughness = max(armh.y, 0.002025);
  result.metallic = armh.z;
  result.height = armh.w;
  vec4 subs = texture(sampler2D(mtl.subsurfaceEmissive), uv);
  result.subColour = subs.xyz;
  result.emissiveMask = saturate(1.0 - (subs.w*1.1));
  return result;
}

// lighting
struct lightAccum {
  vec3 diffuse;
  vec3 specular;
  vec3 emissive;
};

lightAccum makeLightAccum() {
  lightAccum result;
  result.diffuse = vec3(0);
  result.specular = vec3(0);
  result.emissive = vec3(0);
  return result;
}


vec3 surfBRDF( surface surf, float n_l, vec3 l, vec3 v, bool analytic ) {
#if 1
  return specBRDF(surf.mtl.roughness, surf.tbn[2], surf.specular, surf.ao3, n_l, l, v, analytic);
#else
  vec3 n = surf.tbn[2];
  vec3 h = normalize(l + v);

  float n_v = abs( dot(n, v) );
  float n_h = ( dot(n, h) );
  float v_h = ( dot(v, h) );

  float roughness = surf.mtl.roughness;
  float alpha_ibl = roughness * roughness;
  float alpha = analytic ? max(0.01, alpha_ibl) : alpha_ibl;
  float d = specD(n_h, alpha);
  float g = specG(analytic, n_v, n_l, roughness);
  vec3 f = specF(surf.specular, v_h, roughness);
  vec3 num = f * (d * g) * n_l;
  float denom = 4.0 * n_l * n_v + (1.0/32.0);
  vec3 ao = surf.ao3; //(analytic ? 1.0 : surf.mtl.ao)
  return max(vec3(0), num / denom) * ao;
#endif
}



void lightingEnv(inout lightAccum lit, surface surf, samplerCube env, sampler2D lut, vec3 v) {
  vec3 spec = vec3(0);
  vec3 diff = vec3(0);
  vec3 n = surf.tbn[2];
  const float envScale = 2.0;
  {
    vec3 h = n;
    vec3 l = 2.0 * dot(v, h) * h - v;

    float n_v = abs(dot(n,v));
    float v_h = saturate(dot(v,h));
    float roughness = surf.mtl.roughness;
    vec3 samp = sampleEnv(env, roughness, l); // radiance

    vec2 brdf_lut = textureLod( lut, vec2(roughness, n_v), 0).xy;

    // roughness-adjusted fresnel
    vec3 f_rough = max(vec3(1.0 - roughness), surf.specular) - surf.specular;
    vec3 k_spec = surf.specular + f_rough * schlick(v_h);
    vec3 f_singleScatter = saturate(k_spec * brdf_lut.x + brdf_lut.y); // FssEss

    #if 1
      // FdezAguera2019 - A multiple-scattering microfacet model for real-time image-based lighting
      vec3 samp_multiScatter = sampleEnv(env, 0.75, n) / pi; // irradiance

      float e_multiScatter = /*saturate*/(1.0 - (brdf_lut.x + brdf_lut.y)); // Ems

      vec3 f_avg = surf.specular + (1.0 - surf.specular) * 0.14959965; // pi/21
      vec3 f_multiScatter = f_singleScatter * f_avg / ( 1.0 - e_multiScatter * f_avg );

      // mutiple-scattering for metals
      vec3 multiScatter_metal =  f_multiScatter;
      // multiple-scattering for nonmetals
      vec3 e_diffSingleScatter = 1.0 - (f_singleScatter + f_multiScatter * e_multiScatter);
      vec3 k_diff = surf.albedo * e_diffSingleScatter;
      vec3 multiScatter_insulator = (f_multiScatter*e_multiScatter*k_diff);

      // blend based on metallicity
      vec3 multiScatter = mix(multiScatter_insulator, multiScatter_metal, surf.mtl.metallic);

      // combine
      vec3 s_single = f_singleScatter * samp;
      vec3 s_multi = multiScatter * samp_multiScatter;
      spec = s_multi + s_single;
    #else
      // single-scatter
      spec = f_singleScatter * samp;
    #endif
  }
  diff = sampleEnv(env, 1, n) * surf.albedo / pi * surf.ao3;

  lit.specular += spec * surf.ao3 * envScale;
  lit.diffuse += diff * envScale;
}

// Jiminez et al. 2016, Practical realtime strategies for accurate indirect occlusion
// convert SSAO visibility term to a coloured version based on surface albedo
vec3 aoMulti(const vec3 albedo, float ao) {
  vec3 a = 2.0404 * albedo - 0.3324;
  vec3 b = -4.7951 * albedo + 0.6417;
  vec3 c = 2.7552 * albedo + 0.6903;
  return max(vec3(ao), ((ao * a + b) * ao + c) * ao);
}

float quantise(float x) {
  const float steps = 5.0;
  return floor(x * steps) / steps;
}

// based on unreal's pbr model
void lighting(inout lightAccum lit, surface surf, vec3 l, vec3 v, vec3 lightClr) {
  vec3 n = surf.tbn[2];
  //vec3 h = normalize(l + v);
  float n_l_raw =  dot(n, l);
  float n_l = saturate( n_l_raw );
  vec3 specular = surfBRDF(surf, n_l, l, v, true);
  vec3 diffuse = surf.albedo * (n_l) / pi;
  lit.diffuse += diffuse * lightClr;
  lit.specular += specular * lightClr;

  // this is some made up nonsense! Mostly for moss and stuff...
#if 1 //def SUBSURFACE
  vec3 subs = surf.subsurface;
  vec3 smoothedN = normalize(surf.geoTBN[2] * 4.0 + surf.tbn[2]);
  float sub_fres = abs(dot(smoothedN,v));

  float n_l_sm = abs(dot(smoothedN, l));
  n_l_sm *= n_l_sm * sub_fres;
  vec3 h_ish  = l + v;
  h_ish /= length(h_ish) * 0.99 + 0.01;
  float n_h_sm = abs(dot(h_ish, l));
  n_h_sm *= n_h_sm * sub_fres;

  float fake_ss = saturate((n_h_sm + n_l_sm) * 0.5);// * 0.9 + 0.1;

  vec3 sub_glow = fake_ss * 0.5 * subs * lightClr * surf.albedo;
  lit.emissive += sub_glow * 0.25;
  lit.specular += sub_glow;


  /*
  vec3 fuzz = surf.subsurface;
  vec3 fuzzN = normalize(surf.geoTBN[2] * 4.0 + surf.tbn[2]);
  float fuzzAmnt = max( 0.0, (dot(l, fuzzN) + 0.7) / 1.7 ) * (0.5*surf.mtl.height+0.5);
  float fuzzFres = 1.0 - abs(dot(fuzzN, v));
  fuzzFres *= fuzzFres;
  float fuzzScatter = fuzzFres;
  lit.diffuse += (fuzzScatter * fuzzAmnt * 0.5) * (fuzz*lightClr);
  */
#endif
}

struct pointLightData {
  vec4 position_radiusInner;
  vec4 colour_radiusOuter;
};

struct spotLightData {
  vec4 position_dotInner;
  vec4 direction_dotOuter;
  vec4 colour_focus;
};

struct clusterData {
  uvec2 pointStartCount;
  uvec2 spotStartCount;
};

#define PCF_SHADOWS 1
#define ESM_SHADOWS 0

// per-view uniforms: move these to a big ol' buffer
layout(location = 1) uniform mat4 inverseView;

layout(location = 2) uniform mat4 sunlightViewProjection;
layout(location = 3) uniform mat4 sunlightInverseView;

#if PCF_SHADOWS
layout(location = 8, binding=1) uniform sampler2DShadow sunlightShadow;
#else
layout(location = 8, binding=1) uniform sampler2D sunlightShadow;
#endif

layout(location = 13, binding=0) uniform samplerCube cubeEnvironmentMap;
layout(location = 14, binding=2) uniform sampler2D splitSumLUT;
layout(location = 15) uniform int debugMode = 0;

#ifdef TRANSPARENCY
layout(location = 4, binding=3) uniform sampler2D sceneRefraction;
layout(location = 5, binding=4) uniform sampler2D sceneBackfaces;
#else
layout(location = 4, binding=3) uniform sampler2D ssao;
#endif//TRANSPARENCY

layout(location = 17) uniform vec2 zNearFar = vec2(0.1, 100);
float linearDepth(float ndcZ) {
  // maybe reversed-z should be a define, although it mostly works now
#if 1
  // reversed z
  float z = ndcZ;
  float near = zNearFar.x;
  float far = zNearFar.y;
  if (far <= 0) {
    return near / (near + z);
  }
  return (far*near) / (near + z * (far - near));
#else
  // conventional z
  float z = ndcZ * 2 - 1;
  float near = zNearFar.x;
  float far = zNearFar.y;
  return 2 * near * far / (far + near - z * (far - near));
#endif
}

// inputs from vertex stage
flat in ivec2 materialAndInstance;
in vec4 fragVertexColour;
in mat3 fragTBN;
in vec4 fragUVs;
in vec3 fragWorldPos;
in vec4 fragHPos;
in float fragNrmSign;

// material shader storage buffer
layout(std430, binding=0) readonly buffer materialBuffer {
  materialData materials[];
} allMaterials;

// unused light storage buffer
layout(std430, binding=1) readonly buffer pointLightBuffer {
  pointLightData lights[];
} allPointLights;

// outputs
layout(location = 0) out vec4 debugFragmentOut;

// debug
const vec3 sunLightColour = vec3(6.0, 5.5, 5.0);
const vec3 debugLightColour = vec3(0.0, 5.5, 0.0);
const vec3 debugLightDir = normalize(vec3(1.0, 1.0, 0.1));

void main() {
  materialData mtlData = allMaterials.materials[materialAndInstance.x];
  material mtl = makeMaterial(mtlData, fragUVs.xy);
  vec3 ndc = fragHPos.xyz / fragHPos.w;

// there's a depth pre-pass now, this shouldn't be needed
#if 0
#ifdef FOLIAGE
  if (mtl.baseColour.a < 0.5) {
    discard;
  }
#endif
#endif

  // lighting debug
  switch (debugMode) {
    case 1:
      mtl.baseColour = vec4(1);
      mtl.metallic = 0;
      break;
    case 2:
      mtl.baseColour = vec4(1);
      mtl.metallic = 1;
      break;
  }

  surface surf = makeSurface(mtl, fragTBN, fragNrmSign, fragVertexColour.rgb);

#ifndef TRANSPARENCY
  {
    // SSAO
    vec2 screenUV = ndc.xy * 0.5 + 0.5;
    float ao = textureLod(ssao, screenUV, 0).r;
    surf.mtl.ao *= ao;
  }
#endif

  surf.ao3 = aoMulti(surf.albedo, surf.mtl.ao);
  surf.mtl.roughness = normalFiltering(surf.mtl.roughness, surf.tbn[2]);

  vec3 cameraPos = vec3(inverseView[3].xyz);
  vec3 e = fragWorldPos - cameraPos;
  vec3 v = -normalize(e);

  // sunlight parameters
  vec3 l_dir = normalize(sunlightInverseView[2].xyz);
  vec3 l_colour = sunLightColour;

  vec3 shadowWorldPos = fragWorldPos.xyz + surf.tbn[2] * 0.01;
  vec4 shadowH = sunlightViewProjection * vec4(shadowWorldPos,1);
  vec3 shadowNDC = shadowH.xyz / shadowH.w;
  shadowNDC.xy = shadowNDC.xy * 0.5 + 0.5;

#if PCF_SHADOWS // pcf with shadow sampler (comparison sampling)
  //float sampleBias1 = 1.0 / 2048.0;
  //float sampleBias0 = 1.0 / 256.0;
  //float sampleBias = mix(sampleBias0, sampleBias1, saturate(abs(dot(l_dir, surf.geoTBN[2]))));
  #if 1 // cheap and nasty one-tap, debug
    float shadow = textureLod(sunlightShadow, shadowNDC, 0);
  #else
    float shadow = 0;
    for (int i = -1; i <= 1; ++i ) {
      for (int j = -1; j <= 1; ++j ) {
        vec3 off = vec3(i,j,0) / 2048.0;
        shadow += textureLod(sunlightShadow, shadowNDC + off, 0) / 9.0;
      }
    }
  #endif
#elif ESM_SHADOWS // esm shadows
  const float zExp = 70.0;
  float pixelZExp = exp(-zExp * shadowNDC.z);
  float shadowZ = textureLod(sunlightShadow, shadowNDC.xy, 0).x;
  float shadowZExp = exp(zExp * shadowZ);
  float shadow = saturate(shadowZExp * pixelZExp);
#else // no shadows
  float shadow = 1;
#endif


  lightAccum lit = makeLightAccum();

  // analytic light test
  lighting(lit, surf, l_dir, v, l_colour);
  lit.diffuse *= shadow;
  lit.specular *= shadow;

  // ibl test
  lightingEnv(lit, surf, cubeEnvironmentMap, splitSumLUT, v);

  float n_v =  saturate(abs(dot( surf.tbn[2], v )) );
  vec4 debugSplitSum = textureLod(splitSumLUT, vec2(surf.mtl.roughness, n_v), 0 );

  lit.specular += surf.emissive;

#ifdef TRANSPARENCY
  #define USE_BACKFACE
  float alpha = surf.mtl.baseColour.w;
  if (alpha < 1) {
    float fres = pow(1 - n_v, 3); // emphasising the fresnel-ish term
    float alphaF = saturate(mix(alpha, 1, fres));
    vec2 screenUV = ndc.xy * 0.5 + 0.5;

    vec3 cam_x = inverseView[0].xyz;
    vec3 cam_y = inverseView[1].xyz;
    vec2 distort = vec2(dot(cam_x, surf.tbn[2]), dot(cam_y, surf.tbn[2]));
    vec2 refractUV = screenUV + distort * -0.02;

    // grab transparent backface normal and depth
    #ifdef USE_BACKFACE
      vec4 backface = textureLod(sceneBackfaces, refractUV, 0);
      float thickness = max(0, backface.w - linearDepth(ndc.z));
      vec3 backfaceN = backface.xyz;

      surface inSurf = surf;
      inSurf.tbn[2] = -backfaceN;
      lightAccum inLit = makeLightAccum();
      lighting(inLit, inSurf, l_dir, v, l_colour * surf.mtl.baseColour.xyz);
      lightingEnv(inLit, inSurf, cubeEnvironmentMap, splitSumLUT, v);

      distort += vec2(dot(cam_x, backfaceN), dot(cam_y, backfaceN)) * -0.75;
    #endif

    float refractMip = mix(0, 5, surf.mtl.roughness);

    // TODO: replace with something not totally and obviously wrong
    vec2 refractUV_B = screenUV + distort * -0.0375;
    vec2 refractUV_G = screenUV + distort * -0.04;
    vec2 refractUV_R = screenUV + distort * -0.0425;

    vec4 refracted_B = textureLod(sceneRefraction, refractUV_B, refractMip);
    vec4 refracted_G = textureLod(sceneRefraction, refractUV_G, refractMip);
    vec4 refracted_R = textureLod(sceneRefraction, refractUV_R, refractMip);

    // some very approximate extinction
    #ifdef USE_BACKFACE
      float scaledThick = thickness / 0.2;
      float xt = saturate(1 / (1 + scaledThick*scaledThick));
      float inFres = pow(1 - saturate(abs(dot(backfaceN, v))), 5);
    #else
      float xt = 0;
      float inFres = 0;
    #endif

    vec3 rtint = mix(surf.mtl.baseColour.xyz, vec3(1), xt);
    vec3 refracted = vec3(refracted_R.x, refracted_G.y, refracted_B.z) * rtint * (1-inFres);
    lit.diffuse = mix(refracted.xyz, lit.diffuse, alphaF);
    #ifdef USE_BACKFACE
      lit.specular += inLit.specular * rtint;
      lit.diffuse += inLit.diffuse * rtint * inFres;
    #endif
  }
#endif//TRANSPARENCY


  vec3 lit_combined = lit.diffuse + lit.specular + lit.emissive;

#ifdef BACK_FACES
  debugFragmentOut = vec4(surf.tbn[2], linearDepth(ndc.z));
#else
  switch (debugMode) {
    case 3:
      debugFragmentOut = vec4( surf.tbn[2] * 0.5 + 0.5, 1 );
      break;
    case 4:
      debugFragmentOut = vec4( surf.geoTBN[2] * 0.5 + 0.5, 1 );
      break;
    case 5:
      debugFragmentOut = vec4( surf.tbn[0] * 0.5 + 0.5, 1 );
      break;
    case 6:
      debugFragmentOut = vec4( surf.tbn[1] * 0.5 + 0.5, 1 );
      break;
    case 7:
      debugFragmentOut = vec4( surf.mtl.baseColour );
      break;
    case 8:
      debugFragmentOut = vec4( vec3(surf.mtl.roughness), 1 );
      break;
    case 9:
      debugFragmentOut = vec4( lit.diffuse, 1 );
      break;
    case 10:
      debugFragmentOut = vec4( lit.specular, 1 );
      break;
    case 11:
      debugFragmentOut = vec4(surf.ao3, 1.0);
      break;
    case 12:
      debugFragmentOut = debugSplitSum;
      break;
    default:
      debugFragmentOut = vec4( lit_combined, 1 );
      break;
  }
#endif
}
