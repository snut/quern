
vec4 unpack_rgba8(uint p) {
  vec4 rgba;
  rgba.x = float((p>> 0)&0xff) / 255;
  rgba.y = float((p>> 8)&0xff) / 255;
  rgba.z = float((p>>16)&0xff) / 255;
  rgba.w = float((p>>24)&0xff) / 255;
  return rgba;
}

float saturate(float x) { return clamp(x, 0.0, 1.0); }
vec3 saturate(vec3 x) { return clamp(x, vec3(0), vec3(1)); }
const float pi = 3.14159;


// extremely accurate sRGB <-> linear conversion functions ahead
vec3 toLinear(vec3 c) {
  vec3 result = c;
  result.xyz *= result.xyz;
  return result;
}
vec4 toLinear(vec4 c) { return vec4( toLinear(c.xyz), c.w ); }

vec3 fromLinear(vec3 c) {
  vec3 result = c;
  result.xyz = sqrt(result.xyz);
  return result;
}
vec4 fromLinear(vec4 c) { return vec4( fromLinear(c.xyz), c.w ); }
// end extermely accurate not-at-all a hack colour space conversion


vec3 sampleEnv(samplerCube env, float roughness, vec3 dir) {
  /*
    Filament (google) roughness mapping:
      log2(roughness) + roughnessOneLOD
    Approximation:
      roughnessOneLOD * roughness * (2 - roughness)
      roughnessOneLOD = 4 (16x16 with a 256 map)
  */
  float smoothness = saturate(1 - roughness);
  smoothness = mix( smoothness, smoothness * smoothness, 0.5);
  float r = 1 - smoothness;
  float maxMip = 8;
  float mip = saturate(r) * maxMip;
  return textureLod( env, dir, mip ).xyz;
}


float schlick(float v) {
#if 0
  v = 1.0 - v;
  float v2 = v * v;
  return v2 * v2 * v;
#elif 0
  return pow( 1.0 - v, 5 );
#else // unreal approximation using a spherical gaussian expansion
  return pow(2, (-5.55473 * v - 6.98316) * v);
#endif
}


// specular model based on Unreal publication (2013)
// https://cdn2.unrealengine.com/Resources/files/2013SiggraphPresentationsNotes-26915738.pdf

// GGX/Trowbridge-Reitz
// alpha = roughness squared
float specD(float n_h, float alpha) {
  float a2 = alpha * alpha;
  float den = n_h * n_h * (a2 - 1) + 1;
  return a2 / (pi * den * den);
}

float specG1(float n_v, float k) {
  return n_v / (n_v * (1.0 - k) + k);
}

float specG(bool analytic, float n_v, float n_l, float roughness) {
  float dehot = roughness + 1.0;
  float k_anl = (dehot * dehot) * 0.25;
  float k_ibl = (roughness * roughness);
  float k = analytic ?  k_anl : k_ibl;
  return specG1(n_l, k) * specG1(n_v, k);
}


vec3 specF(vec3 f0, float v_h, float roughness) {
  vec3 fr = max(vec3(1.0 - roughness), f0) - f0;
  return f0 + fr * schlick(v_h);
}

// cook-torrence-ish
// https://en.wikipedia.org/wiki/Cook-Torrance#Cook%E2%80%93Torrance_model
/*
  k_spec = (D*F*G) / (4 * (V.N) * (N.L))
  F = Fresnel term (Schlick's approximation)
  D = distribution factor
  G = geometric attenuation term
*/
vec3 specBRDF(float roughness, vec3 n, vec3 specular, vec3 ao, float n_l, vec3 l, vec3 v, bool analytic) {
  vec3 h = normalize(l + v);

  float n_v = abs( dot(n, v) );
  float n_h = ( dot(n, h) );
  float v_h = ( dot(v, h) );

  float alpha_ibl = roughness * roughness;
  float alpha = analytic ? max(0.01, alpha_ibl) : alpha_ibl;
  float d = specD(n_h, alpha);
  float g = specG(analytic, n_v, n_l, roughness);
  vec3 f = specF(specular, v_h, roughness);
  vec3 num = f * (d * g) * n_l;
  float denom = 4.0 * n_l * n_v + (1.0/32.0);
  return max(vec3(0), num / denom) * ao;
}
