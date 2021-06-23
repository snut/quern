//#version 450
#extension GL_ARB_bindless_texture : enable

float saturate(float x) { return clamp(x, 0.0, 1.0); }
//const float pi = 3.14159;

// uniforms
//layout(location = 1) uniform mat4 inverseView;

// inputs from vertex stage
in vec3 fragWorldPos;
in vec4 fragHPos;

#ifdef FOLIAGE
flat in ivec2 materialAndInstance;
in vec4 fragUVs;

struct materialData {
  uvec2 baseColour;
  uvec2 normalMap;
  uvec2 ARMH;
  uvec2 subsurfaceEmissive;
};

// material shader storage buffer
layout(std430, binding=0) readonly buffer materialBuffer {
  materialData materials[];
} allMaterials;
#endif//FOLIAGE

// outputs
//layout(location = 0) out vec4 shadowFragmentOut;

void main() {
#ifdef FOLIAGE
  materialData mtl = allMaterials.materials[materialAndInstance.x];
  float alpha = texture(sampler2D(mtl.baseColour), fragUVs.xy).w;
  if (alpha < 0.5) {
    discard;
  }
#endif
}
