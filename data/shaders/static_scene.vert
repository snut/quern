//#version 450
#extension GL_ARB_shader_draw_parameters : enable

// attributes
layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec4 vertexColour;
layout(location = 2) in vec4 vertexTangent;
layout(location = 3) in vec4 vertexBitangent;
layout(location = 4) in vec2 vertexUV0;
layout(location = 5) in vec2 vertexUV1;

// uniforms
layout(location = 0) uniform mat4 viewProjection;
layout(location = 16) uniform vec4 time;

// storage buffers
layout(std430, binding=1) readonly buffer instanceBuffer {
  mat4 matrixAndTint[];
} instances;

// magic
ivec2 unpackMaterialAndInstance = ivec2( gl_BaseInstanceARB >> 16, gl_BaseInstanceARB & 0xffff );

// outputs
flat out ivec2 materialAndInstance;
out vec4 fragVertexColour;
out mat3 fragTBN;
out vec4 fragUVs;
out vec3 fragWorldPos;
out vec4 fragHPos;
out float fragNrmSign;

#ifdef FOLIAGE
#include <wind.glsl>
#endif

// whee
void main() {
  materialAndInstance = unpackMaterialAndInstance;
  mat4 world = instances.matrixAndTint[unpackMaterialAndInstance.y];
  vec4 tint = world[3];
  world[3] = vec4(0,0,0,1);
  world = transpose(world);
  vec4 pos = vec4(vertexPosition.xyz, 1);
  vec4 wpos = world * pos;

#ifdef FOLIAGE
  wpos.xyz += wind(wpos.xyz, time.x);
#endif
  fragHPos = viewProjection * wpos;
  gl_Position = fragHPos;
  fragVertexColour = vertexColour * tint;
  fragUVs = vec4(vertexUV0, vertexUV1);
  fragWorldPos = wpos.xyz;

  fragTBN[0] = normalize((world * vec4(vertexTangent.xyz,0)).xyz);
  fragTBN[1] = normalize((world * vec4(vertexBitangent.xyz,0)).xyz);
  fragTBN[2] = normalize(cross(fragTBN[0], fragTBN[1])) * (vertexTangent.w < 0 ? -1 : 1);
  fragNrmSign = vertexTangent.w < 0 ? -1 : 1;
}
