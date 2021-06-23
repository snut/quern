//#version 450
#extension GL_ARB_shader_draw_parameters : enable

// attributes
layout(location = 0) in vec3 vertexPosition;

#ifdef FOLIAGE
layout(location = 4) in vec2 vertexUV0;
layout(location = 5) in vec2 vertexUV1;
#endif

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
out vec3 fragWorldPos;
out vec4 fragHPos;

#ifdef FOLIAGE
flat out ivec2 materialAndInstance;
out vec4 fragUVs;

#include <wind.glsl>
#endif

// whee
void main() {
  mat4 world = instances.matrixAndTint[unpackMaterialAndInstance.y];
  world[3] = vec4(0,0,0,1);
  world = transpose(world);
  vec4 pos = vec4(vertexPosition.xyz, 1);
  vec4 wpos = world * pos;

#ifdef FOLIAGE
  wpos.xyz += wind(wpos.xyz, time.x);

  materialAndInstance = unpackMaterialAndInstance;
  fragUVs = vec4(vertexUV0, vertexUV1);
#endif // FOLIAGE

  vec4 hpos = viewProjection * wpos;
  gl_Position = hpos;
  fragHPos = hpos;
  fragWorldPos = wpos.xyz;

}
