
layout(location = 0) in vec3 vertexPosition;

struct instanceData {
  vec4 position_age;
  vec4 size_fade;
  vec4 rot;
};

layout(std430, binding=1) restrict readonly buffer instances_ {
  instanceData pcls[];
} instances;

// uniforms
layout(location = 0) uniform mat4 viewProjection;
layout(location = 1) uniform mat3 camBasis;

// outputs
out vec2 particleUv;

vec4 qmul(vec4 a, vec4 b) {
  vec4 r;
  r.w = a.w*b.w - dot(a.xyz, b.xyz);
  r.xyz = cross(a.xyz,b.xyz) + a.w*b.xyz + b.w*a.xyz;
  return r;
}
vec4 qconj(vec4 q) {
  return q * vec4(-1,-1,-1,1);
}
vec3 qrot(vec4 q, vec3 v) {
  vec4 v_ = vec4(v, 0);
  return qmul(qmul(q,v_),qconj(q)).xyz;
}

void main () {
  instanceData pcl = instances.pcls[gl_InstanceID];
  float ttl = pcl.position_age.w;
  vec3 size = pcl.size_fade.xyz * smoothstep(0, 1, ttl);
  vec3 v = vertexPosition * size * vec3(1,1,-1);
  v = qrot(pcl.rot.yzwx, v);
  v = v * camBasis;
  gl_Position = viewProjection * vec4((v + pcl.position_age.xyz), 1);
  particleUv = vertexPosition.xy * 0.5 + 0.5;
}
