
layout(location = 0) in vec3 vertexPosition;

struct instanceData {
  vec4 position_age;
  vec4 size_fade;
  vec4 rot;
  vec4 seed_colour_pad1_pad2;
};

layout(std430, binding=1) restrict readonly buffer instances_ {
  instanceData pcls[];
} instances;

// uniforms
layout(location = 0) uniform mat4 viewProjection;
layout(location = 1) uniform mat3 camBasis;

// outputs
out vec2 particleUv;
out vec3 worldPos;
out vec3 pclTan;
out vec3 pclBitan;
out vec3 pclNrm;
out float erodeFactor;
out vec3 pclClr;

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
  vec3 size = pcl.size_fade.xyz * 4;

  vec4 rot = pcl.rot.yzwx;
  vec3 v = (vertexPosition + vec3(0,0,0.1)) * size * vec3(1,1,-1);
  v = qrot(rot, v);
  v = v * camBasis;
  vec3 pos = (v + pcl.position_age.xyz);
  worldPos = pos;
  gl_Position = viewProjection * vec4(worldPos, 1);

  int seed = floatBitsToInt(pcl.seed_colour_pad1_pad2.x);

  vec2 uv = vertexPosition.xy * 0.5 + 0.5;
  int subX = seed % 4;
  int subY = (seed / 4) % 4;

  particleUv = uv * 0.25 + vec2(float(subX)*0.25, float(subY)*0.25);
  pclTan = qrot(rot, vec3(1,0,0)) * camBasis;
  pclBitan = qrot(rot, vec3(0,1,0)) * camBasis;
  pclNrm = normalize(v);
  vec3 t = normalize(cross(pclBitan, pclNrm));
  vec3 b = normalize(cross(pclNrm, pclTan));
  pclBitan = b;
  pclTan = t;


  vec3 pink = vec3(0.95,0.3,0.4);
  vec3 blue = vec3(0.3,0.4,0.96);
  vec3 white = vec3(0.7,0.7,0.7);
  switch(seed%3) {
    case 0: pclClr = pink; break;
    case 1: pclClr = blue; break;
    default: pclClr = white;
  }

  erodeFactor = 1.0 - smoothstep(0.0, 1.0, clamp(ttl, 0.0, 1.0));
}
