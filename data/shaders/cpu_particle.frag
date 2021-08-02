
layout(location = 0) out vec4 fragOut;

in vec2 particleUv;

void main() {
  vec2 uv_ = (particleUv * 2 - 1);
  float r = clamp(1 - dot(uv_,uv_), 0, 1);
  fragOut = vec4(0.35, 0.43, 0.6, 0.75) * r * r * 4;

}
