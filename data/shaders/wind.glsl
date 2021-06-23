

vec3 wind(vec3 wpos, float time) {
  vec3 windDeflect = vec3(0.2, 0.4, 0.05);
  float wndWave = sin(time * 1.5 + dot(wpos.xy, vec2(0.3,0.21)));
  float wndAmnt = (0.4 + 0.5 * wndWave) * min(2, max(0, wpos.z*wpos.z));
  return wndAmnt * windDeflect;
}
