
vec4 unpack_rgba8(uint p) {
  vec4 rgba;
  rgba.x = float((p>> 0)&0xff) / 255;
  rgba.y = float((p>> 8)&0xff) / 255;
  rgba.z = float((p>>16)&0xff) / 255;
  rgba.w = float((p>>24)&0xff) / 255;
  return rgba;
}
