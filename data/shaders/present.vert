
layout(location = 4) in vec2 vertexUV0;

out vec2 uv;

void main() {
  uv = vertexUV0;
  gl_Position = vec4( vertexUV0.xy * 2 - 1, 0.0, 1 );
}
