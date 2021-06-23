//#version 450

layout(location = 0) in vec2 vertexPosition;
layout(location = 1) in vec2 quadPosition;

void main () {
  gl_Position = vec4( vertexPosition.xy, 0, 1 );
}
