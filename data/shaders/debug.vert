//#version 450

layout(location = 0) in vec4 vertexPosition;
layout(location = 1) in vec4 vertexColour;

layout(location = 0) uniform mat4 viewProjection;

out vec4 fragVertexColour;

void main () {
  gl_Position = viewProjection * vertexPosition;
  fragVertexColour = vertexColour;
}
