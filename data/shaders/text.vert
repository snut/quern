


// attributes
layout(location = 0) in vec2 vertexPosition;
layout(location = 1) in vec2 vertexUV;
layout(location = 2) in vec4 clrFore;
layout(location = 3) in vec4 clrBack;
layout(location = 4) in uvec4 sdfParam;

layout(location = 1) uniform vec2 screenResolution;

out vec2 fragUV;
flat out vec4 fgColour;
flat out vec4 bgColour;
flat out uvec4 sdfData;

void main() {
  vec2 spos = vertexPosition / screenResolution.xy;
  gl_Position = vec4( spos * 2.0 - 1.0, 0, 1 );
  fragUV = vertexUV;
  fgColour = clrFore;
  bgColour = clrBack;
  sdfData = sdfParam;
}
