//#version 450

//in vec4 fragVertexColour;

layout(location = 0) out vec4 fragOut;

layout(location = 0) uniform ivec2 renderRes;

#define T_CIRCLE 0
#define T_BOX 1
// (3) circle: 2 pos, radius
// (4) box: 2 bl, 2 tr
//
struct shape {
  int tag;
};

void main() {
  vec2 screen = gl_FragCoord.xy;
  vec2 pixel = screen * vec2(renderRes);
  fragOut = vec4( 1, 0, 1, 0.125 );

}
