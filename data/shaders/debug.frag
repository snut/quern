//#version 450

in vec4 fragVertexColour;

layout(location = 0) out vec4 debugFragmentOut;

void main() {
  debugFragmentOut = fragVertexColour;
  debugFragmentOut.xyz *= fragVertexColour.w;
}
