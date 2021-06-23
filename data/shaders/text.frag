#include "utility.glsl"
#include "sdf.glsl"

layout(location = 0, binding=0) uniform sampler2D textPage;

layout(location = 1) uniform vec2 screenResolution;

in vec2 fragUV;
flat in vec4 fgColour;
flat in vec4 bgColour;
flat in uvec4 sdfData;


layout(location = 0) out vec4 fragOut;

ivec2 unpackShort2(uint ui) {
  const uint msk = 0xffff;
  uint ux = (ui & 0x0000ffff);
  uint uy = (ui & 0xffff0000) >> 16;

  const int maxShort = 32768;
  int ix = ux >= maxShort ? int(ux | 0xffff0000) : int(ux);
  int iy = uy >= maxShort ? int(uy | 0xffff0000) : int(uy);
  return ivec2(ix, iy);
}

void main() {
  ivec2 pageSize = textureSize(textPage, 0);
  vec2 delta = vec2(1.0) / vec2(pageSize);
  vec2 shadow = delta * vec2(-1.0, -2.0);

  // main text
  vec4 t = texture2D(textPage, fragUV);
  // drop shadow
  vec4 s = texture2D(textPage, fragUV + shadow);

  vec4 fg = fgColour;
  vec4 bg = bgColour;
  bg.a = s.r;

  vec4 txt = mix( bg, fg, t.x );

  fragOut = txt;

  // drawing rounded shapes
  if (sdfData.x != 0 && sdfData.y != 0) {
    vec2 pixelPos = gl_FragCoord.xy;// * screenResolution;

    vec2 boxBL = vec2(unpackShort2(sdfData.x));
    vec2 boxTR = vec2(unpackShort2(sdfData.y));

    vec2 boxSize = abs(boxTR - boxBL) * 0.5;
    vec2 boxPos  = (boxTR+boxBL) * 0.5;
    vec2 boxRoundSoft = vec2
      ( float( sdfData.z&0x00ff )
      , float((sdfData.z&0xff00)>>8)
      );
    // half of z and all of w left for more params

    float sdf = sd_round
      ( sd_box(pixelPos - boxPos, boxSize - boxRoundSoft.x)
      , boxRoundSoft.x);

    float glow = 1.0 - clamp(sdf/max(boxRoundSoft.y,0.01), 0.0, 1.0);
    vec4 sdf_bg = boxRoundSoft.y == 0
      ? vec4(0.0)
      : mix(bgColour, vec4(bgColour.rgb, 0.0), 1.0-glow*glow);

    vec4 sdf_fg = mix(fgColour, sdf_bg, clamp(sdf, 0.0, 1.0));
    fragOut = sdf_fg;
  }


}
