
// https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
// all are centered on the origin and unrotated
float sd_circle(vec2 p, float r) { return length(p) - r; }
float sd_line(vec2 p, vec2 a, vec2 b) {
  vec2 pa = p - a;
  vec2 ba = b - a;
  float h = clamp( dot(pa, ba) / dot(ba, ba), 0, 1 );
  return length((ba * -h) + pa);
}
float sd_box(vec2 p, vec2 sz) {
  vec2 d = abs(p) - sz;
  return length(max(d, vec2(0))) + min(max(d.x, d.y), 0);
}
float sd_hexagon(vec2 p, float r) {
    const vec3 k = vec3(-0.866025404, 0.5, 0.577350269);
    p = abs(p);
    p -= 2.0*min(dot(k.xy, p), 0) * k.xy;
    p -= vec2(clamp(p.x, -k.z * r, k.z * r), r);
    return length(p) * sign(p.y);
}
float sd_ellipse(vec2 p, vec2 ab) {
  p = abs(p); if (p.x > p.y) { p = p.yx; ab = ab.yx; }
  float l = ab.y*ab.y - ab.x*ab.x;

  float m = ab.x * p.x / l;      float m2 = m * m;
  float n = ab.y * p.y / l;      float n2 = n * n;
  float c = (m2 + n2 - 1.0) / 3.0; float c3 = c * c * c;

  float q = c3 + m2 * n2 * 2.0;
  float d = c3 + m2 * n2;
  float g = m + m * n2;

  float co;
  if (d < 0.0) {
     float h = acos(q/c3)/3.0;
     float s = cos(h);
     float t = sin(h)*sqrt(3.0);
     float rx = sqrt( -c*(s + t + 2.0) + m2 );
     float ry = sqrt( -c*(s - t + 2.0) + m2 );
     co = (ry+sign(l)*rx+abs(g)/(rx*ry)- m)/2.0;
  } else {
     float h = 2.0*m*n*sqrt( d );
     float s = sign(q+h)*pow(abs(q+h), 1.0/3.0);
     float u = sign(q-h)*pow(abs(q-h), 1.0/3.0);
     float rx = -s - u - c*4.0 + 2.0*m2;
     float ry = (s - u)*sqrt(3.0);
     float rm = sqrt( rx*rx + ry*ry );
     co = (ry/sqrt(rm-rx)+2.0*g/rm-m)/2.0;
  }

  vec2 r = ab * vec2(co, sqrt(1.0-co*co));
  return length(r-p) * sign(p.y-r.y);
}

// modifiers
// to round, just subtract rounding radius
float sd_round(float sd, float r) { return sd - r; }
float sd_annular(float sd, float r) { return abs(sd) - r; }

// smooth booleans
float smooth_union(float sd1, float sd2, float k) {
  float h = clamp(0.5 + 0.5*(sd2-sd1)/k, 0, 1);
  return mix(sd2, sd1, h) - k*h*(1-h);
}
float smooth_difference(float sd1, float sd2, float k) {
  float h = clamp(0.5 - 0.5*(sd2+sd1)/k, 0, 1);
  return mix(sd2, -sd1, h) - k*h*(1-h);
}
float smooth_intersect(float sd1, float sd2, float k) {
  float h = clamp(0.5 - 0.5*(sd2-sd1)/k, 0, 1);
  return mix(sd2, sd1, h) + k*h*(1.0-h);
}

vec2 rotate(vec2 p, float a) {
  float ca = cos(a);
  float sa = sin(a);
  vec2 pr = vec2(p.x * ca - p.y * sa, p.x * sa + p.y * ca);
  return pr;
}
