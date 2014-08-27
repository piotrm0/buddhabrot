#version 150 core

// adapted from http://nuclear.mutantstargoat.com/articles/sdr_fract/

in vec4 passColor;
out vec4 color;

void main() {
  int iter = 50;
  vec2 z, c;
  float scale = 1.0;
  vec2 center = vec2(0,0);

  c.x = 1.3333 * passColor.x * scale - center.x;
  c.y = passColor.y * scale - center.y;

  int i;
  z = c;
  for(i=0; i<iter; i++) {
    float x = (z.x * z.x - z.y * z.y) + c.x;
    float y = (z.y * z.x + z.x * z.y) + c.y;

    if((x * x + y * y) > 4.0) break;
    z.x = x;
    z.y = y;
  }

  float temp = (i == iter ? 0.0 : float(i) / 50.0);
  color = vec4(temp, 0,0,1);
}
