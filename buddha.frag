#version 150 core

in vec2 texCoord;
//in vec4 passColor;
out vec4 color;
uniform float scale0;
uniform float scale1;
uniform float scale2;
uniform float scale3;

uniform sampler2D tex;

void main() {
  color = vec4(0.0, 0.0, 0.0, 256.0);
  vec4 tex = texture(tex, texCoord);
  color[0] = pow (tex[0] / scale0, 0.75);
  color[1] = pow (tex[1] / scale1, 0.75);
  color[2] = pow (tex[2] / scale2, 0.75);

  //color[3] = scale3;
}
