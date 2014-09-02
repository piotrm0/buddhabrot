#version 150 core

in vec2 texCoord;
in vec4 passColor;
out vec4 color;

uniform sampler2D tex;

void main() {
  color = texture(tex, texCoord) * 5;
}
