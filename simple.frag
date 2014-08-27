#version 150 core

in vec4 passColor;
out vec4 color;

void main() {
  if (passColor.x + passColor.y > 0) {
    color = passColor;
  } else {
    color = vec4(1,0,0,1);
  }
}
