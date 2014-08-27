#version 150 core
 
in vec2 position;
in vec3 inColor;
out vec4 passColor;
 
void main(void) {
  gl_Position = vec4(position,0.0,1.0);
  passColor = vec4(gl_Position);
}
