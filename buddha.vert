#version 150 core
 
in vec2 position;
in vec2 texCoordIn;
//in vec3 inColor;
//out vec4 passColor;
out vec2 texCoord; 

void main(void) {
  gl_Position = vec4(position,0.0,1.0);
  texCoord = texCoordIn;
  //texCoord = (position + vec2(1.0,1.0)) * 0.5;
  
  //passColor = vec4(gl_Position);
}
