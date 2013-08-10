#version 130

in vec3 v_norm_ws;
in vec3 v_texCoord;
in vec3 v_pos;

uniform sampler2DArray image;

uniform vec3 ambient;
uniform vec3 sunColor;
uniform vec3 sunDir_ws;

out vec4 oColor;

void main() {
  vec3 texCol = texture(image, v_texCoord).xyz;
  vec3 sunLight = sunColor * vec3(max(0, -dot(sunDir_ws, v_norm_ws)));
  vec3 light = sunLight + ambient;
  //oColor = vec4( texCol * light,1);
  //oColor = vec4( texCol, 1);
  //oColor = vec4( v_texCoord, 1);
  //oColor = vec4(v_pos,1);
  //oColor = vec4( v_norm_ws, 1);
  oColor = vec4( light*texCol, 1 );
  //vec3 test = sin(v_pos);
  //test = test * test;
  //oColor = vec4(test,1);
  //oColor = vec4(1,0,0,1);
}




