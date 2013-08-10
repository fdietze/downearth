#version 130

in vec3 position;
in vec3 normal_ws;
in vec3 texCoord;

uniform mat4 pvm;

out vec3 v_norm_ws;
out vec3 v_texCoord;
out vec3 v_pos;

void main() {
  gl_Position = pvm * vec4(position,1);
  v_pos = position;
  v_norm_ws = clamp( normal_ws, vec3(-1), vec3(1) );
  v_texCoord = texCoord;//clamp( texCoord, vec3(-1), vec3(1) );
}


