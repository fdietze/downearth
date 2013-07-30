in vec3 position;
in vec2 normal_ws; // ws = WorldSpace
in vec3 texCoord;

uniform mat4 pvm;


out vec4 v_pos;
out vec4 v_norm_ws;
out vec4 v_texCoord;

void main() {
  v_pos  = pvm * vec4(position,1);
  float z = sqrt(normal.x*normal.x + normal.y*normal.y);
  v_norm = pvm * vec4(normal,z,0);
  v_texCoord = texCoord;
}


