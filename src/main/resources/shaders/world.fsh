in vec4 v_pos;
in vec4 v_norm_ws;
in vec4 v_texCoord;

uniform sampler2DArray image;

uniform vec4 ambient;
uniform vec4 sunColor;
uniform vec4 sunDir_ws;

out vec4 color;

void main() {
  color = texture(image, v_texCoord) * (sunColor * max(vec4(0), dot(sunDir_ws, v_norm_ws)) + ambient);
}


