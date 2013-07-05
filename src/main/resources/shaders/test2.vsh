#version 150

in vec4 a_pos;
in vec4 offset;
in float scale;

uniform mat4 matrix;

void main() {
    gl_Position = matrix * (offset + a_pos * vec4(vec3(scale),1));
}
