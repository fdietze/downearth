#version 150

in vec4 a_pos;

uniform mat4 matrix;

void main() {
    gl_Position = matrix * a_pos;
}
