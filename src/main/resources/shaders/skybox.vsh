#version 150

in vec4 position;

uniform mat4 matrix;

out vec4 v_tex_coord;

void main() {
    gl_Position = matrix*position;
    v_tex_coord = position;
}

