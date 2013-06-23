#version 120

attribute vec3 a_position;

uniform mat4 u_mvp;

varying vec3 v_tex_coord;

void main() {
    gl_Position = u_mvp * vec4(a_position,1);
    v_tex_coord = a_position;
}

