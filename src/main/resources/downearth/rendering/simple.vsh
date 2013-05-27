#version 120

attribute vec3 a_position;
attribute vec3 a_normal;
attribute vec2 a_texCoord;

uniform mat4 u_mvp;
uniform mat4 u_projection;
uniform mat4 u_modelview;

varying vec3 v_normal;
varying vec2 v_texCoord;

void main() {
    gl_Position = u_mvp * vec4(a_position, 1);
    vec4 normal = u_modelview * vec4(a_normal, 0);
    v_normal = normal.xyz;
    v_texCoord = a_texCoord;
}

