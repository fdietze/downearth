#version 120

attribute vec3 a_position;
attribute vec3 a_normal;
attribute vec2 a_texCoord;

uniform mat4 u_mvp;
uniform mat4 u_projection;
uniform mat4 u_modelview;

uniform vec3 u_position;
uniform float u_scale;

varying vec3 v_normal;
varying vec2 v_texCoord;

void main() {
//    gl_Position = gl_ModelViewProjectionMatrix * vec4(u_position + gl_Vertex.xyz * vec3(u_scale), 1);
//    vec4 normal = gl_ModelViewMatrix * vec4(a_normal, 0);

    gl_Position = u_mvp * vec4(u_position + a_position * vec3(u_scale), 1);

    v_normal = a_normal;
    v_texCoord = a_texCoord;
}

