#version 120
varying vec4 worldpos;

void main () {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    worldpos = gl_Position;
}
