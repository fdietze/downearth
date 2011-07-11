#version 120
varying vec4 worldpos;
varying vec3 normal;
varying vec3 vertex;



void main () {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
	vertex = vec3(gl_ModelViewMatrix * gl_Vertex);       
	normal = normalize(gl_NormalMatrix * gl_Normal);
    worldpos = gl_Vertex;
}
