#version 130

uniform mat4 matrix;

in vec4 position;

out vec3 texCoord;

void main() {
    gl_Position = matrix*position;
    texCoord = position.xyz;
}

