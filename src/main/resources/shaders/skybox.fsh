#version 150

uniform samplerCube cubemap;

in vec3 texCoord;

out vec4 color;

void main() {
    color = texture(cubemap,texCoord);
}




