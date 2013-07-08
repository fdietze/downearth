#version 130

in vec4 a_pos;
in vec4 offset;

void main(){
    gl_Position = a_pos + offset;
}
