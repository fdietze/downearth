// attribute vec4 a_pos;

attribute vec4 a_pos;

void main(){
    // gl_Position = gl_Vertex;
    gl_Position = a_pos;
}