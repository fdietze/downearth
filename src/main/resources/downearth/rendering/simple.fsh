#version 150

// in vec3 v_pos;
uniform vec4 u_tint;

void main() {
//    if( sin( (v_pos.x + v_pos.y + v_pos.z) ) > 0)
//        gl_FragColor = vec4(0);
//    else
        gl_FragColor = u_tint;
}

