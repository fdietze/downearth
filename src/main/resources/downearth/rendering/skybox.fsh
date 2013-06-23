#version 120

uniform samplerCube cubemap;

varying vec3 v_tex_coord;

void main() {
	if( sin( (gl_FragCoord.x + gl_FragCoord.y) / 16 ) > 0)
    	gl_FragColor = textureCube(cubemap, v_tex_coord);
    else
    	gl_FragColor = vec4(1,0,0,0);
}


