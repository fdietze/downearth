#version 150

uniform samplerCube cubemap;

in vec4 v_tex_coord;

out vec4 color;

void main() {
	if( sin( (gl_FragCoord.x + gl_FragCoord.y) / 16 ) > 0)
    	color = texture(cubemap, v_tex_coord.xyz);
    else
    	color = vec4(1,0,0,0);
}


