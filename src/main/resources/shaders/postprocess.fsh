#version 140
out vec4 color;

uniform float a;
uniform float b;
uniform float c;
uniform float time;

uniform sampler2DRect image;

void main() {
	float offset = a * sin(b*time + c*gl_FragCoord.y);
	color = texture(image, vec2(gl_FragCoord.x + offset, gl_FragCoord.y) );
}


