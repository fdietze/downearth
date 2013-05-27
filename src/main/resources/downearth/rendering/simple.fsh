#version 120

varying vec3 v_normal;
varying vec2 v_texCoord;

uniform vec3 lightDir;
uniform vec4 lightColor;
uniform vec4 ambientLight;

uniform sampler2D texture;

void main() {
    vec4 tex = texture2D(texture, v_texCoord);
    float light = dot( lightDir, v_normal );
    light = max(0, light);

    gl_FragColor = ( lightColor * vec4(light) + ambientLight ) * tex;
}
