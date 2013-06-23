#version 120

varying vec3 v_normal;
varying vec2 v_texCoord;

uniform vec3 lightDir;
uniform vec4 lightColor;
uniform vec4 ambientLight;
uniform vec4 tint;

uniform sampler2D texture;

void main() {
    vec4 tex = texture2D(texture, v_texCoord);
    float light = dot( lightDir, v_normal );
    // float light = dot( lightDir, vec3(0,0,1) );
    light = max(0, light);

    if( sin( (gl_FragCoord.x + gl_FragCoord.y) / 16 ) > 0)
        // gl_FragColor = vec4(light) * tex;
        gl_FragColor = ( lightColor * vec4(light) + ambientLight ) * tex * tint;
        // gl_FragColor = tex;
    else
        gl_FragColor = vec4(1,1,0,0);
}
