#version 130

out vec2 vTexCoord;

in vec4 position;

void main()
{
    vec2 p = position.xy;
    vTexCoord   = p * vec2(0.5, 0.5) + vec2(0.5, 0.5);
    gl_Position = position;
}

